#include "erl_nif.h"

#include "crypt.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv,
          ERL_NIF_TERM load_info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}

ERL_NIF_TERM 
pack(cryptState_t *cs, ErlNifEnv* env)
{
  ERL_NIF_TERM local = enif_make_tuple4(env, 
					enif_make_uint(env,cs->uiGood),
					enif_make_uint(env,cs->uiLate),
					enif_make_uint(env,cs->uiLost),
					enif_make_uint(env,cs->uiResync));
  
  ERL_NIF_TERM remote = enif_make_tuple4(env, 
					 enif_make_uint(env,cs->uiRemoteGood),
					 enif_make_uint(env,cs->uiRemoteLate),
					 enif_make_uint(env,cs->uiRemoteLost),
					 enif_make_uint(env,cs->uiRemoteResync));
  
  ERL_NIF_TERM history;
  memcpy(enif_make_new_binary(env, AES_BLOCK_SIZE, &history), cs->decrypt_history, 4);

  ERL_NIF_TERM encrypt_iv;
  memcpy(enif_make_new_binary(env, AES_BLOCK_SIZE, &encrypt_iv), cs->encrypt_iv, AES_BLOCK_SIZE);

  ERL_NIF_TERM decrypt_iv;
  memcpy(enif_make_new_binary(env, AES_BLOCK_SIZE, &decrypt_iv), cs->decrypt_iv, AES_BLOCK_SIZE);

  ERL_NIF_TERM raw_key;
  memcpy(enif_make_new_binary(env, AES_BLOCK_SIZE, &raw_key), cs->raw_key, AES_BLOCK_SIZE);

  return enif_make_tuple(env, 6,
			 raw_key, 
			 decrypt_iv, 
			 encrypt_iv, 
			 history, 
			 local, 
			 remote);
}

int
unpack(cryptState_t *cs, const ERL_NIF_TERM** tuple, ErlNifEnv* env, const ERL_NIF_TERM argv[])
{
  ErlNifBinary p_key, p_div, p_eiv, p_history;

  const ERL_NIF_TERM** local_tuple;
  const ERL_NIF_TERM** remote_tuple;
  int ar;
  
  long bin_size = 0;
  
  if(!enif_inspect_binary(env, tuple[0], &p_key) || p_key.size != AES_BLOCK_SIZE)
    return FALSE;
  
  if(!enif_inspect_binary(env, tuple[1], &p_div) || p_div.size != AES_BLOCK_SIZE)
    return FALSE;
  
  if(!enif_inspect_binary(env, tuple[2], &p_eiv) || p_eiv.size != AES_BLOCK_SIZE)
    return FALSE;

  if(!enif_inspect_binary(env, tuple[3], &p_history))
    return FALSE;

  memcpy(cs->decrypt_history, p_history.data, p_history.size);
    
  CryptState_setKey (cs, p_key.data, p_eiv.data, p_div.data );

  if(!enif_get_tuple(env, tuple[4], &ar, &local_tuple) || ar != 4)
    return FALSE;

  if(!enif_get_uint(env, local_tuple[0], &(cs->uiGood)))
    return FALSE;
  
  if(!enif_get_uint(env, local_tuple[1], &(cs->uiLate)))
     return FALSE;

  if(!enif_get_uint(env, local_tuple[2], &(cs->uiLost)))
     return FALSE;

  if(!enif_get_uint(env, local_tuple[3], &(cs->uiResync)))
     return FALSE;

  if(!enif_get_tuple(env, tuple[5], &ar, &remote_tuple) || ar != 4)
    return FALSE;

  if(!enif_get_uint(env, local_tuple[0], &(cs->uiRemoteGood)))
    return FALSE;
  
  if(!enif_get_uint(env, local_tuple[1], &(cs->uiRemoteLate)))
     return FALSE;

  if(!enif_get_uint(env, local_tuple[2], &(cs->uiRemoteLost)))
     return FALSE;

  if(!enif_get_uint(env, local_tuple[3], &(cs->uiRemoteResync)))
     return FALSE;

  return TRUE;
}

static ERL_NIF_TERM 
encrypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  cryptState_t cs;
  ErlNifBinary p_source;
  const ERL_NIF_TERM** tuple;
  int ar;

  if(!enif_get_tuple(env, argv[0], &ar, &tuple) || ar != 6 ||
     !enif_inspect_binary(env, argv[1], &p_source))
    return enif_make_badarg(env);
  
  if(!unpack(&cs, tuple, env, argv))
    return enif_make_badarg(env);


  ERL_NIF_TERM encrypted;
  CryptState_encrypt ( &cs, 
		       p_source.data, 
		       enif_make_new_binary(env, 
					    p_source.size + 4, 
					    &encrypted), 
		       p_source.size );
  
  return enif_make_tuple2(env, encrypted, pack(&cs, env));
}

static ERL_NIF_TERM 
decrypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  cryptState_t cs;
  ErlNifBinary p_source;
  const ERL_NIF_TERM** tuple;
  int ar;

  if(!enif_get_tuple(env, argv[0], &ar, &tuple) || ar != 6 ||
     !enif_inspect_binary(env, argv[1], &p_source))
    return enif_make_badarg(env);
  
  if(!unpack(&cs, tuple, env, argv))
    return enif_make_badarg(env);


  ERL_NIF_TERM decrypted;
  if(CryptState_decrypt ( &cs, 
			  p_source.data, 
			  enif_make_new_binary(env, 
					       p_source.size - 4, 
					       &decrypted), 
			  p_source.size ) != TRUE)
    {
      return enif_make_atom(env, "error");
    }
 
  return enif_make_tuple2(env, decrypted, pack(&cs, env));
}

static ErlNifFunc nif_funcs[] = {
    {"encrypt", 2, encrypt_nif},
    {"decrypt", 2, decrypt_nif}
};

ERL_NIF_INIT(ocb128crypt, nif_funcs, load, reload, upgrade, unload)

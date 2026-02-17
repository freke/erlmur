-ifndef(ERLMUR_HRL).
-define(ERLMUR_HRL, true).

-record(user, {
    session_id :: pos_integer(),
    pid :: pid(),
    username :: binary(),
    channel_id = 0 :: non_neg_integer(),
    udp_addr :: {inet:ip_address(), inet:port_number()} | undefined
}).

-record(channel, {
    id :: non_neg_integer(),
    parent_id :: non_neg_integer() | undefined,
    name :: binary(),
    description :: binary() | undefined,
    position :: integer(),
    max_users :: non_neg_integer() | undefined,
    temporary :: boolean(),
    links = [] :: [non_neg_integer()]
}).

-endif.

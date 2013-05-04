-module (testlyet).
-compile ({ parse_transform, lyet }).
-compile (export_all).

-ifdef (HAVE_EUNIT).
-include_lib ("eunit/include/eunit.hrl").
-endif.

g (X) -> X + 1.
h (X) -> X + 2.
l (X) -> X + 3.
m (X) -> X + 4.

% different equivalent ways of writing m (l (h (g (X))))
% i like the first one the best

% per Ulf's suggestion, use a local call (let_)

testmegaone (X) ->
  let_ (X = g (X),
        X = h (X),
        X = l (X),
    m (X)).


testmegatwo (X) ->
  let_ (X = g (X), 
    let_ (X = h (X), 
      let_ (X = l (X), 
        m (X)
      )
    )
  ).

testmegathree (X) ->
  let_ (X = 
    let_ (X = 
      let_ (X = 
        let_ (X = g (X), 
              X), 
        h (X)), 
    l (X)), 
  m (X)).

testmeganoassign () ->
  let_ (9 + 2).

testmegastructured (X) ->
  let_ ({ _, X } = { dude, g (X) },
        { X, _ } = { h (X), wheres },
        { _, X } = { my, l (X) },
    { car, m (X) }).

testone (X) ->
  lyet:lyet (X = g (X),
             X = h (X),
             X = l (X),
    m (X)).


testtwo (X) ->
  lyet:lyet (X = g (X), 
    lyet:lyet (X = h (X), 
      lyet:lyet (X = l (X), 
        m (X)
      )
    )
  ).

testthree (X) ->
  lyet:lyet (X = 
    lyet:lyet (X = 
      lyet:lyet (X = 
        lyet:lyet (X = g (X), 
                   X), 
        h (X)), 
    l (X)), 
  m (X)).

testnoassign () ->
  lyet:lyet (9 + 2).

-ifdef (EUNIT).

one_mega_test () -> 
  ?assertEqual (11, testmegaone (1)).

two_mega_test () -> 
  ?assertEqual (11, testmegatwo (1)).

three_mega_test () -> 
  ?assertEqual (11, testmegathree (1)).

noassign_mega_test () -> 
  ?assertEqual (11, testmeganoassign ()).

structured_mega_test () -> 
  ?assertEqual ({ car, 11 }, testmegastructured (1)).

one_test () -> 
  ?assertEqual (11, testone (1)).

two_test () -> 
  ?assertEqual (11, testtwo (1)).

three_test () -> 
  ?assertEqual (11, testthree (1)).

noassign_test () -> 
  ?assertEqual (11, testnoassign ()).

-endif.

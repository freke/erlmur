-record(crypto_stats, {
    good = 0 :: non_neg_integer(),
    late = 0 :: non_neg_integer(),
    lost = 0 :: integer()
}).
-type crypto_stats() :: #crypto_stats{}.

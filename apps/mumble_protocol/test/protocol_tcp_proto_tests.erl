-module(protocol_tcp_proto_tests).
-include_lib("eunit/include/eunit.hrl").
-include("Mumble_gpb.hrl").

roundtrip_test_() ->
    Msgs = [
        #{message_type => 'Version', version_v1 => 1, release => <<"test">>},
        #{message_type => 'Authenticate', username => <<"user">>},
        #{message_type => 'Ping', timestamp => 12345},
        #{message_type => 'ServerSync', session => 1, welcome_text => <<"Hi">>},
        #{message_type => 'ChannelState', channel_id => 0, name => <<"Root">>},
        #{message_type => 'UserState', session => 1, name => <<"User">>}
    ],
    [?_assertEqual([mumble_msg:to_map(mumble_msg:from_map(Msg))], mumble_tcp_proto:decode(mumble_tcp_proto:pack(Msg))) || Msg <- Msgs].

multi_message_decode_test() ->
    Msg1 = #{message_type => 'Version', version_v1 => 1},
    Msg2 = #{message_type => 'Ping', timestamp => 100},
    Data = <<(mumble_tcp_proto:pack(Msg1))/binary, (mumble_tcp_proto:pack(Msg2))/binary>>,
    Expected1 = mumble_msg:to_map(mumble_msg:from_map(Msg1)),
    Expected2 = mumble_msg:to_map(mumble_msg:from_map(Msg2)),
    ?assertEqual([Expected1, Expected2], mumble_tcp_proto:decode(Data)).

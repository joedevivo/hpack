-module(hpack_integer_tests).

-include_lib("eunit/include/eunit.hrl").


encode_decode_simple_test() ->
    % Value, PrefixBits, Encoded
    Tests = [
        % Basic tests
        {0, 5, <<0:5>>},
        {62, 6, <<62:6>>},
        {63, 6, <<63:6, 0:8>>},
        {64, 6, <<63:6, 1:8>>},

        % Three byte encoding
        {267, 7, <<127:7, 140:8, 1:8>>},

        % Max value for prefix
        {1, 1, <<1:1, 0:8>>},
        {3, 2, <<3:2, 0:8>>},
        {7, 3, <<7:3, 0:8>>},
        {15, 4, <<15:4, 0:8>>},
        {31, 5, <<31:5, 0:8>>},
        {63, 6, <<63:6, 0:8>>},
        {127, 7, <<127:7, 0:8>>},
        {255, 8, <<255:8, 0:8>>},

        % Zero for each prefix length
        {0, 1, <<0:1>>},
        {0, 2, <<0:2>>},
        {0, 3, <<0:3>>},
        {0, 4, <<0:4>>},
        {0, 5, <<0:5>>},
        {0, 6, <<0:6>>},
        {0, 7, <<0:7>>},
        {0, 8, <<0:8>>}
    ],
    lists:foreach(fun({Value, PrefixBits, Encoded}) ->
        ?assertEqual(Encoded, hpack_integer:encode(Value, PrefixBits)),
        ?assertEqual({Value, <<>>}, hpack_integer:decode(Encoded, PrefixBits))
    end, Tests).


decode_with_tail_test() ->
    Tests = [
        % Prefix bits, encoded, decoded
        {5, <<0:5, 1:8>>, {0, <<1:8>>}},
        {6, <<62:6, 1:8>>, {62, <<1:8>>}},
        {6, <<63:6, 0:8, 1:8>>, {63, <<1:8>>}},
        {6, <<63:6, 1:8, 1:8>>, {64, <<1:8>>}}
    ],

    lists:foreach(fun({PrefixBits, Encoded, Decoded}) ->
        ?assertEqual(Decoded, hpack_integer:decode(Encoded, PrefixBits))
    end, Tests).


roundtrip_sequential_test() ->
    lists:foreach(fun(Value) ->
        lists:foreach(fun(PrefixBits) ->
            roundtrip(Value, PrefixBits)
        end, lists:seq(1, 8))
    end, lists:seq(0, 512)).


roundtrip_random_test() ->
    lists:foreach(fun(_) ->
        Value = hpack_tutil:random_int(16#FFFF),
        lists:foreach(fun(PrefixBits) ->
            roundtrip(Value, PrefixBits)
        end, lists:seq(1, 8))
    end, lists:seq(0, 512)).


roundtrip(Value, PrefixBits) ->
    Encoded = hpack_integer:encode(Value, PrefixBits),
    Decoded = hpack_integer:decode(Encoded, PrefixBits),
    ?assertEqual({Value, <<>>}, Decoded),

    Garbage = hpack_tutil:random_bin(64),
    WithGarbage = <<Encoded/bits, Garbage/binary>>,
    ?assertEqual(
        {Value, Garbage},
        hpack_integer:decode(WithGarbage, PrefixBits)
    ).

-module(hpack_integer_tests).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

decode_zero_test() ->
    ?assertEqual({0, <<>>}, hpack_integer:decode(<<0:5>>, 5)),
    ?assertEqual({0, <<1>>}, hpack_integer:decode(<<0:5,1:8>>, 5)),
    ok.

decode_sixtytwo_test() ->
    ?assertEqual({62, <<>>}, hpack_integer:decode(<<62:6>>, 6)),
    ?assertEqual({62, <<1>>}, hpack_integer:decode(<<62:6,1:8>>, 6)),
    ok.

decode_sixtyfour_test() ->
    ?assertEqual({64, <<>>}, hpack_integer:decode(<<63:6,1:8>>, 6)),
    ?assertEqual({64, <<1>>}, hpack_integer:decode(<<63:6,1:8,1:8>>, 6)),
    ok.

encode_integer_test() ->
    ?assertEqual(<<62:6>>, hpack_integer:encode(62,6)),
    ?assertEqual(<<63:6,1:8>>, hpack_integer:encode(64,6)),
    ?assertEqual(<<63:6,0:8>>, hpack_integer:encode(63,6)),
    ok.

encode_prefix_max_equals_value_test() ->
    ?assertEqual(<<  1:1,0:8>>, hpack_integer:encode(  1,1)),
    ?assertEqual(<<  3:2,0:8>>, hpack_integer:encode(  3,2)),
    ?assertEqual(<<  7:3,0:8>>, hpack_integer:encode(  7,3)),
    ?assertEqual(<< 15:4,0:8>>, hpack_integer:encode( 15,4)),
    ?assertEqual(<< 31:5,0:8>>, hpack_integer:encode( 31,5)),
    ?assertEqual(<< 63:6,0:8>>, hpack_integer:encode( 63,6)),
    ?assertEqual(<<127:7,0:8>>, hpack_integer:encode(127,7)),
    ?assertEqual(<<255,0>>,     hpack_integer:encode(255,8)),
    ok.

encode_zero_test() ->
    ?assertEqual(<<0:1>>, hpack_integer:encode(0,1)),
    ?assertEqual(<<0:2>>, hpack_integer:encode(0,2)),
    ?assertEqual(<<0:3>>, hpack_integer:encode(0,3)),
    ?assertEqual(<<0:4>>, hpack_integer:encode(0,4)),
    ?assertEqual(<<0:5>>, hpack_integer:encode(0,5)),
    ?assertEqual(<<0:6>>, hpack_integer:encode(0,6)),
    ?assertEqual(<<0:7>>, hpack_integer:encode(0,7)),
    ?assertEqual(<<0>>,   hpack_integer:encode(0,8)),
    ok.

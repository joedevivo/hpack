-module(huffman_tests).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

encode_wwwexamplecom_test() ->
    ?assertEqual(<<16#f1,16#e3,16#c2,16#e5,16#f2,16#3a,16#6b,16#a0,16#ab,16#90,16#f4,16#ff>>,
                 huffman:encode(<<"www.example.com">>)),
    ok.

decode_wwwexamplecom_test() ->

    ?assertEqual(<<"www.example.com">>,
                 huffman:decode(<<16#f1,16#e3,16#c2,16#e5,16#f2,16#3a,16#6b,16#a0,16#ab,16#90,16#f4,16#ff>>)),
    ok.

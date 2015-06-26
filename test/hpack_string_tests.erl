-module(hpack_string_tests).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

basic_decode_test() ->
    %% "Commands
    Bin = <<0:1,8:7,$C,$o,$m,$m,$a,$n,$d,$s>>,
    ?assertEqual({<<"Commands">>, <<>>}, hpack_string:decode(Bin)),
    ok.

basic_decode_with_huffman_test() ->
    %% "Commands"
    Bin = <<1:1,
            6:7, %% now it's 6 bytes instead of 8. Savings!
            2#1011110:7, %% $C  7
            2#00111:5,   %% $o  5
            2#101001:6,  %% $m  6
            2#101001:6,  %% $m  6
            2#00011:5,   %% $a  5
            2#101010:6,  %% $n  6
            2#100100:6,  %% $d  6
            2#01000:5,   %% $s +5
            0:2          %%   =46
          >>,
    ?assertEqual({<<"Commands">>, <<>>}, hpack_string:decode(Bin)),
    ok.

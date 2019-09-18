
-module(hpack_huffman_tests).

-include_lib("eunit/include/eunit.hrl").


www_example_com_test() ->
    Expect = <<
        16#f1, 16#e3, 16#c2, 16#e5,
        16#f2, 16#3a, 16#6b, 16#a0,
        16#ab, 16#90, 16#f4, 16#ff
    >>,
    ?assertEqual(Expect, hpack_huffman:encode(<<"www.example.com">>)),
    ?assertEqual(<<"www.example.com">>, hpack_huffman:decode(Expect)).


invalid_eos_padding_test() ->
    ?assertEqual(<<"foo">>, hpack_huffman:decode(<<148, 231>>)),
    ?assertThrow(
            {hpack_error, {invalid_huffman_encoding, partial_code}},
            hpack_huffman:decode(<<148, 231, 0>>)
        ).


invalid_internal_eos_test() ->
    Bin = <<
        16#25:6, % "f"
        16#3fffffff:30, % "eos"
        16#ffa:12 % "#"
    >>,
    ?assertThrow(
            {hpack_error, {invalid_huffman_encoding, internal_eos}},
            hpack_huffman:decode(Bin)
        ).

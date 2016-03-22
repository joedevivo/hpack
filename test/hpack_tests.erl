-module(hpack_tests).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

basic_nghttp2_request_test() ->
    Bin = <<130,132,134,65,138,160,228,29,19,157,9,184,240,30,7,83,3,42,47,42,144,
            122,138,170,105,210,154,196,192,23,117,119,127>>,

    {ok,
     {Decoded=[H1,H2,H3,H4,H5,H6,H7], _DC}} = hpack:decode(Bin, hpack:new_context()),
    % :method: GET
    ?assertEqual({<<":method">>, <<"GET">>},H1),
    % :path: /
    ?assertEqual({<<":path">>, <<"/">>},H2),
    % :scheme: http
    ?assertEqual({<<":scheme">>, <<"http">>},H3),
    % :authority: localhost:8080
    ?assertEqual({<<":authority">>, <<"localhost:8080">>},H4),
    % accept: */*
    ?assertEqual({<<"accept">>, <<"*/*">>},H5),
    % accept-encoding: gzip, deflate
    ?assertEqual({<<"accept-encoding">>, <<"gzip, deflate">>},H6),
    % user-agent: nghttp2/0.7.7
    ?assertEqual({<<"user-agent">>, <<"nghttp2/0.7.7">>},H7),


    {ok, {ReEncoded, _EncodeContext}} = hpack:encode([H1,H2,H3,H4,H5,H6,H7], hpack:new_context()),
    {ok, {ReDecoded, _}} = hpack:decode(ReEncoded, hpack:new_context()),
    io:format("Original : ~p~n", [Bin]),
    io:format("ReEncoded: ~p~n", [ReEncoded]),
    io:format("ReDecoded: ~p~n", [ReDecoded]),

    ?assertEqual(Decoded, ReDecoded),
    ok.

decode_1_test() ->
    Bin = <<130,132,134,65,138,160,228,29,19,157,9,184,240,30,15,83,3,42,47,42,
            144,122,138,170,105,210,154,196,192,23,117,112,135,64,135,242,178,
            125,117,73,236,175,1,66,126,1,79,64,133,242,181,37,63,143,1,112,126,
            1,116,127,1,1,116>>,
    {ok, {[
           H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12
          ], _DC}} = hpack:decode(Bin, hpack:new_context()),

    ?assertEqual({<<":method">>, <<"GET">>},H1),
    ?assertEqual({<<":path">>, <<"/">>},H2),
    ?assertEqual({<<":scheme">>, <<"http">>},H3),
    ?assertEqual({<<":authority">>, <<"localhost:8081">>},H4),
    ?assertEqual({<<"accept">>, <<"*/*">>},H5),
    ?assertEqual({<<"accept-encoding">>, <<"gzip, deflate">>},H6),
    ?assertEqual({<<"user-agent">>, <<"nghttp2/0.7.11">>},H7),
    ?assertEqual({<<"x-tyktorp">>, <<"B">>},H8),
    ?assertEqual({<<"x-tyktorp">>, <<"O">>},H9),
    ?assertEqual({<<"x-meow">>, <<"p">>},H10),
    ?assertEqual({<<"x-meow">>, <<"t">>},H11),
    ?assertEqual({<<"x-tyktorp">>, <<"t">>},H12),
    ok.

% http://http2.github.io/http2-spec/compression.html#rfc.section.C.2.1
decode_c_2_1_test() ->
    Bin = <<16#40,16#0a,16#63,16#75,16#73,16#74,16#6f,16#6d,16#2d,16#6b,16#65,
            16#79,16#0d,16#63,16#75,16#73,16#74,16#6f,16#6d,16#2d,16#68,16#65,
            16#61,16#64,16#65,16#72>>,
    BinStr = <<"@\ncustom-key\rcustom-header">>,
    ?assertEqual(Bin, BinStr),
    %% 400a 6375 7374 6f6d 2d6b 6579 0d63 7573 | @.custom-key.cus
    %% 746f 6d2d 6865 6164 6572                | tom-header
    {ok, {[H1],_DC}} = hpack:decode(Bin, hpack:new_context()),
    ?assertEqual({<<"custom-key">>, <<"custom-header">>}, H1),
    ok.

% http://http2.github.io/http2-spec/compression.html#rfc.section.C.2.2
decode_c_2_2_test() ->
    Bin = <<16#04,16#0c,16#2f,16#73,16#61,16#6d,16#70,16#6c,16#65,
             16#2f,16#70,16#61,16#74,16#68>>,
    %% input| 040c 2f73 616d 706c 652f 7061 7468
    %% out  | :path: /sample/path
    {ok, {[H1],_DC}} = hpack:decode(Bin, hpack:new_context()),
    ?assertEqual({<<":path">>, <<"/sample/path">>}, H1),
    ok.

% http://http2.github.io/http2-spec/compression.html#rfc.section.C.2.3
decode_c_2_3_test() ->
    Bin = <<16#10, 16#08, 16#70, 16#61, 16#73, 16#73, 16#77, 16#6f,
            16#72, 16#64, 16#06, 16#73, 16#65, 16#63, 16#72, 16#65, 16#74>>,

    %% input| 1008 7061 7373 776f 7264 0673 6563 7265 74
    %% out  | password: secret
    {ok, {[H1],_DC}} = hpack:decode(Bin, hpack:new_context()),
    ?assertEqual({<<"password">>, <<"secret">>}, H1),
    ok.

% http://http2.github.io/http2-spec/compression.html#rfc.section.C.2.4
decode_c_2_4_test() ->
    Bin = <<16#82>>,
    %% input| 82
    %% out  | :method: GET
    {ok, {[H1],_DC}} = hpack:decode(Bin, hpack:new_context()),
    ?assertEqual({<<":method">>, <<"GET">>}, H1),
    ok.

% http://http2.github.io/http2-spec/compression.html#rfc.section.C.3.1
decode_c_3_test() ->
    C_3_1 = <<16#82, 16#86, 16#84, 16#41, 16#0f, 16#77, 16#77, 16#77,
            16#2e, 16#65, 16#78, 16#61, 16#6d, 16#70, 16#6c, 16#65,
            16#2e, 16#63, 16#6f, 16#6d >>,
    {ok, {[R1H1, R1H2, R1H3, R1H4], DC2}} = hpack:decode(C_3_1, hpack:new_context()),
    io:format("DC2: ~p", [DC2]),
    ?assertEqual({<<":method">>, <<"GET">>}, R1H1),
    ?assertEqual({<<":scheme">>, <<"http">>}, R1H2),
    ?assertEqual({<<":path">>, <<"/">>}, R1H3),
    ?assertEqual({<<":authority">>, <<"www.example.com">>}, R1H4),

    C_3_2 = <<16#82, 16#86, 16#84, 16#be, 16#58, 16#08, 16#6e, 16#6f,
            16#2d, 16#63, 16#61, 16#63, 16#68, 16#65>>,
    {ok, {[R2H1, R2H2, R2H3, R2H4, R2H5], DC3}} = hpack:decode(C_3_2, DC2),
    io:format("DC3: ~p", [DC3]),

    ?assertEqual({<<":method">>, <<"GET">>}, R2H1),
    ?assertEqual({<<":scheme">>, <<"http">>}, R2H2),
    ?assertEqual({<<":path">>, <<"/">>}, R2H3),
    ?assertEqual({<<":authority">>, <<"www.example.com">>}, R2H4),
    ?assertEqual({<<"cache-control">>, <<"no-cache">>}, R2H5),

    C_3_3 = <<16#82, 16#87, 16#85, 16#bf, 16#40, 16#0a, 16#63, 16#75,
              16#73, 16#74, 16#6f, 16#6d, 16#2d, 16#6b, 16#65, 16#79,
              16#0c, 16#63, 16#75, 16#73, 16#74, 16#6f, 16#6d, 16#2d,
              16#76, 16#61, 16#6c, 16#75, 16#65>>,
    {ok, {[R3H1, R3H2, R3H3, R3H4, R3H5], DC4}} = hpack:decode(C_3_3, DC3),
    io:format("DC4: ~p", [DC4]),
    ?assertEqual({<<":method">>   , <<"GET">>}            , R3H1),
    ?assertEqual({<<":scheme">>   , <<"https">>}          , R3H2),
    ?assertEqual({<<":path">>     , <<"/index.html">>}    , R3H3),
    ?assertEqual({<<":authority">>, <<"www.example.com">>}, R3H4),
    ?assertEqual({<<"custom-key">>, <<"custom-value">>}   , R3H5),
    ok.

% http://http2.github.io/http2-spec/compression.html#rfc.section.C.4.1
decode_c_4_test() ->
    C_4_1 = <<16#82, 16#86, 16#84, 16#41, 16#8c, 16#f1, 16#e3, 16#c2,
              16#e5, 16#f2, 16#3a, 16#6b, 16#a0, 16#ab, 16#90, 16#f4,
              16#ff>>,
    {ok, {[R1H1, R1H2, R1H3, R1H4], DC2}} = hpack:decode(C_4_1, hpack:new_context()),
    io:format("DC2: ~p", [DC2]),
    ?assertEqual({<<":method">>, <<"GET">>}, R1H1),
    ?assertEqual({<<":scheme">>, <<"http">>}, R1H2),
    ?assertEqual({<<":path">>, <<"/">>}, R1H3),
    ?assertEqual({<<":authority">>, <<"www.example.com">>}, R1H4),

    C_4_2 = <<16#82, 16#86, 16#84, 16#be, 16#58, 16#86,
              16#a8, 16#eb, 16#10, 16#64, 16#9c, 16#bf>>,
    {ok, {[R2H1, R2H2, R2H3, R2H4, R2H5], DC3}} = hpack:decode(C_4_2, DC2),
    io:format("DC3: ~p", [DC3]),

    ?assertEqual({<<":method">>, <<"GET">>}, R2H1),
    ?assertEqual({<<":scheme">>, <<"http">>}, R2H2),
    ?assertEqual({<<":path">>, <<"/">>}, R2H3),
    ?assertEqual({<<":authority">>, <<"www.example.com">>}, R2H4),
    ?assertEqual({<<"cache-control">>, <<"no-cache">>}, R2H5),

    C_4_3 = <<16#82, 16#87, 16#85, 16#bf, 16#40, 16#88, 16#25, 16#a8,
              16#49, 16#e9, 16#5b, 16#a9, 16#7d, 16#7f, 16#89, 16#25,
              16#a8, 16#49, 16#e9, 16#5b, 16#b8, 16#e8, 16#b4, 16#bf>>,
    {ok, {[R3H1, R3H2, R3H3, R3H4, R3H5], DC4}} = hpack:decode(C_4_3, DC3),
    io:format("DC4: ~p", [DC4]),
    ?assertEqual({<<":method">>   , <<"GET">>}            , R3H1),
    ?assertEqual({<<":scheme">>   , <<"https">>}          , R3H2),
    ?assertEqual({<<":path">>     , <<"/index.html">>}    , R3H3),
    ?assertEqual({<<":authority">>, <<"www.example.com">>}, R3H4),
    ?assertEqual({<<"custom-key">>, <<"custom-value">>}   , R3H5),
    ok.

hpack_c_1_1_test() ->
    Encoded = hpack_integer:encode(10, 5),
    ?assertEqual(<<10:5>>, Encoded),
    ok.

hpack_c_1_2_test() ->
    Encoded = hpack_integer:encode(1337, 5),
    ?assertEqual(<<252,208,10:5>>, Encoded),
    ok.

hpack_c_2_4_test() ->
    EncodedInt = hpack_integer:encode(2,7),
    ?assertEqual(<<2:7>>, EncodedInt),
    {ok, {Encoded, _}} = hpack:encode([{<<":method">>, <<"GET">>}], hpack:new_context()),
    ?assertEqual(<<16#82>>, Encoded),
    ok.

%%encode_indexed_test() ->
%%    ?assertEqual(<<2#11111110>>, hpack:encode_indexed(62)),
%%    ?assertEqual(<<2#11111111>>, hpack:encode_indexed(63)),
%%
%%    ok.

% Regression tests
decode_indexed_static_test() ->
    Bin = <<2#10001000>>,
    {ok, {[H1], DC2}} = hpack:decode(Bin, hpack:new_context()),
    io:format("DC2: ~p", [DC2]),
    ?assertEqual({<<":status">>, <<"200">>}, H1),
    ok.

decode_literal_incremental_indexing_indexed_test() ->
    Bin = <<2#01000100, 2#00000101, "/test">>,
    {ok, {[H1], DC2}} = hpack:decode(Bin, hpack:new_context()),
    io:format("DC2: ~p", [DC2]),
    ?assertEqual({<<":path">>, <<"/test">>}, H1),
    ok.

decode_literal_incremental_indexing_new_test() ->
    Bin = <<2#01000000, 2#00001010, "custom-key", 2#00001100, "custom-value">>,
    {ok, {[H1], DC2}} = hpack:decode(Bin, hpack:new_context()),
    io:format("DC2: ~p", [DC2]),
    ?assertEqual({<<"custom-key">>, <<"custom-value">>}, H1),
    ok.

decode_literal_without_indexing_indexed_test() ->
    Bin = <<2#00001111, 2#00101011, 2#00000111, "Firefox">>,
    {ok, {[H1], DC2}} = hpack:decode(Bin, hpack:new_context()),
    io:format("DC2: ~p", [DC2]),
    ?assertEqual({<<"user-agent">>, <<"Firefox">>}, H1),
    ?assertEqual(DC2, hpack:new_context()),
    ok.

decode_literal_without_indexing_new_test() ->
    Bin = <<2#00000000, 2#00001010, "custom-key", 2#00001100, "custom-value">>,
    {ok, {[H1], DC2}} = hpack:decode(Bin, hpack:new_context()),
    io:format("DC2: ~p", [DC2]),
    ?assertEqual({<<"custom-key">>, <<"custom-value">>}, H1),
    ?assertEqual(DC2, hpack:new_context()),
    ok.

decode_literal_never_indexed_indexed_test() ->
    Bin = <<2#00011111, 2#00101011, 2#00000111, "Firefox">>,
    {ok, {[H1], DC2}} = hpack:decode(Bin, hpack:new_context()),
    io:format("DC2: ~p", [DC2]),
    ?assertEqual({<<"user-agent">>, <<"Firefox">>}, H1),
    ?assertEqual(DC2, hpack:new_context()),
    ok.

decode_literal_never_indexed_new_test() ->
    Bin = <<2#00010000, 2#00001010, "custom-key", 2#00001100, "custom-value">>,
    {ok, {[H1], DC2}} = hpack:decode(Bin, hpack:new_context()),
    io:format("DC2: ~p", [DC2]),
    ?assertEqual({<<"custom-key">>, <<"custom-value">>}, H1),
    ?assertEqual(DC2, hpack:new_context()),
    ok.

compression_error_on_too_large_size_increase_test() ->
    Error = hpack:decode(<<2#001:3,255,16,31:5>>, hpack:new_context()),
    ?assertEqual({error, compression_error}, Error),
    ok.

no_compression_error_on_small_enough_adjustment_test() ->
    {Ok, {EmptyList, _NewContext}} =
        hpack:decode(<<2#001:3,255,16,0:5>>, hpack:new_context()),
    ?assertEqual(ok, Ok),
    ?assertEqual([], EmptyList),
    ok.


no_compression_error_on_two_adjustments_test() ->
    {Ok, _Return} =
        hpack:decode(<<16#20, 16#3f, 16#e1, 16#1f>>, hpack:new_context()),
    ?assertEqual(ok, Ok),
    ok.

%% If a value starts with 01000000, there has to be something after it
compression_error_on_indexed_fieed_no_value_test() ->
    Error =
        hpack:decode(<<16#40>>, hpack:new_context()),
    ?assertEqual(error, Error),
    ok.

-module(hpack_decode_tests).

-include_lib("eunit/include/eunit.hrl").


decode_static_indexed_test() ->
    Encoded = <<2#1:1, 2#0000010:7>>,
    Decoded = [{<<":method">>, <<"GET">>}],

    {ok, _, Result} = hpack:decode(hpack:new(), Encoded),
    ?assert(hpack_tutil:headers_equal(Decoded, Result)).


decode_known_name_incremental_index_test() ->
    AddToIndex = <<2#01:2>>,            % Add to index

    IndexedUA = <<
        2#111010:6                      % Static table index: 58 user-agent
    >>,

    CompFF = <<
        2#1:1,                          % compressed
        2#0000110:7,                    % 6 bytes
        194, 107, 11, 41, 252, 255      % data
    >>,

    UncompFF = <<
        2#0:1,                          % uncompressed
        2#0000111:7,                    % 7 bytes
        "Firefox"                       % data
    >>,

    Combinations = [
        [AddToIndex, IndexedUA, CompFF],
        [AddToIndex, IndexedUA, UncompFF]
    ],

    check_combinations(Combinations, [{<<"user-agent">>, <<"Firefox">>}]).


decode_unknown_name_incremental_index_test() ->
    AddToIndex = <<2#01:2>>,            % Add to index
    NoNameIndex = <<2#000000:6>>,       % No name index

    CompName = <<
        2#1:1,                          % Compressed
        2#0001000:7,                    % 8 bytes
        37, 168,  73, 233,
        90, 161, 210,  95               % compressed: custom-name
    >>,

    UncompName = <<
        2#0:1,                          % Uncompressed
        2#0001011:7,                    % 11 bytes
        "custom-name"                   % data
    >>,

    CompValue = <<
        2#1:1,                          % compressed
        2#0001001:7,                    % 9 bytes
         37, 168,  73, 233,
         91, 184, 232, 180,
        191                             % compressed: custom-value
    >>,

    UncompValue = <<
        2#0:1,                          % Uncompressed
        2#0001100:7,                    % 12 bytes
        "custom-value"                  % data
    >>,

    Combinations = [
        [AddToIndex, NoNameIndex, CompName, CompValue],
        [AddToIndex, NoNameIndex, UncompName, UncompValue]
    ],

    check_combinations(Combinations, [{<<"custom-name">>, <<"custom-value">>}]).


decode_unindexed_test() ->
    NoIndex = <<2#0000:4>>,             % Don't index this header
    NeverIndex = <<2#0001:4>>,          % Never index this header

    NoNameIndex = <<2#0000:4>>,         % 0 indicates name follows

    IndexedUA = <<
        2#1111:4, 2#00101011:8          % Static table index: 58 user-agent
    >>,

    CompUA = <<
        2#1:1,                          % huffman encoded
        2#0000111:7,                    % number of bytes
        181, 5, 177, 97, 204, 90, 147   % compressed: user-agent
    >>,

    UncompUA = <<
        2#0:1,                          % uncompressed
        2#0001010:7,                    % 10 bytes
        "user-agent"                    % data
    >>,

    CompFF = <<
        2#1:1,                          % compressed
        2#0000110:7,                    % 6 bytes
        194, 107, 11, 41, 252, 255      % data
    >>,

    UncompFF = <<
        2#0:1,                          % uncompressed
        2#0000111:7,                    % 7 bytes
        "Firefox"                       % data
    >>,

    Combinations = [
        [NoIndex, IndexedUA, CompFF],
        [NeverIndex, IndexedUA, CompFF],
        [NoIndex, NoNameIndex, CompUA, CompFF],
        [NeverIndex, NoNameIndex, CompUA, CompFF],
        [NoIndex, IndexedUA, UncompFF],
        [NeverIndex, IndexedUA, UncompFF],
        [NoIndex, NoNameIndex, UncompUA, UncompFF],
        [NeverIndex, NoNameIndex, UncompUA, UncompFF]
    ],

    check_combinations(Combinations, [{<<"user-agent">>, <<"Firefox">>}]).


decode_unknown_name_unindexed_test() ->
    NoIndex = <<2#0000:4>>,             % Don't index this header
    NeverIndex = <<2#0001:4>>,          % Never index this header
    NoNameIndex = <<2#0000:4>>,         % No name index

    CompName = <<
        2#1:1,                          % Compressed
        2#0001000:7,                    % 8 bytes
        37, 168,  73, 233,
        90, 161, 210,  95               % compressed: custom-name
    >>,

    UncompName = <<
        2#0:1,                          % Uncompressed
        2#0001011:7,                    % 11 bytes
        "custom-name"                   % data
    >>,

    CompValue = <<
        2#1:1,                          % compressed
        2#0001001:7,                    % 9 bytes
         37, 168,  73, 233,
         91, 184, 232, 180,
        191                             % compressed: custom-value
    >>,

    UncompValue = <<
        2#0:1,                          % Uncompressed
        2#0001100:7,                    % 12 bytes
        "custom-value"                  % data
    >>,

    Combinations = [
        [NoIndex, NoNameIndex, CompName, CompValue],
        [NeverIndex, NoNameIndex, CompName, CompValue],
        [NoIndex, NoNameIndex, CompName, CompValue],
        [NeverIndex, NoNameIndex, CompName, CompValue],
        [NoIndex, NoNameIndex, UncompName, UncompValue],
        [NeverIndex, NoNameIndex, UncompName, UncompValue],
        [NoIndex, NoNameIndex, UncompName, UncompValue],
        [NeverIndex, NoNameIndex, UncompName, UncompValue]
    ],

    check_combinations(Combinations, [{<<"custom-name">>, <<"custom-value">>}]).


decode_size_update_test() ->
    Encoded = <<
        2#001:3,                        % Size update
        2#11111:5,
        2#11100001:8,
        2#00001111:8                    % Integer: 2048
    >>,
    Ctx1 = hpack:new(),
    {ok, Ctx2, []} = hpack:decode(Ctx1, Encoded),

    ?assertEqual(4096, hpack_index:max_size(Ctx1)),
    ?assertEqual(2048, hpack_index:max_size(Ctx2)).


decode_multiple_size_updates_test() ->
    Encoded = <<
        2#001:3,                        % Size update
        2#00000:5,                      % Integer: 0
        2#001:3,                        % Size update
        2#11111:5,
        2#11100001:8,
        2#00001111:8                    % Integer: 2048
    >>,

    Ctx1 = hpack:new(),
    {ok, Ctx2, []} = hpack:decode(Ctx1, Encoded),

    ?assertEqual(4096, hpack_index:max_size(Ctx1)),
    ?assertEqual(2048, hpack_index:max_size(Ctx2)).


decode_invalid_indexed_header_test() ->
    Encoded = <<
        2#1:1,                          % Indexed header
        2#0000000:7                     % Index: 0
    >>,
    ?assertEqual(
            {error, {invalid_index, 0}},
            hpack:decode(hpack:new(), Encoded)
        ).


decode_unknown_indexed_header_test() ->
    Encoded = <<
        2#1:1,                          % Indexed header
        2#1111110:7                     % Index: 126
    >>,
    ?assertEqual(
            {error, {unknown_index, 126}},
            hpack:decode(hpack:new(), Encoded)
        ).


decode_unknown_index_name_test() ->
    Encoded = <<
        2#01:2,                         % Add to index
        2#111110:6,                     % Index: 62
        2#0:1,                          % Uncompressed
        2#0000001:7,                    % 1 byte
        "a"                             % data
    >>,
    ?assertEqual(
            {error, {unknown_index, 62}},
            hpack:decode(hpack:new(), Encoded)
        ).


decode_unknown_index_name_no_index_test() ->
    Encoded = <<
        2#0000:4,                       % Don't index
        2#1111:4,
        2#01101111:8,                   % Index: 126
        2#0:1,                          % Uncompressed
        2#0000001:7,                    % 1 byte
        "a"                             % data
    >>,
    ?assertEqual(
            {error, {unknown_index, 126}},
            hpack:decode(hpack:new(), Encoded)
        ).


decode_unknown_index_name_never_index_test() ->
    Encoded = <<
        2#0001:4,                       % Don't index
        2#1111:4,
        2#01101111:8,                   % Index: 126
        2#0:1,                          % Uncompressed
        2#0000001:7,                    % 1 byte
        "a"                             % data
    >>,
    ?assertEqual(
            {error, {unknown_index, 126}},
            hpack:decode(hpack:new(), Encoded)
        ).


decode_size_update_too_large_test() ->
    Encoded = <<
        2#001:3,                        % Size update
        2#11111:5,
        2#11100001:8,
        2#01111111:8                    % Integer: 16,384
    >>,
    ?assertEqual(
            {error, {invalid_table_size, 16384}},
            hpack:decode(hpack:new(), Encoded)
        ).


decode_invalid_size_update_test() ->
    % RFC 7541 Section 4.2 says size updates have
    % to happen at the beginning of a header block

    Encoded = <<
        2#1:1,                          % Indexed header
        2#0000010:7,                    % Index 2: :method GET
        2#001:3,                        % Size update
        2#11111:5,
        2#11100001:8,
        2#00001111:8                    % Integer: 2048
    >>,

    ?assertEqual(
            {error, {invalid_size_update, headers_received}},
            hpack:decode(hpack:new(), Encoded)
        ).


check_combinations(Combinations, Headers) ->
    lists:foreach(fun(Parts) ->
        <<Encoded/binary>> = list_to_bitstring(Parts),
        {ok, _, Result} = hpack:decode(hpack:new(), Encoded),
        ?assert(hpack_tutil:headers_equal(Headers, Result))
    end, Combinations).

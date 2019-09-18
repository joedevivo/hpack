-module(hpack_encode_tests).

-include_lib("eunit/include/eunit.hrl").


encode_static_indexed_test() ->
    Encoded = <<2#1:1, 2#0000010:7>>,
    ?assertMatch(
            {ok, _, Encoded},
            hpack:encode(hpack:new(), [{<<":method">>, <<"GET">>}])
        ).


encode_known_name_incremental_index_test() ->
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
        {[], [AddToIndex, IndexedUA, CompFF]},
        {[uncompressed], [AddToIndex, IndexedUA, UncompFF]}
    ],

    check_combinations(Combinations, <<"user-agent">>, <<"Firefox">>).


encode_unknown_name_incremental_index_test() ->
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
        {[], [AddToIndex, NoNameIndex, CompName, CompValue]},
        {[uncompressed], [AddToIndex, NoNameIndex, UncompName, UncompValue]}
    ],

    check_combinations(Combinations, <<"custom-name">>, <<"custom-value">>).


encode_unindexed_test() ->
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
        {
            [no_index],
            [NoIndex, IndexedUA, CompFF]
        },
        {
            [never_index],
            [NeverIndex, IndexedUA, CompFF]
        },
        {
            [no_index, no_name_index],
            [NoIndex, NoNameIndex, CompUA, CompFF]
        },
        {
            [never_index, no_name_index],
            [NeverIndex, NoNameIndex, CompUA, CompFF]
        },
        {
            [no_index, uncompressed],
            [NoIndex, IndexedUA, UncompFF]
        },
        {
            [never_index, uncompressed],
            [NeverIndex, IndexedUA, UncompFF]
        },
        {
            [no_index, no_name_index, uncompressed],
            [NoIndex, NoNameIndex, UncompUA, UncompFF]
        },
        {
            [never_index, no_name_index, uncompressed],
            [NeverIndex, NoNameIndex, UncompUA, UncompFF]
        }
    ],

    check_combinations(Combinations, <<"user-agent">>, <<"Firefox">>).


encode_unknown_name_unindexed_test() ->
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
        {
            [no_index],
            [NoIndex, NoNameIndex, CompName, CompValue]
        },
        {
            [never_index],
            [NeverIndex, NoNameIndex, CompName, CompValue]
        },
        {
            [no_index, no_name_index],
            [NoIndex, NoNameIndex, CompName, CompValue]
        },
        {
            [never_index, no_name_index],
            [NeverIndex, NoNameIndex, CompName, CompValue]
        },
        {
            [no_index, uncompressed],
            [NoIndex, NoNameIndex, UncompName, UncompValue]
        },
        {
            [never_index, uncompressed],
            [NeverIndex, NoNameIndex, UncompName, UncompValue]
        },
        {
            [no_index, no_name_index, uncompressed],
            [NoIndex, NoNameIndex, UncompName, UncompValue]
        },
        {
            [never_index, no_name_index, uncompressed],
            [NeverIndex, NoNameIndex, UncompName, UncompValue]
        }
    ],

    check_combinations(Combinations, <<"custom-name">>, <<"custom-value">>).


encode_invalid_header_test() ->
    ?assertEqual(
            {error, {invalid_header, foo}},
            hpack:encode(hpack:new(), [foo])
        ).


check_combinations(Combinations, Name, Value) ->
    lists:foreach(fun({Opts, Parts}) ->
        <<Expect/binary>> = list_to_bitstring(Parts),
        Headers = [{Name, Value, Opts}],
        {ok, _, Result} = hpack:encode(hpack:new(), Headers),
        ?assertEqual(Expect, Result)
    end, Combinations).

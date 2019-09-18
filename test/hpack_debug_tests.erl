-module(hpack_debug_tests).

-include_lib("eunit/include/eunit.hrl").


debug_static_indexed_test() ->
    Encoded = <<2#1:1, 2#0000010:7>>,

    Expect = [
        {<<2#1:1>>, indexed_header},
        {<<":method">>, <<"GET">>}
    ],

    {ok, _, Result} = hpack_debug:explain(hpack:new(), Encoded),
    ?assertEqual(Expect, Result).


debug_known_name_incremental_index_test() ->
    Encoded = <<
        2#01:2,                         % Add to index
        2#111010:6,                     % Static table index: 58 user-agent
        2#0:1,                          % uncompressed
        2#0000111:7,                    % 7 bytes
        "Firefox"                       % data
    >>,

    {ok, _, Result} = hpack_debug:explain(hpack:new(), Encoded),
    ?assertMatch([
        {<<1:2>>, incremental_index},
        {<<58:6>>, {indexed_name, 58, <<"user-agent">>}},
        {
            <<_/binary>>,
            {string, {<<7:7>>, 7, uncompressed}, <<"Firefox">>}
        }
    ], Result).


debug_known_name_incremental_index_compressed_test() ->
    Encoded = <<
        2#01:2,                         % Add to index
        2#111010:6,                     % Static table index: 58 user-agent
        2#1:1,                          % compressed
        2#0000110:7,                    % 6 bytes
        194, 107, 11, 41, 252, 255      % data
    >>,

    {ok, _, Result} = hpack_debug:explain(hpack:new(), Encoded),
    ?assertMatch([
        {<<1:2>>, incremental_index},
        {<<58:6>>, {indexed_name, 58, <<"user-agent">>}},
        {
            <<_/binary>>,
            {string, {<<6:7>>, 6, compressed}, <<"Firefox">>}
        }
    ], Result).


debug_unknown_name_incremental_index_test() ->
    Encoded = <<
        2#01:2,                         % Add to index
        2#000000:6,                     % Unknown name
        2#0:1,                          % uncompressed
        2#0000111:7,                    % 7 bytes
        "foo-bar",                      % data
        2#0:1,                          % uncompressed
        2#0000111:7,                    % 7 bytes
        "Firefox"                       % data
    >>,

    {ok, _, Result} = hpack_debug:explain(hpack:new(), Encoded),
    ?assertMatch([
        {<<1:2>>, incremental_index},
        {<<0:6>>, unindexed_name},
        {
            <<_/binary>>,
            {string, {<<7:7>>, 7, uncompressed}, <<"foo-bar">>}
        },
        {
            <<_/binary>>,
            {string, {<<7:7>>, 7, uncompressed}, <<"Firefox">>}
        }
    ], Result).


debug_known_name_no_index_test() ->
    Encoded = <<
        2#0000:4,                       % No index
        2#1111:4,
        2#00101011:8,                   % Index: 58 user-agent
        2#0:1,                          % uncompressed
        2#0000111:7,                    % 7 bytes
        "Firefox"                       % data
    >>,

    {ok, _, Result} = hpack_debug:explain(hpack:new(), Encoded),
    ?assertMatch([
        {<<0:4>>, no_index},
        {<<242,11:4>>, {indexed_name, 58, <<"user-agent">>}},
        {
            <<_/binary>>,
            {string, {<<7:7>>, 7, uncompressed}, <<"Firefox">>}
        }
    ], Result).


debug_unknown_name_no_index_test() ->
    Encoded = <<
        2#0000:4,                       % No index
        2#0000:4,                       % Unknown name
        2#0:1,                          % uncompressed
        2#0000111:7,                    % 7 bytes
        "foo-bar",                      % data
        2#0:1,                          % uncompressed
        2#0000111:7,                    % 7 bytes
        "Firefox"                       % data
    >>,

    {ok, _, Result} = hpack_debug:explain(hpack:new(), Encoded),
    ?assertMatch([
        {<<0:4>>, no_index},
        {<<0:4>>, unindexed_name},
        {
            <<_/binary>>,
            {string, {<<7:7>>, 7, uncompressed}, <<"foo-bar">>}
        },
        {
            <<_/binary>>,
            {string, {<<7:7>>, 7, uncompressed}, <<"Firefox">>}
        }
    ], Result).


debug_known_name_never_index_test() ->
    Encoded = <<
        2#0001:4,                       % No index
        2#1111:4,
        2#00101011:8,                   % Index: 58 user-agent
        2#0:1,                          % uncompressed
        2#0000111:7,                    % 7 bytes
        "Firefox"                       % data
    >>,

    {ok, _, Result} = hpack_debug:explain(hpack:new(), Encoded),
    ?assertMatch([
        {<<1:4>>, never_index},
        {<<242,11:4>>, {indexed_name, 58, <<"user-agent">>}},
        {
            <<_/binary>>,
            {string, {<<7:7>>, 7, uncompressed}, <<"Firefox">>}
        }
    ], Result).


debug_unknown_name_never_index_test() ->
    Encoded = <<
        2#0001:4,                       % No index
        2#0000:4,                       % Unknown name
        2#0:1,                          % uncompressed
        2#0000111:7,                    % 7 bytes
        "foo-bar",                      % data
        2#0:1,                          % uncompressed
        2#0000111:7,                    % 7 bytes
        "Firefox"                       % data
    >>,

    {ok, _, Result} = hpack_debug:explain(hpack:new(), Encoded),
    ?assertMatch([
        {<<1:4>>, never_index},
        {<<0:4>>, unindexed_name},
        {
            <<_/binary>>,
            {string, {<<7:7>>, 7, uncompressed}, <<"foo-bar">>}
        },
        {
            <<_/binary>>,
            {string, {<<7:7>>, 7, uncompressed}, <<"Firefox">>}
        }
    ], Result).


debug_size_update_test() ->
    Encoded = <<
        2#001:3,                        % Size update
        2#11111:5,
        2#11100001:8,
        2#00001111:8                    % Integer: 2048
    >>,
    {ok, _, Result} = hpack_debug:explain(hpack:new(), Encoded),

    ?assertMatch([
        {<<1:3>>, size_update},
        {<<_/bits>>, {new_size, 2048}}
    ], Result).


debug_multiple_size_updates_test() ->
    Encoded = <<
        2#001:3,                        % Size update
        2#00000:5,                      % Integer: 0
        2#001:3,                        % Size update
        2#11111:5,
        2#11100001:8,
        2#00001111:8                    % Integer: 2048
    >>,

    {ok, _, Result} = hpack_debug:explain(hpack:new(), Encoded),

    ?assertMatch([
        {<<1:3>>, size_update},
        {<<_/bits>>, {new_size, 0}},
        {<<1:3>>, size_update},
        {<<_/bits>>, {new_size, 2048}}
    ], Result).


debug_invalid_indexed_header_test() ->
    Encoded = <<
        2#1:1,                          % Indexed header
        2#0000000:7                     % Index: 0
    >>,
    ?assertEqual({error, [
            {<<1:1>>, indexed_header},
            {invalid_index, 0}
    ]}, hpack_debug:explain(hpack:new(), Encoded)).


debug_unknown_indexed_header_test() ->
    Encoded = <<
        2#1:1,                          % Indexed header
        2#1111110:7                     % Index: 126
    >>,
    ?assertEqual({error, [
            {<<1:1>>, indexed_header},
            {unknown_index, 126}
    ]}, hpack_debug:explain(hpack:new(), Encoded)).


debug_unknown_index_name_test() ->
    Encoded = <<
        2#01:2,                         % Add to index
        2#111110:6,                     % Index: 62
        2#0:1,                          % Uncompressed
        2#0000001:7,                    % 1 byte
        "a"                             % data
    >>,
    ?assertEqual({error, [
            {<<1:2>>, incremental_index},
            {unknown_index, 62}
    ]}, hpack_debug:explain(hpack:new(), Encoded)).


debug_unknown_index_name_no_index_test() ->
    Encoded = <<
        2#0000:4,                       % Don't index
        2#1111:4,
        2#01101111:8,                   % Index: 126
        2#0:1,                          % Uncompressed
        2#0000001:7,                    % 1 byte
        "a"                             % data
    >>,
    ?assertEqual({error, [
            {<<0:4>>, no_index},
            {unknown_index, 126}
    ]}, hpack_debug:explain(hpack:new(), Encoded)).


debug_unknown_index_name_never_index_test() ->
    Encoded = <<
        2#0001:4,                       % Don't index
        2#1111:4,
        2#01101111:8,                   % Index: 126
        2#0:1,                          % Uncompressed
        2#0000001:7,                    % 1 byte
        "a"                             % data
    >>,
    ?assertEqual({error, [
            {<<1:4>>, never_index},
            {unknown_index, 126}
    ]}, hpack_debug:explain(hpack:new(), Encoded)).


debug_invalid_size_update_test() ->
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

    ?assertEqual({error, [
        {<<1:1>>, indexed_header},
        {<<":method">>, <<"GET">>},
        {<<1:3>>, size_update},
        {invalid_size_update, headers_received}
    ]}, hpack_debug:explain(hpack:new(), Encoded)).


%% decode_size_update_too_large_test() ->
%%     Encoded = <<
%%         2#001:3,                        % Size update
%%         2#11111:5,
%%         2#11100001:8,
%%         2#01111111:8                    % Integer: 16,384
%%     >>,
%%     ?assertEqual(
%%             {error, {invalid_table_size, 16384}},
%%             hpack:decode(hpack:new(), Encoded)
%%         ).
%%
%%
%% decode_invalid_size_update_test() ->
%%     % RFC 7541 Section 4.2 says size updates have
%%     % to happen at the beginning of a header block
%%
%%     Encoded = <<
%%         2#1:1,                          % Indexed header
%%         2#0000010:7,                    % Index 2: :method GET
%%         2#001:3,                        % Size update
%%         2#11111:5,
%%         2#11100001:8,
%%         2#00001111:8                    % Integer: 2048
%%     >>,
%%
%%     ?assertEqual(
%%             {error, {invalid_size_update, headers_received}},
%%             hpack:decode(hpack:new(), Encoded)
%%         ).
%%
%%
%% check_combinations(Combinations, Headers) ->
%%     lists:foreach(fun(Parts) ->
%%         <<Encoded/binary>> = list_to_bitstring(Parts),
%%         {ok, _, Result} = hpack:decode(hpack:new(), Encoded),
%%         ?assert(hpack_tutil:headers_equal(Headers, Result))
%%     end, Combinations).

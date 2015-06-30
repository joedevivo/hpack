-module(hpack_index_tests).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

resize_test() ->
    DT = hpack_index:new(64),
    DT2 = hpack_index:add(<<"four">>,<<"four">>,DT),
    ?assertEqual(40, hpack_index:table_size(DT2)),
    DT3 = hpack_index:add(<<"--eight-">>,<<"--eight-">>,DT2),
    ?assertEqual(48, hpack_index:table_size(DT3)),
    ok.

increase_max_size_test() ->
    DT = hpack_index:new(64),

    %% add data, less than max size
    DT2 = hpack_index:add(<<"keyA">>, <<"valA">>, DT),
    ?assertEqual(40, %% 32 + length("keyA") + length("valA")
                 hpack_index:table_size(DT2)),

    %% increase max size
    DT3 = hpack_index:resize(128, DT2),

    %% what fit before should still fit
    ?assertEqual(hpack_index:table_size(DT2),
                 hpack_index:table_size(DT3)).

decrease_max_size_test() ->
    DT = hpack_index:new(64),

    %% add data, less than max size
    DT2 = hpack_index:add(<<"keyA">>, <<"valA">>, DT),
    ?assertEqual(40, %% 32 + length("keyA") + length("valA")
                 hpack_index:table_size(DT2)),

    %% reduce max size below current size
    DT3 = hpack_index:resize(32, DT2),

    %% the lone entry should have been removed, it didn't fit
    ?assertEqual(0, hpack_index:table_size(DT3)).

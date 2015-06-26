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

-module(hpack_index_tests).


-include("../src/hpack_index.hrl").
-include_lib("eunit/include/eunit.hrl").


size_test() ->
    Ctx0 = hpack:new(),
    ?assertEqual(0, hpack_index:size(Ctx0)),
    ?assertEqual(4096, hpack_index:max_size(Ctx0)).


evict_one_test() ->
    Ctx0 = hpack:new(64),
    Ctx1 = hpack_index:add(Ctx0, <<"four">>, <<"four">>),
    Ctx2 = hpack_index:add(Ctx1, <<"foo">>, <<"bar">>),

    ?assertEqual(
            {40, [{1, <<"four">>, <<"four">>}]},
            hpack_index:table(Ctx1)
        ),
    ?assertEqual(
            {38, [{1, <<"foo">>, <<"bar">>}]},
            hpack_index:table(Ctx2)
        ).


evict_multiple_test() ->
    Ctx0 = hpack:new(96),
    Ctx1 = hpack_index:add(Ctx0, <<"four">>, <<"four">>),
    Ctx2 = hpack_index:add(Ctx1, <<"foo">>, <<"bar">>),
    Ctx3 = hpack_index:add(Ctx2,
            <<"this-is-a-super-long-header-name">>,
            <<"this-is-also-a-long-value">>
        ),

    ?assertEqual({78, [
        {1, <<"foo">>, <<"bar">>},
        {2, <<"four">>, <<"four">>}
    ]}, hpack_index:table(Ctx2)),

    ?assertEqual({89, [
        {
            1,
            <<"this-is-a-super-long-header-name">>,
            <<"this-is-also-a-long-value">>
        }
    ]}, hpack_index:table(Ctx3)).


evict_all_test() ->
    Ctx0 = hpack:new(96),
    Ctx1 = hpack_index:add(Ctx0, <<"four">>, <<"four">>),
    Ctx2 = hpack_index:add(Ctx1, <<"foo">>, <<"bar">>),
    Ctx3 = hpack_index:add(Ctx2,
            <<"this-is-an-extra-super-duper-long-header-name">>,
            <<"this-is-also-a-super-duper-extra-long-value">>
        ),

    ?assertEqual({78, [
        {1, <<"foo">>, <<"bar">>},
        {2, <<"four">>, <<"four">>}
    ]}, hpack_index:table(Ctx2)),

    ?assertEqual({0, []}, hpack_index:table(Ctx3)).


resize_test() ->
    Ctx0 = hpack:new(),
    Ctx1 = hpack:resize(Ctx0, 2048),
    Ctx2 = hpack_index:add(Ctx1, <<"four">>, <<"four">>),
    Ctx3 = hpack_index:add(Ctx2, <<"foo">>, <<"bar">>),
    Ctx4 = hpack_index:add(Ctx3, <<"baz">>, <<"boop">>),

    Table1 = {117, [
        {1, <<"baz">>, <<"boop">>},
        {2, <<"foo">>, <<"bar">>},
        {3, <<"four">>, <<"four">>}
    ]},

    ?assertEqual(Table1, hpack_index:table(Ctx4)),
    ?assertEqual(Table1, hpack_index:table(hpack:resize(Ctx4, 4096))),
    ?assertEqual({0, []}, hpack_index:table(hpack:resize(Ctx4, 0))),

    Table2 = {77, [
        {1, <<"baz">>, <<"boop">>},
        {2, <<"foo">>, <<"bar">>}
    ]},
    ?assertEqual(Table2, hpack_index:table(hpack:resize(Ctx4, 100))),

    Table3 = {39, [
        {1, <<"baz">>, <<"boop">>}
    ]},
    ?assertEqual(Table3, hpack_index:table(hpack:resize(Ctx4, 64))).


index_not_found_test() ->
    ?assertEqual(undefined, hpack_index:lookup(hpack:new(), 70)).


prefer_static_name_index_test() ->
    Ctx = hpack_index:add(hpack:new(), <<"user-agent">>, <<"Firefox">>),
    ?assertEqual(
            {49, [{1, <<"user-agent">>, <<"Firefox">>}]},
            hpack_index:table(Ctx)
        ),
    ?assertEqual(
            {name_indexed, 58},
            hpack_index:match(Ctx, {<<"user-agent">>, <<"Chrome">>})
        ).


match_static_exact_test() ->
    Entries = tuple_to_list(?STATIC_TABLE),
    Ctx = hpack:new(),
    lists:foldl(fun(Entry, Idx) ->
        ?assertEqual({hdr_indexed, Idx}, hpack_index:match(Ctx, Entry)),
        Idx + 1
    end, 1, Entries).


match_static_name_test() ->
    AllEntries = tuple_to_list(?STATIC_TABLE),
    {_, Entries} = lists:foldl(fun({Name, _}, {Idx, EntryAcc}) ->
        case lists:keyfind(Name, 1, EntryAcc) of
            {_, _} -> {Idx + 1, EntryAcc};
            false -> {Idx + 1, [{Name, Idx} | EntryAcc]}
        end
    end, {1, []}, AllEntries),
    Ctx = hpack:new(),
    lists:foreach(fun({HdrName, Idx}) ->
        Entry = {HdrName, undefined},
        ?assertEqual({name_indexed, Idx}, hpack_index:match(Ctx, Entry))
    end, Entries).
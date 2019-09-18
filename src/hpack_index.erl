%% @private

-module(hpack_index).

-export([
    resize/2,
    size/1,
    max_size/1,
    table/1,

    add/3,

    lookup/2,
    match/2
]).


-include("hpack.hrl").
-include("hpack_index.hrl").


-spec resize(context(), pos_integer()) -> context().
resize(#hpack_ctx{} = Ctx0, NewMaxSize) ->
    #hpack_ctx{
        table = Table
    } = Ctx0,
    Ctx1 = Ctx0#hpack_ctx{
        max_size = NewMaxSize
    },
    drop_entries(Ctx1, lists:reverse(Table), 0).


-spec size(context()) -> pos_integer().
size(#hpack_ctx{} = Ctx) ->
    Ctx#hpack_ctx.cur_size.


-spec max_size(context()) -> pos_integer().
max_size(#hpack_ctx{} = Ctx) ->
    Ctx#hpack_ctx.max_size.


-spec table(context()) -> {non_neg_integer(), [any()]}.
table(#hpack_ctx{} = Ctx) ->
    {Ctx#hpack_ctx.cur_size, Ctx#hpack_ctx.table}.


-spec add(context(), header_name(), header_value()) -> context().
add(Ctx0, Name, Value) ->
    #hpack_ctx{
        table = Table,
        max_size = MaxSize
    } = Ctx0,

    EntrySize = entry_size(Name, Value),
    Ctx1 = drop_entries(Ctx0, lists:reverse(Table), EntrySize),
    Ctx2 = relable_entries(Ctx1),
    if EntrySize > MaxSize -> Ctx2; true ->
        add_entry(Ctx2, Name, Value, EntrySize)
    end.


-spec lookup(context(), pos_integer()) -> header() | undefined.
lookup(_Ctx, Idx) when Idx > 0, Idx < ?DYNAMIC_TABLE_MIN_INDEX ->
    element(Idx, ?STATIC_TABLE);

lookup(#hpack_ctx{} = Ctx, Idx) ->
    DynamicIdx = Idx - ?DYNAMIC_TABLE_MIN_INDEX + 1,
    case lists:keyfind(DynamicIdx, 1, Ctx#hpack_ctx.table) of
        {DynamicIdx, Name, Value} -> {Name, Value};
        false -> undefined
    end.


-spec match(context(), {header_name(), header_value()}) -> match_result().
match(#hpack_ctx{} = Ctx, {Name, Value}) ->
    % The order of preference for finding a usable cached
    % header index is:
    %
    % 1. An exact static match
    % 2. An exact dynamic match
    % 3. A name only static match
    % 4. A name only dynamic match
    try
        case static_exact({Name, Value}) of
            I1 when is_integer(I1) ->
                throw({hdr_indexed, I1});
            undefined ->
                ok
        end,

        DynamicNameIdx = lists:foldl(fun({I, N, V}, Acc) ->
            case Name == N andalso Value == V of
                true ->
                    throw({hdr_indexed, I + ?DYNAMIC_TABLE_MIN_INDEX - 1});
                false ->
                    ok
            end,
            case Name == N andalso Acc == undefined of
                true -> I + ?DYNAMIC_TABLE_MIN_INDEX - 1;
                false -> Acc
            end
        end, undefined, Ctx#hpack_ctx.table),

        case {static_name(Name), DynamicNameIdx} of
            {I2, _} when is_integer(I2) -> {name_indexed, I2};
            {_, I2} when is_integer(I2) -> {name_indexed, I2};
            {undefined, undefined} -> not_indexed
        end
    catch
        throw:{hdr_indexed, Idx} ->
            {hdr_indexed, Idx}
    end.


drop_entries(#hpack_ctx{cur_size = 0} = Ctx, [], _EntrySize) ->
    Ctx#hpack_ctx{
        table = []
    };

drop_entries(Ctx, [{_Pos, Name, Value} | RevRest] = RevTable, EntrySize) ->
    #hpack_ctx{
        cur_size = CurSize,
        max_size = MaxSize
    } = Ctx,
    case EntrySize + CurSize =< MaxSize of
        true ->
            Ctx#hpack_ctx{
                table = lists:reverse(RevTable)
            };
        false ->
            RemSize = entry_size(Name, Value),
            NewCtx = Ctx#hpack_ctx{
                cur_size = CurSize - RemSize
            },
            drop_entries(NewCtx, RevRest, EntrySize)
    end.


entry_size(Name, Value) ->
    erlang:size(Name) + erlang:size(Value) + 32.


relable_entries(Ctx) ->
    #hpack_ctx{
        table = Table
    } = Ctx,
    Ctx#hpack_ctx{
        table = [{Idx + 1, Name, Value} || {Idx, Name, Value} <- Table]
    }.


add_entry(Ctx, Name, Value, EntrySize) ->
    #hpack_ctx{
        table = Table,
        cur_size = CurSize
    } = Ctx,
    Ctx#hpack_ctx{
        table = [{1, Name, Value} | Table],
        cur_size = CurSize + EntrySize
    }.


static_exact({<<":authority">>, <<>>}) ->                      1;
static_exact({<<":method">>, <<"GET">>}) ->                    2;
static_exact({<<":method">>, <<"POST">>}) ->                   3;
static_exact({<<":path">>, <<"/">>}) ->                        4;
static_exact({<<":path">>, <<"/index.html">>}) ->              5;
static_exact({<<":scheme">>, <<"http">>}) ->                   6;
static_exact({<<":scheme">>, <<"https">>}) ->                  7;
static_exact({<<":status">>, <<"200">>}) ->                    8;
static_exact({<<":status">>, <<"204">>}) ->                    9;
static_exact({<<":status">>, <<"206">>}) ->                   10;
static_exact({<<":status">>, <<"304">>}) ->                   11;
static_exact({<<":status">>, <<"400">>}) ->                   12;
static_exact({<<":status">>, <<"404">>}) ->                   13;
static_exact({<<":status">>, <<"500">>}) ->                   14;
static_exact({<<"accept-charset">>, <<>>}) ->                 15;
static_exact({<<"accept-encoding">>, <<"gzip, deflate">>}) -> 16;
static_exact({<<"accept-language">>, <<>>}) ->                17;
static_exact({<<"accept-ranges">>, <<>>}) ->                  18;
static_exact({<<"accept">>, <<>>}) ->                         19;
static_exact({<<"access-control-allow-origin">>, <<>>}) ->    20;
static_exact({<<"age">>, <<>>}) ->                            21;
static_exact({<<"allow">>, <<>>}) ->                          22;
static_exact({<<"authorization">>, <<>>}) ->                  23;
static_exact({<<"cache-control">>, <<>>}) ->                  24;
static_exact({<<"content-disposition">>, <<>>}) ->            25;
static_exact({<<"content-encoding">>, <<>>}) ->               26;
static_exact({<<"content-language">>, <<>>}) ->               27;
static_exact({<<"content-length">>, <<>>}) ->                 28;
static_exact({<<"content-location">>, <<>>}) ->               29;
static_exact({<<"content-range">>, <<>>}) ->                  30;
static_exact({<<"content-type">>, <<>>}) ->                   31;
static_exact({<<"cookie">>, <<>>}) ->                         32;
static_exact({<<"date">>, <<>>}) ->                           33;
static_exact({<<"etag">>, <<>>}) ->                           34;
static_exact({<<"expect">>, <<>>}) ->                         35;
static_exact({<<"expires">>, <<>>}) ->                        36;
static_exact({<<"from">>, <<>>}) ->                           37;
static_exact({<<"host">>, <<>>}) ->                           38;
static_exact({<<"if-match">>, <<>>}) ->                       39;
static_exact({<<"if-modified-since">>, <<>>}) ->              40;
static_exact({<<"if-none-match">>, <<>>}) ->                  41;
static_exact({<<"if-range">>, <<>>}) ->                       42;
static_exact({<<"if-unmodified-since">>, <<>>}) ->            43;
static_exact({<<"last-modified">>, <<>>}) ->                  44;
static_exact({<<"link">>, <<>>}) ->                           45;
static_exact({<<"location">>, <<>>}) ->                       46;
static_exact({<<"max-forwards">>, <<>>}) ->                   47;
static_exact({<<"proxy-authenticate">>, <<>>}) ->             48;
static_exact({<<"proxy-authorization">>, <<>>}) ->            49;
static_exact({<<"range">>, <<>>}) ->                          50;
static_exact({<<"referer">>, <<>>}) ->                        51;
static_exact({<<"refresh">>, <<>>}) ->                        52;
static_exact({<<"retry-after">>, <<>>}) ->                    53;
static_exact({<<"server">>, <<>>}) ->                         54;
static_exact({<<"set-cookie">>, <<>>}) ->                     55;
static_exact({<<"strict-transport-security">>, <<>>}) ->      56;
static_exact({<<"transfer-encoding">>, <<>>}) ->              57;
static_exact({<<"user-agent">>, <<>>}) ->                     58;
static_exact({<<"vary">>, <<>>}) ->                           59;
static_exact({<<"via">>, <<>>}) ->                            60;
static_exact({<<"www-authenticate">>, <<>>}) ->               61;
static_exact(_) ->                                            undefined.

static_name(<<":authority">>) ->                       1;
static_name(<<":method">>) ->                          2;
static_name(<<":path">>) ->                            4;
static_name(<<":scheme">>) ->                          6;
static_name(<<":status">>) ->                          8;
static_name(<<"accept-charset">>) ->                  15;
static_name(<<"accept-encoding">>) ->                 16;
static_name(<<"accept-language">>) ->                 17;
static_name(<<"accept-ranges">>) ->                   18;
static_name(<<"accept">>) ->                          19;
static_name(<<"access-control-allow-origin">>) ->     20;
static_name(<<"age">>) ->                             21;
static_name(<<"allow">>) ->                           22;
static_name(<<"authorization">>) ->                   23;
static_name(<<"cache-control">>) ->                   24;
static_name(<<"content-disposition">>) ->             25;
static_name(<<"content-encoding">>) ->                26;
static_name(<<"content-language">>) ->                27;
static_name(<<"content-length">>) ->                  28;
static_name(<<"content-location">>) ->                29;
static_name(<<"content-range">>) ->                   30;
static_name(<<"content-type">>) ->                    31;
static_name(<<"cookie">>) ->                          32;
static_name(<<"date">>) ->                            33;
static_name(<<"etag">>) ->                            34;
static_name(<<"expect">>) ->                          35;
static_name(<<"expires">>) ->                         36;
static_name(<<"from">>) ->                            37;
static_name(<<"host">>) ->                            38;
static_name(<<"if-match">>) ->                        39;
static_name(<<"if-modified-since">>) ->               40;
static_name(<<"if-none-match">>) ->                   41;
static_name(<<"if-range">>) ->                        42;
static_name(<<"if-unmodified-since">>) ->             43;
static_name(<<"last-modified">>) ->                   44;
static_name(<<"link">>) ->                            45;
static_name(<<"location">>) ->                        46;
static_name(<<"max-forwards">>) ->                    47;
static_name(<<"proxy-authenticate">>) ->              48;
static_name(<<"proxy-authorization">>) ->             49;
static_name(<<"range">>) ->                           50;
static_name(<<"referer">>) ->                         51;
static_name(<<"refresh">>) ->                         52;
static_name(<<"retry-after">>) ->                     53;
static_name(<<"server">>) ->                          54;
static_name(<<"set-cookie">>) ->                      55;
static_name(<<"strict-transport-security">>) ->       56;
static_name(<<"transfer-encoding">>) ->               57;
static_name(<<"user-agent">>) ->                      58;
static_name(<<"vary">>) ->                            59;
static_name(<<"via">>) ->                             60;
static_name(<<"www-authenticate">>) ->                61;
static_name(_) ->                                     undefined.

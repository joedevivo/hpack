%% @doc
%% The `hpack' module provides functions for working with HPACK as described in
%% <a href="https://tools.ietf.org/html/rfc7541">RFC 7541</a>.
%%
%% @reference <a href="https://tools.ietf.org/html/rfc7541">RFC 7541</a>

-module(hpack).


-export([
    new/0,
    new/1,

    resize/2,

    decode/2,
    encode/2
]).


-include("hpack.hrl").


-export_type([
    context/0,

    header_name/0,
    header_value/0,
    header_opt/0,
    header/0,
    headers/0,

    encode_error/0,
    decode_error/0
]).


%% @equiv new(4096)
-spec new() -> context().
new() ->
    #hpack_ctx{}.


%% @doc
%% Returns a new HPACK context with the given `ConnMaxTableSize'
-spec new(non_neg_integer()) -> context().
new(ConnMaxTableSize) ->
    #hpack_ctx{
        max_size = min(ConnMaxTableSize, 4096),
        conn_max_size = ConnMaxTableSize
    }.


%% @doc
%% Updates the max table size of the given HPACK
%% context to the given `NewSize'. `NewSize` must
%% not exceed the size specified when creating
%% the context.
-spec resize(context(), non_neg_integer()) -> context().
resize(#hpack_ctx{} = Ctx, NewSize) when is_integer(NewSize) ->
    #hpack_ctx{
        max_size = OldSize,
        conn_max_size = ConnMaxSize
    } = Ctx,
    if NewSize =< ConnMaxSize -> ok; true ->
        ?ERROR({invalid_table_size, NewSize})
    end,
    case NewSize > OldSize of
        true -> Ctx#hpack_ctx{max_size = NewSize};
        false -> hpack_index:resize(Ctx, NewSize)
    end.


%% @doc
%% Encodes the given `Headers' using the given `Ctx'.
%%
%% When successful, returns a `{ok, NewContext, EncodedHeaders}'
%% tuple where `EncodedHeaders' is a binary representing the
%% encoded headers and `NewContext' is the new HPACK context.
%%
%% For example:
%% ```
%% Headers = [{<<":method">>, <<"GET">>}],
%% {ok, NewCtx EncodedHeaders} = hpack:encode(hpack:new(), Headers).
%% '''
-spec encode(context(), headers()) ->
        {ok, context(), binary()} |
        {error, encode_error()}.
encode(#hpack_ctx{} = Ctx, Headers) when is_list(Headers) ->
    try
        encode(Ctx, Headers, [])
    catch throw:{hpack_error, Error} ->
        {error, Error}
    end.


%% @doc
%% Decodes the provided binary `Bin` using the HPACK
%% context `Ctx`.
%%
%% If successful, returns `{ok, NewCtx, Headers}'
%%
%% For example:
%% ```
%% {ok, NewCtx, Headers} = hpack:decode(OldCtx, Binary)
%% '''
-spec decode(context(), binary()) ->
        {ok, {headers(), context()}} |
        {error, decode_error()}.
decode(#hpack_ctx{} = Ctx, Bin) when is_binary(Bin) ->
    try
        decode(Ctx, Bin, [])
    catch throw:{hpack_error, Error} ->
        {error, Error}
    end.


encode(Ctx, [], Acc) ->
    {ok, Ctx, iolist_to_binary(lists:reverse(Acc))};

encode(Ctx, [{Name, Value} | Tail], Acc) ->
    encode(Ctx, [{Name, Value, []} | Tail], Acc);

encode(Ctx, [{Name, Value, Opts} | Tail], Acc)
        when is_binary(Name), is_binary(Value), is_list(Opts) ->
    NeverIndex = lists:member(never_index, Opts),
    NoIndex = lists:member(no_index, Opts),
    NoNameIndex = lists:member(no_name_index, Opts),

    {NewCtx, Encoded} = if
        NeverIndex and NoNameIndex ->
            {Ctx, encode_never_index(Name, Value, Opts)};
        NeverIndex ->
            {Ctx, encode_never_index(name_index(Ctx, Name), Value, Opts)};
        NoIndex and NoNameIndex ->
            {Ctx, encode_no_index(Name, Value, Opts)};
        NoIndex ->
            {Ctx, encode_no_index(name_index(Ctx, Name), Value, Opts)};
        true ->
            case hpack_index:match(Ctx, {Name, Value}) of
                {hdr_indexed, Idx} ->
                    {Ctx, encode_indexed(Idx)};
                {name_indexed, Idx} ->
                    {
                        hpack_index:add(Ctx, Name, Value),
                        encode_indexed(Idx, Value, Opts)
                    };
                not_indexed ->
                    {
                        hpack_index:add(Ctx, Name, Value),
                        encode_indexed(Name, Value, Opts)
                    }
            end
    end,

    encode(NewCtx, Tail, [Encoded | Acc]);

encode(_Ctx, [InvalidHeader | _], _Acc) ->
    ?ERROR({invalid_header, InvalidHeader}).


encode_never_index(Idx, Value, Opts) when is_integer(Idx) ->
    Prefix = <<2#0001:4>>,
    IdxBin = hpack_integer:encode(Idx, 4),
    ValueBin = hpack_string:encode(Value, Opts),
    <<Prefix/bits, IdxBin/bits, ValueBin/bits>>;

encode_never_index(Name, Value, Opts) when is_binary(Name) ->
    Prefix = <<2#0001:4, 0:4>>,
    NameBin = hpack_string:encode(Name, Opts),
    ValueBin = hpack_string:encode(Value, Opts),
    <<Prefix/bits, NameBin/bits, ValueBin/bits>>.


encode_no_index(Idx, Value, Opts) when is_integer(Idx) ->
    Prefix = <<2#0000:4>>,
    IdxBin = hpack_integer:encode(Idx, 4),
    ValueBin = hpack_string:encode(Value, Opts),
    <<Prefix/bits, IdxBin/bits, ValueBin/bits>>;

encode_no_index(Name, Value, Opts) when is_binary(Name) ->
    Prefix = <<2#0000:4, 0:4>>,
    NameBin = hpack_string:encode(Name, Opts),
    ValueBin = hpack_string:encode(Value, Opts),
    <<Prefix/bits, NameBin/bits, ValueBin/bits>>.


encode_indexed(Idx) ->
    IdxBin = hpack_integer:encode(Idx, 7),
    <<2#1:1, IdxBin/bits>>.


encode_indexed(Idx, Value, Opts) when is_integer(Idx) ->
    Prefix = <<2#01:2>>,
    IdxBin = hpack_integer:encode(Idx, 6),
    ValueBin = hpack_string:encode(Value, Opts),
    <<Prefix/bits, IdxBin/bits, ValueBin/bits>>;

encode_indexed(Name, Value, Opts) when is_binary(Name) ->
    Prefix = <<2#01:2, 2#000000:6>>,
    NameBin = hpack_string:encode(Name, Opts),
    ValueBin = hpack_string:encode(Value, Opts),
    <<Prefix/bits, NameBin/bits, ValueBin/bits>>.


name_index(Ctx, Name) when is_binary(Name) ->
    case hpack_index:match(Ctx, {Name, undefined}) of
        {_, Idx} when is_integer(Idx) -> Idx;
        not_indexed -> Name
    end.


decode(Ctx, <<>>, Acc) ->
    {ok, Ctx, lists:reverse(Acc)};

decode(Ctx, <<2#1:1, _/bits>> = Bin, Acc) ->
    decode_indexed(Ctx, Bin, Acc);

decode(Ctx, <<2#01:2, _/bits>> = Bin, Acc) ->
    decode_and_index(Ctx, Bin, Acc);

decode(Ctx, <<2#0000:4, _/bits>> = Bin, Acc) ->
    decode_no_index(Ctx, Bin, Acc);

decode(Ctx, <<2#0001:4, _/bits>> = Bin, Acc) ->
    decode_never_index(Ctx, Bin, Acc);

decode(Ctx, <<2#001:3, _/bits>> = Bin, Acc) ->
    decode_size_update(Ctx, Bin, Acc).


decode_indexed(_Ctx, <<2#1:1, 2#0000000:7, _/binary>>, _Acc) ->
    ?ERROR({invalid_index, 0});

decode_indexed(Ctx, <<2#1:1, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 7),
    Header = case hpack_index:lookup(Ctx, Idx) of
        {N, V} -> {N, V, []};
        undefined -> ?ERROR({unknown_index, Idx})
    end,
    decode(Ctx, B2, [Header | Acc]).


decode_and_index(Ctx, <<2#01:2, 2#000000:6, B1/bits>>, Acc) ->
    {Name, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:any_uncompressed([B1, B2]) of
        true -> {Name, Value, [uncompressed]};
        false -> {Name, Value, []}
    end,
    decode(hpack_index:add(Ctx, Name, Value), B3, [Header | Acc]);

decode_and_index(Ctx, <<2#01:2, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 6),
    Name = case hpack_index:lookup(Ctx, Idx) of
        {N, _} -> N;
        undefined -> ?ERROR({unknown_index, Idx})
    end,
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:is_uncompressed(B2) of
        true -> {Name, Value, [uncompressed]};
        false -> {Name, Value, []}
    end,
    decode(hpack_index:add(Ctx, Name, Value), B3, [Header | Acc]).


decode_no_index(Ctx, <<2#0000:4, 2#0000:4, B1/bits>>, Acc) ->
    {Name, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:any_uncompressed([B1, B2]) of
        true -> {Name, Value, [uncompressed, no_index, no_name_index]};
        false -> {Name, Value, [no_index, no_name_index]}
    end,
    decode(Ctx, B3, [Header | Acc]);

decode_no_index(Ctx, <<2#0000:4, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 4),
    Name = case hpack_index:lookup(Ctx, Idx) of
        {N, _} -> N;
        undefined -> ?ERROR({unknown_index, Idx})
    end,
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:is_uncompressed(B2) of
        true -> {Name, Value, [uncompressed, no_index]};
        false -> {Name, Value, [no_index]}
    end,
    decode(Ctx, B3, [Header | Acc]).


decode_never_index(Ctx, <<2#0001:4, 2#0000:4, B1/bits>>, Acc) ->
    {Name, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:any_uncompressed([B1, B2]) of
        true -> {Name, Value, [uncompressed, never_index, no_name_index]};
        false -> {Name, Value, [never_index, no_name_index]}
    end,
    decode(Ctx, B3, [Header | Acc]);

decode_never_index(Ctx, <<2#0001:4, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 4),
    Name = case hpack_index:lookup(Ctx, Idx) of
        {N, _} -> N;
        undefined -> ?ERROR({unknown_index, Idx})
    end,
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:is_uncompressed(B2) of
        true -> {Name, Value, [uncompressed, never_index]};
        false -> {Name, Value, [never_index]}
    end,
    decode(Ctx, B3, [Header | Acc]).


% TODO: Test whether resize has to precede all headers
decode_size_update(Ctx, <<2#001:3, B1/bits>>, []) ->
    {NewSize, B2} = hpack_integer:decode(B1, 5),
    decode(resize(Ctx, NewSize), B2, []);

decode_size_update(_Ctx, _Bin, Acc) when length(Acc) > 0 ->
    ?ERROR({invalid_size_update, headers_received}).

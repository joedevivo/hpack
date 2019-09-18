%% @doc
%% The `hpack' module provides functions for working with HPACK as described in
%% <a href="https://tools.ietf.org/html/rfc7541">RFC 7541</a>.
%%
%% @reference <a href="https://tools.ietf.org/html/rfc7541">RFC 7541</a>

-module(hpack_debug).


-export([
    explain/2
]).


-include("hpack.hrl").


-define(DONE(Last, Acc), ?ERROR({error, lists:reverse(Acc, [Last])})).


%% @doc
%% Explain the decoding of a given HPack binary representation
%%
%% If successful, returns `[{bitstring(), any()}]' listing the
%% steps decoding takes.
%%
%% For example:
%% ```
%% {Headers, NewContext} = hpack:decode(Binary, OldContext).
%% '''
-spec explain(hpack:context(), binary()) -> [{bitstring(), any()}].
explain(#hpack_ctx{} = Ctx, Bin) when is_binary(Bin) ->
    try
        explain(Ctx, Bin, [])
    catch throw:{hpack_error, Error} ->
        Error
    end.


explain(Ctx, <<>>, Acc) ->
    {ok, Ctx, lists:reverse(Acc)};

explain(Ctx, <<2#1:1, _/bits>> = Bin, Acc) ->
    explain_indexed(Ctx, Bin, [{<<2#1:1>>, indexed_header} | Acc]);

explain(Ctx, <<2#01:2, _/bits>> = Bin, Acc) ->
    explain_and_index(Ctx, Bin, [{<<2#01:2>>, incremental_index} | Acc]);

explain(Ctx, <<2#0000:4, _/bits>> = Bin, Acc) ->
    explain_no_index(Ctx, Bin, [{<<2#0000:4>>, no_index} | Acc]);

explain(Ctx, <<2#0001:4, _/bits>> = Bin, Acc) ->
    explain_never_index(Ctx, Bin, [{<<2#0001:4>>, never_index} | Acc]);

explain(Ctx, <<2#001:3, _/bits>> = Bin, Acc) ->
    explain_size_update(Ctx, Bin, [{<<2#001:3>>, size_update} | Acc]).


explain_indexed(_Ctx, <<2#1:1, 2#0000000:7, _/binary>>, Acc) ->
    ?DONE({invalid_index, 0}, Acc);

explain_indexed(Ctx, <<2#1:1, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 7),
    Header = case hpack_index:lookup(Ctx, Idx) of
        undefined ->
            ?DONE({unknown_index, Idx}, Acc);
        Else ->
            Else
    end,
    explain(Ctx, B2, [Header | Acc]).


explain_and_index(Ctx, <<2#01:2, 2#000000:6, B1/bits>>, Acc) ->
    {Name, NameInfo, B2} = explain_string(B1),
    {Value, ValueInfo, B3} = explain_string(B2),

    NameBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, {string, ValueInfo, Value}},
        {NameBits, {string, NameInfo, Name}},
        {<<2#000000:6>>, unindexed_name}
    ] ++ Acc,

    explain(hpack_index:add(Ctx, Name, Value), B3, NewAcc);

explain_and_index(Ctx, <<2#01:2, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 6),
    {Name, _} = case hpack_index:lookup(Ctx, Idx) of
        undefined ->
            ?DONE({unknown_index, Idx}, Acc);
        Else ->
            Else
    end,
    {Value, ValueInfo, B3} = explain_string(B2),

    IdxBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, {string, ValueInfo, Value}},
        {IdxBits, {indexed_name, Idx, Name}}
    ] ++ Acc,

    explain(hpack_index:add(Ctx, Name, Value), B3, NewAcc).


explain_no_index(Ctx, <<2#0000:4, 2#0000:4, B1/bits>>, Acc) ->
    {Name, NameInfo, B2} = explain_string(B1),
    {Value, ValueInfo, B3} = explain_string(B2),

    NameBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, {string, ValueInfo, Value}},
        {NameBits, {string, NameInfo, Name}},
        {<<2#0000:4>>, unindexed_name}
    ] ++ Acc,

    explain(Ctx, B3, NewAcc);

explain_no_index(Ctx, <<2#0000:4, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 4),
    {Name, _} = case hpack_index:lookup(Ctx, Idx) of
        undefined ->
            ?DONE({unknown_index, Idx}, Acc);
        Else ->
            Else
    end,
    {Value, ValueInfo, B3} = explain_string(B2),

    IdxBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, {string, ValueInfo, Value}},
        {IdxBits, {indexed_name, Idx, Name}}
    ] ++ Acc,

    explain(Ctx, B3, NewAcc).


explain_never_index(Ctx, <<2#0001:4, 2#0000:4, B1/bits>>, Acc) ->
    {Name, NameInfo, B2} = explain_string(B1),
    {Value, ValueInfo, B3} = explain_string(B2),

    NameBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, {string, ValueInfo, Value}},
        {NameBits, {string, NameInfo, Name}},
        {<<2#0000:4>>, unindexed_name}
    ] ++ Acc,

    explain(Ctx, B3, NewAcc);

explain_never_index(Ctx, <<2#0001:4, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 4),
    {Name, _} = case hpack_index:lookup(Ctx, Idx) of
        undefined -> ?DONE({unknown_index, Idx}, Acc);
        Else -> Else
    end,
    {Value, ValueInfo, B3} = explain_string(B2),

    IdxBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, {string, ValueInfo, Value}},
        {IdxBits, {indexed_name, Idx, Name}}
    ] ++ Acc,

    explain(Ctx, B3, NewAcc).


% TODO: Test whether resize has to precede all headers
explain_size_update(Ctx, <<2#001:3, B1/bits>>, Acc) ->
    lists:foreach(fun
        ({_, size_update}) ->
            ok;
        ({_, {new_size, _}}) ->
            ok;
        (_) ->
            ?DONE({invalid_size_update, headers_received}, Acc)
    end, Acc),
    {NewSize, B2} = hpack_integer:decode(B1, 5),
    UpdateBits = hdbinary(B1, B2),
    NewAcc = [{UpdateBits, {new_size, NewSize}} | Acc],
    explain(hpack:resize(Ctx, NewSize), B2, NewAcc).


explain_string(<<Huff:1, B1/bits>>) ->
    {Length, B2} = hpack_integer:decode(B1, 7),
    <<Data:Length/binary, B3/bits>> = B2,
    LenBits = hdbinary(B1, B2),
    {Info, Value} = case Huff of
        0 -> {{LenBits, Length, uncompressed}, Data};
        1 -> {{LenBits, Length, compressed}, hpack_huffman:decode(Data)}
    end,
    {Value, Info, B3}.


hdbinary(B1, B2) when bit_size(B1) > bit_size(B2) ->
    Len = bit_size(B1) - bit_size(B2),
    <<H:Len/bits, _/bits>> = B1,
    H.

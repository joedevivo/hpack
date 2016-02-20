-module(hpack).

%% @doc the hpack module should be the only API access point you need
%% for the encoding and decoding of header blocks

%% API Exports
-export([
         new_decode_context/0,
         new_encode_context/0,
         decode/2,
         encode/2,
         encode/3
        ]).

%% Datatypes for the real world:
-record(decode_context, {
        dynamic_table = hpack_index:new()
    }).
-type decode_context() :: #decode_context{}.

-record(encode_context, {
        dynamic_table = hpack_index:new()
    }).
-type encode_context() :: #encode_context{}.
-type encode_matcher() :: fun((header(), hpack_index:dynamic_table()) -> {atom(), pos_integer()|undefined}).

-export_type([decode_context/0,encode_context/0]).

-type header_name() :: binary().
-type header_value() :: binary().
-type header() :: {header_name(), header_value()}.
-type headers() :: [header()].

-export_type([
              header_name/0,
              header_value/0,
              header/0,
              headers/0
             ]).

-spec new_encode_context() -> encode_context().
new_encode_context() -> #encode_context{}.

-spec new_decode_context() -> decode_context().
new_decode_context() -> #decode_context{}.

-spec encode([{binary(), binary()}], encode_context()) -> {binary(), encode_context()}.
encode(Headers, Context) ->
    encode(Headers, <<>>, Context, fun hpack_index:match/2).

-spec encode([{binary(), binary()}], encode_context(), encode_matcher()) -> {binary(), encode_context()}.
encode(Headers, Context, Matcher) ->
    encode(Headers, <<>>, Context, Matcher).

-spec decode(binary(), decode_context()) -> {headers(), decode_context()}.
decode(Bin, Context) ->
    decode(Bin, [], Context).

%% Private Functions

-spec decode(binary(), headers(), decode_context()) -> {headers(), decode_context()}.
%% We're done decoding, return headers
decode(<<>>, HeadersAcc, C) ->
    {HeadersAcc, C};
%% First bit is '1', so it's an 'Indexed Header Feild'
%% http://http2.github.io/http2-spec/compression.html#rfc.section.6.1
decode(<<2#1:1,_/bits>>=B, HeaderAcc, Context) ->
    decode_indexed_header(B, HeaderAcc, Context);
%% First two bits are '01' so it's a 'Literal Header Field with Incremental Indexing'
%% http://http2.github.io/http2-spec/compression.html#rfc.section.6.2.1
decode(<<2#01:2,_/bits>>=B, HeaderAcc, Context) ->
    decode_literal_header_with_indexing(B, HeaderAcc, Context);

%% First four bits are '0000' so it's a 'Literal Header Field without Indexing'
%% http://http2.github.io/http2-spec/compression.html#rfc.section.6.2.2
decode(<<2#0000:4,_/bits>>=B, HeaderAcc, Context) ->
    decode_literal_header_without_indexing(B, HeaderAcc, Context);

%% First four bits are '0001' so it's a 'Literal Header Field never Indexed'
%% http://http2.github.io/http2-spec/compression.html#rfc.section.6.2.3
decode(<<2#0001:4,_/bits>>=B, HeaderAcc, Context) ->
    decode_literal_header_never_indexed(B, HeaderAcc, Context);

%% First three bits are '001' so it's a 'Dynamic Table Size Update'
%% http://http2.github.io/http2-spec/compression.html#rfc.section.6.3
decode(<<2#001:3,_/bits>>=B, HeaderAcc, Context) ->
    %% TODO: This will be annoying because it means passing the HTTP setting
    %% for maximum table size around this entire funtion set
    decode_dynamic_table_size_update(B, HeaderAcc, Context);

%% Oops!
decode(<<B:1,_/binary>>, _HeaderAcc, _Context) ->
    lager:debug("Bad header packet ~p", [B]),
    error.

decode_indexed_header(<<2#1:1,2#1111111:7,B1/bits>>,
                      Acc,
                      Context = #decode_context{dynamic_table=T}) ->
    {Index, B2} = hpack_integer:decode(B1, 7),
    decode(B2, Acc ++ [hpack_index:lookup(Index, T)], Context);
decode_indexed_header(<<2#1:1,Index:7,B1/bits>>,
                      Acc,
                      Context = #decode_context{dynamic_table=T}) ->
    decode(B1, Acc ++ [hpack_index:lookup(Index, T)], Context).

%% The case where the field isn't indexed yet, but should be.
decode_literal_header_with_indexing(<<2#01:2,2#000000:6,B1/bits>>, Acc,
    Context = #decode_context{dynamic_table=T}) ->
    {Str, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    decode(B3,
           Acc ++ [{Str, Value}],
           Context#decode_context{dynamic_table=hpack_index:add(Str, Value, T)});
%% This is the case when the index is greater than 0, 0 being not yet
%% indexed
decode_literal_header_with_indexing(<<2#01:2,B1/bits>>, Acc,
    Context = #decode_context{dynamic_table=T}) ->
    {Index, Rem} = hpack_integer:decode(B1,6),
    {Str, B2} = hpack_string:decode(Rem),
    {Name,_} = case hpack_index:lookup(Index, T) of
        undefined ->
            lager:error("Tried to find ~p in ~p", [Index, T]),
            throw(undefined);
        {N, V} -> {N, V}
    end,
    decode(B2,
           Acc ++ [{Name, Str}],
           Context#decode_context{dynamic_table=hpack_index:add(Name, Str, T)}).

decode_literal_header_without_indexing(<<2#0000:4,2#1111:4,B1/bits>>, Acc,
    Context = #decode_context{dynamic_table=T}) ->
    {Index, Rem} = hpack_integer:decode(B1,4),
    {Str, B2} = hpack_string:decode(Rem),
    {Name,_}= hpack_index:lookup(Index, T),
    decode(B2, Acc ++ [{Name, Str}], Context);
decode_literal_header_without_indexing(<<2#0000:4,2#0000:4,B1/bits>>, Acc,
    Context) ->
    {Str, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    decode(B3, Acc ++ [{Str, Value}], Context);
decode_literal_header_without_indexing(<<2#0000:4,Index:4,B1/bits>>, Acc,
    Context = #decode_context{dynamic_table=T}) ->
    {Str, B2} = hpack_string:decode(B1),
    {Name,_}= hpack_index:lookup(Index, T),
    decode(B2, Acc ++ [{Name, Str}], Context).

decode_literal_header_never_indexed(<<2#0001:4,2#1111:4,B1/bits>>, Acc,
    Context = #decode_context{dynamic_table=T}) ->
    {Index, Rem} = hpack_integer:decode(B1,4),
    {Str, B2} = hpack_string:decode(Rem),
    {Name,_}= hpack_index:lookup(Index, T),
    decode(B2, Acc ++ [{Name, Str}], Context);
decode_literal_header_never_indexed(<<2#0001:4,2#0000:4,B1/bits>>, Acc,
    Context) ->
    {Str, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    decode(B3, Acc ++ [{Str, Value}], Context);
decode_literal_header_never_indexed(<<2#0001:4,Index:4,B1/bits>>, Acc,
    Context = #decode_context{dynamic_table=T}) ->
    {Str, B2} = hpack_string:decode(B1),
    {Name,_}= hpack_index:lookup(Index, T),
    decode(B2, Acc ++ [{Name, Str}], Context).

decode_dynamic_table_size_update(<<2#001:3,2#11111:5,Bin/binary>>, Acc,
    Context = #decode_context{dynamic_table=T}) ->
    {NewSize, Rem} = hpack_integer:decode(Bin,5),
    decode(Rem, Acc, Context#decode_context{dynamic_table=hpack_index:resize(NewSize, T)});
decode_dynamic_table_size_update(<<2#001:3,NewSize:5,Bin/binary>>, Acc,
    Context = #decode_context{dynamic_table=T}) ->
    decode(Bin, Acc, Context#decode_context{dynamic_table=hpack_index:resize(NewSize, T)}).

-spec encode([{binary(), binary()}], binary(), encode_context(), encode_matcher()) -> {binary(), encode_context()}.
encode([], Acc, Context, _) ->
    {Acc, Context};
encode([{HeaderName, HeaderValue}|Tail], B, Context = #encode_context{dynamic_table=T},
       Matcher) ->
    {BinToAdd, NewContext} = case Matcher({HeaderName, HeaderValue}, T) of
        {indexed, I} ->
            {encode_indexed(I), Context};
        {literal_with_indexing, I} ->
            {encode_literal_indexed(I, HeaderValue),
             Context#encode_context{dynamic_table=hpack_index:add(HeaderName, HeaderValue, T)}};
        {literal_wo_indexing, _X} ->
            {encode_literal_wo_index(HeaderName, HeaderValue),
             Context#encode_context{dynamic_table=hpack_index:add(HeaderName, HeaderValue, T)}}
    end,
    NewB = <<B/binary,BinToAdd/binary>>,
    encode(Tail, NewB, NewContext, Matcher).

encode_indexed(I) when I < 63 ->
    <<2#1:1,I:7>>;
encode_indexed(I) ->
    Encoded = hpack_integer:encode(I, 7),
    <<2#1:1, Encoded/bits>>.

encode_literal_indexed(I, Value) when I < 63 ->
    BinToAdd = encode_literal(Value),
    <<2#01:2,I:6,BinToAdd/binary>>;
encode_literal_indexed(I, Value) ->
    Index = hpack_integer:encode(I, 6),
    BinToAdd = encode_literal(Value),
    <<2#01:2,Index/bits,BinToAdd/binary>>.

encode_literal(Value) ->
    L = hpack_integer:encode(size(Value),7),
    <<2#0:1,L/bits,Value/binary>>.

encode_literal_wo_index(Name, Value) ->
    EncName = encode_literal(Name),
    EncValue = encode_literal(Value),
    <<2#01000000,EncName/binary,EncValue/binary>>.

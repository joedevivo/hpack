%% @private

-module(hpack_integer).

%% @doc For encoding/decoding HPACK's primitive integer representation
%% as defined http://tools.ietf.org/html/rfc7541#section-5.1

-export([
    encode/2,
    decode/2
]).


-spec encode(non_neg_integer(), pos_integer()) -> bitstring().
encode(  1, 1) -> <<  1:1, 0:8>>;
encode(  3, 2) -> <<  3:2, 0:8>>;
encode(  7, 3) -> <<  7:3, 0:8>>;
encode( 15, 4) -> << 15:4, 0:8>>;
encode( 31, 5) -> << 31:5, 0:8>>;
encode( 63, 6) -> << 63:6, 0:8>>;
encode(127, 7) -> <<127:7, 0:8>>;
encode(255, 8) -> <<255:8, 0:8>>;

encode(Int, N) when is_integer(Int), is_integer(N), Int < (1 bsl N - 1) ->
    <<Int:N>>;

encode(Int, N) when is_integer(Int), is_integer(N) ->
    Prefix = 1 bsl N - 1,
    Remaining = Int - Prefix,
    encode_int(Remaining, [<<Prefix:N>>]).

-spec encode_int(non_neg_integer(), [bitstring()]) -> bitstring().
encode_int(Int, Acc) when is_integer(Int), is_list(Acc) ->
    ThisByte = (Int rem 128),
    RestInt = Int bsr 7,
    case RestInt of
        0 ->
            list_to_bitstring(lists:reverse(Acc, [<<ThisByte:8>>]));
        _ ->
            encode_int(RestInt, [<<1:1, ThisByte:7>> | Acc])
    end.


-spec decode(bitstring(), pos_integer()) -> {non_neg_integer(), binary()}.
decode(<<  1:1, Bin/bits>>, 1) -> decode(Bin, 1,   1);
decode(<<  3:2, Bin/bits>>, 2) -> decode(Bin, 1,   3);
decode(<<  7:3, Bin/bits>>, 3) -> decode(Bin, 1,   7);
decode(<< 15:4, Bin/bits>>, 4) -> decode(Bin, 1,  15);
decode(<< 31:5, Bin/bits>>, 5) -> decode(Bin, 1,  31);
decode(<< 63:6, Bin/bits>>, 6) -> decode(Bin, 1,  63);
decode(<<127:7, Bin/bits>>, 7) -> decode(Bin, 1, 127);
decode(<<255:8, Bin/bits>>, 8) -> decode(Bin, 1, 255);
decode(Bin, Prefix) ->
    %% This clause means we have something small
    %% enough to fit in prefix
    <<Value:Prefix, Rem/bits>> = Bin,
    {Value, Rem}.


decode(<<1:1, Int:7, Rest/binary>>, Factor, Value) ->
    decode(Rest, Factor bsl 7, Value + (Int * Factor));

decode(<<0:1, Int:7, Rest/binary>>, Factor, Value) ->
    {Value + (Int * Factor), Rest}.

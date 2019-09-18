
-module(hpack_tutil).


-export([
    headers_equal/2,

    random_int/0,
    random_int/1,

    random_bin/0,
    random_bin/1,

    dehex/1
]).


headers_equal([], []) ->
    true;
headers_equal([{N, V} | RestL], [{N, V} | RestR]) ->
    headers_equal(RestL, RestR);
headers_equal([{N, V, _O} | RestL], RestR) ->
    headers_equal([{N, V} | RestL], RestR);
headers_equal(RestL, [{N, V, _P} | RestR]) ->
    headers_equal(RestL, [{N, V} | RestR]);
headers_equal([H1 | _], [H2 | _]) ->
    erlang:error({not_equal, H1, H2}).


random_int() ->
    random_int(16#FFFFFFFF).


random_int(Range) ->
    rand:uniform(Range).


random_bin() ->
    random_bin(1024).


random_bin(Size) ->
    Bytes = [
        random_int(256) - 1
        || _ <- lists:seq(1, Size)
    ],
    list_to_binary(Bytes).


dehex(Binary) ->
    dehex(strip(Binary, []), []).


dehex(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));

dehex(<<Hi:8, Lo:8, Rest/binary>>, Acc) ->
    HN = nibble(Hi),
    LN = nibble(Lo),
    dehex(Rest, [<<HN:4, LN:4>> | Acc]).


strip(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));

strip(<<" ", Rest/binary>>, Acc) ->
    strip(Rest, Acc);

strip(<<"\n", Rest/binary>>, Acc) ->
    strip(Rest, Acc);

strip(<<Byte:8, Rest/binary>>, Acc) ->
    strip(Rest, [Byte | Acc]).


nibble($0) ->  0;
nibble($1) ->  1;
nibble($2) ->  2;
nibble($3) ->  3;
nibble($4) ->  4;
nibble($5) ->  5;
nibble($6) ->  6;
nibble($7) ->  7;
nibble($8) ->  8;
nibble($9) ->  9;
nibble($a) -> 10;
nibble($A) -> 10;
nibble($b) -> 11;
nibble($B) -> 11;
nibble($c) -> 12;
nibble($C) -> 12;
nibble($d) -> 13;
nibble($D) -> 13;
nibble($e) -> 14;
nibble($E) -> 14;
nibble($f) -> 15;
nibble($F) -> 15.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


headers_not_equal_test() ->
    ?assertError({not_equal, _, _}, headers_equal([{a, b}], [{a, c}])),
    ?assertError({not_equal, _, _}, headers_equal([{a, b}], [{c, d}])),
    ?assertError({not_equal, _, _}, headers_equal([{a, b}], [{c, b}])).


random_test() ->
    ?assert(is_integer(random_int())),
    ?assert(is_integer(random_int(10))),
    ?assert(random_int(10) =< 10),
    ?assert(is_binary(random_bin())),
    ?assert(is_binary(random_bin(10))),
    ?assert(size(random_bin(10)) == 10).


hex_test() ->
    lists:foreach(fun(Val) ->
        Bin = list_to_binary(io_lib:format("~2.16.0B", [Val])),
        ?assertEqual(<<Val:8>>, dehex(Bin))
    end, lists:seq(0, 255)).

-endif.

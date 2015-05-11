%% Erlang support for LLSN - Allyst's data interchange format.
%% LLSN specification http://allyst.org/opensource/llsn/
%%
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Library General Public License for more details.
%%
%% Full license: https://github.com/allyst/go-llsn/blob/master/LICENSE
%%
%% copyright (C) 2014 Allyst Inc. http://allyst.com
%% author Taras Halturin <halturin@allyst.com>

-module(base_tests).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("common/include/log.hrl").

-import(lists, [seq/2]).

-define(TESTS_CNT, 100).

negative_numbers_test_() ->
    Limit7  = erlang:trunc(math:pow(2, 7)),
    Limit14 = erlang:trunc(math:pow(2, 14)),
    Limit21 = erlang:trunc(math:pow(2, 21)),
    Limit28 = erlang:trunc(math:pow(2, 28)),
    Limit35 = erlang:trunc(math:pow(2, 35)),
    Limit42 = erlang:trunc(math:pow(2, 42)),
    Limit49 = erlang:trunc(math:pow(2, 49)),
    Limit56 = erlang:trunc(math:pow(2, 56)),
    Limit64 = erlang:trunc(math:pow(2, 64)),
    [
      {"7 bits 2", {inparallel, 10, negative_lazy_gen({-Limit7, -64}, 2)}},
      {"7 bits 1", {inparallel, 10, negative_lazy_gen({-63, 0}, 1)}},

      {"14 bits 3", {inparallel, 10, negative_lazy_gen({-Limit14, -(Limit14-?TESTS_CNT)}, 3)}},
      {"14 bits 2", {inparallel, 10, negative_lazy_gen({-(Limit7+?TESTS_CNT), -Limit7}, 2)}},

      {"21 bits 4", {inparallel, 10, negative_lazy_gen({-Limit21, -(Limit21-?TESTS_CNT)}, 4)}},
      {"21 bits 3", {inparallel, 10, negative_lazy_gen({-(Limit14+?TESTS_CNT), -Limit14}, 3)}},

      {"28 bits 5", {inparallel, 10, negative_lazy_gen({-Limit28, -(Limit28-?TESTS_CNT)}, 5)}},
      {"28 bits 4", {inparallel, 10, negative_lazy_gen({-(Limit21+?TESTS_CNT), -Limit21}, 4)}},

      {"35 bits 6", {inparallel, 10, negative_lazy_gen({-Limit35, -(Limit35-?TESTS_CNT)}, 6)}},
      {"35 bits 5", {inparallel, 10, negative_lazy_gen({-(Limit28+?TESTS_CNT), -Limit28}, 5)}},

      {"42 bits 7", {inparallel, 10, negative_lazy_gen({-Limit42, -(Limit42-?TESTS_CNT)}, 7)}},
      {"42 bits 6", {inparallel, 10, negative_lazy_gen({-(Limit35+?TESTS_CNT), -Limit35}, 6)}},

      {"49 bits 8", {inparallel, 10, negative_lazy_gen({-Limit49, -(Limit49-?TESTS_CNT)}, 8)}},
      {"49 bits 7", {inparallel, 10, negative_lazy_gen({-(Limit42+?TESTS_CNT), -Limit42}, 7)}},

      {"56 bits 9", {inparallel, 10, negative_lazy_gen({-Limit56, -(Limit56-?TESTS_CNT)}, 9)}},
      {"56 bits 8", {inparallel, 10, negative_lazy_gen({-(Limit49+?TESTS_CNT), -Limit49}, 8)}},

      {"64 bits 9", {inparallel, 10, negative_lazy_gen({-Limit64, -(Limit64-?TESTS_CNT)}, 9)}},
      {"64 bits 9", {inparallel, 10, negative_lazy_gen({-(Limit56+?TESTS_CNT), -Limit56}, 9)}},

      []

    ].

negative_lazy_gen({Min, Max}, Len) ->
   {generator,
    fun () ->
        if Min =< Max ->
            [ {"check: " ++ integer_to_list(Min), ?_test(
               begin
                  {BinNum, Len} = llsn:encode_NUMBER(Min),
                  {DecNum, _}   = llsn:decode_NUMBER(BinNum),
                  Min =:= DecNum
               end
              )} | negative_lazy_gen({Min + 1, Max}, Len)
            ];
          true ->
            []
        end
    end}.

positive_numbers_test_() ->
    Limit7  = erlang:trunc(math:pow(2, 7)),
    Limit14 = erlang:trunc(math:pow(2, 14)),
    Limit21 = erlang:trunc(math:pow(2, 21)),
    Limit28 = erlang:trunc(math:pow(2, 28)),
    Limit35 = erlang:trunc(math:pow(2, 35)),
    Limit42 = erlang:trunc(math:pow(2, 42)),
    Limit49 = erlang:trunc(math:pow(2, 49)),
    Limit56 = erlang:trunc(math:pow(2, 56)),
    Limit64 = erlang:trunc(math:pow(2, 64)),
    [
      {"7 bits", {inparallel, 10, positive_lazy_gen({0, Limit7})}},

      {"14 bits 1", {inparallel, 10, positive_lazy_gen({Limit7, Limit7+?TESTS_CNT})}},
      {"14 bits 2", {inparallel, 10, positive_lazy_gen({Limit14-?TESTS_CNT, Limit14})}},

      {"21 bits 1", {inparallel, 10, positive_lazy_gen({Limit14, Limit14+?TESTS_CNT})}},
      {"21 bits 2", {inparallel, 10, positive_lazy_gen({Limit21-?TESTS_CNT, Limit21})}},

      {"28 bits 1", {inparallel, 10, positive_lazy_gen({Limit21, Limit21+?TESTS_CNT})}},
      {"28 bits 2", {inparallel, 10, positive_lazy_gen({Limit28-?TESTS_CNT, Limit28})}},

      {"35 bits 1", {inparallel, 10, positive_lazy_gen({Limit28, Limit28+?TESTS_CNT})}},
      {"35 bits 2", {inparallel, 10, positive_lazy_gen({Limit35-?TESTS_CNT, Limit35})}},

      {"42 bits 1", {inparallel, 10, positive_lazy_gen({Limit35, Limit35+?TESTS_CNT})}},
      {"42 bits 2", {inparallel, 10, positive_lazy_gen({Limit42-?TESTS_CNT, Limit42})}},

      {"49 bits 1", {inparallel, 10, positive_lazy_gen({Limit42, Limit42+?TESTS_CNT})}},
      {"49 bits 2", {inparallel, 10, positive_lazy_gen({Limit49-?TESTS_CNT, Limit49})}},

      {"56 bits 1", {inparallel, 10, positive_lazy_gen({Limit49, Limit49+?TESTS_CNT})}},
      {"56 bits 2", {inparallel, 10, positive_lazy_gen({Limit56-?TESTS_CNT, Limit56})}},

      {"64 bits 1", {inparallel, 10, positive_lazy_gen({Limit56, Limit56+?TESTS_CNT})}},
      {"64 bits 2", {inparallel, 10, positive_lazy_gen({Limit64-?TESTS_CNT, Limit64})}},

      []

    ].

positive_lazy_gen({Cur, Max}) ->
   {generator,
    fun () ->
        if Cur =< Max ->
            [ {"check: " ++ integer_to_list(Cur), ?_test(
               begin
                  {BinNum, _} = llsn:encode_NUMBER(Cur),
                  {DecNum, _} = llsn:decode_NUMBER(BinNum),
                  Cur =:= DecNum
               end
              )} | positive_lazy_gen({Cur + 1, Max})
            ];
          true ->
            []
        end
    end}.


random_struct_test_() ->
    {inparallel, 10, random_struct_lazy_gen(10000)}.

random_struct_lazy_gen(N) ->
   {generator,
    fun () ->
        if N >= 0 ->
            Decl  = llsn_gen:gen_struct(10,5),
            Data  = llsn_gen:gen("struct"++integer_to_list(N), Decl),
            R     = io_lib:format("~p",[Decl]),
            Check = lists:flatten(R),
            [ {"check: " ++ Check, ?_test(
               begin
                  Encoded = llsn:encode(Data, Decl),
                  ?assert(is_binary(Encoded)),
                  Decoded = llsn:decode(Encoded),
                  Data =:= Decoded
               end
              )} | random_struct_lazy_gen(N - 1)
            ];
          true ->
            []
        end
    end}.


proper_test_() ->
    {timeout, 60, fun() ->
        proper_utils:run_proper(module,
                    fun() ->
                            ?assertEqual([], proper:module(?MODULE,
                                    [long_result, {max_size, 42}, {numtests, 1000}]))
                    end)
        end
    }.

prop_enc_dec() ->
  ?FORALL(Msg, union([binary(), list(range(1,255))]),
      begin
        EncDecMsg = base64:decode(base64:encode(Msg)),
        case is_binary(Msg) of
          true  -> EncDecMsg =:= Msg;
          false -> EncDecMsg =:= list_to_binary(Msg)
        end
      end).

prop_encode_decode_numbers() ->
  Limit = erlang:trunc(math:pow(2, 64)),
  ?FORALL(N, integer(-Limit, Limit),
      begin
        {BinNum, _} = llsn:encode_NUMBER(N),
        {DecNum, _} = llsn:decode_NUMBER(BinNum),
        N =:= DecNum
      end).


prop_encode_decode_unumbers() ->
  Limit = erlang:trunc(math:pow(2, 64)),
  ?FORALL(N, integer(0, Limit),
      begin
        {BinNum, _} = llsn:encode_UNUMBER(N),
        {DecNum, _} = llsn:decode_UNUMBER(BinNum),
        N =:= DecNum
      end).

prop_encode_decode_floats() ->
  ?FORALL(N, float(),
      begin
        {BinNum, _} = llsn:encode_FLOAT(N),
        {DecNum, _} = llsn:decode_FLOAT(BinNum),
        N =:= DecNum
      end).

prop_encode_decode_strings100() ->
  ?FORALL(BS, binary(1000),
      begin
        S = binary_to_list(BS),
        {BinS, _, []} = llsn:encode_STRING(S, []),
        {DecS, _, []} = llsn:decode_STRING(BinS, []),
        S =:= DecS
      end).

unichar() ->
  union([integer(0, 16#d7ff),
         integer(16#e000, 16#10ffff)]).

utf8_binary() ->
  ?LET(L, list(unichar()),
    unicode:characters_to_binary(L, utf8)).

prop_encode_decode_booleans() ->
  ?FORALL(N, boolean(),
      begin
        {BinNum, _} = llsn:encode_BOOL(N),
        {DecNum, _} = llsn:decode_BOOL(BinNum),
        N =:= DecNum
      end).

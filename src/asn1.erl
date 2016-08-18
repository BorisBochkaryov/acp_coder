-module(asn1).
-author("borisbochkarev").
-include("../include/ACP.hrl").
-include("../include/ACPdatatypes.hrl").
-include("../include/INCS3datatypes.hrl").
-include("../include/INCS3Internals.hrl").
-include("../include/INCS3opsargs.hrl").
-include_lib("chronica/include/chronica.hrl").

-export([encode/1, decode/1, test/1, writeToLogs/5]).

% logs for encode/decode
writeToLogs(TimeStart, TimeEnd, Packet, Result,Status) ->
  Time = (TimeEnd - TimeStart) / 1000,
  SizeBefore = length(lists:flatten(io_lib:format("~p",[Packet]))),
  SizeAfter = size(Result),
  case Status of
    encode ->
      log:info([asn1encode],"Encode Time: ~p Size before/after: ~p/~p, Compress:~.2f%~n",[Time, SizeBefore, SizeAfter, (1 - SizeAfter / SizeBefore) * 100]);
    decode ->
      log:info([asn1encode],"Decode Time: ~p Size before/after: ~p/~p~n",[Time, SizeBefore, SizeAfter])
  end.

% encode packet
encode(Pack) ->
  TimeStart = os:system_time(),
  Packet = change(Pack),
  % change packet type
  {TempTerm,TempPack} = Packet#'AcpMessage'.body,
  [H | T] = atom_to_list(TempTerm),
  Term = list_to_atom([H + 32 | T]),
  TempResult = Packet#'AcpMessage'{body = {Term, TempPack}},
  % encode
  case 'ACP':encode('AcpMessage',TempResult) of
    {ok, Binary} ->
      TimeEnd = os:system_time(),
      Flag = integer_to_binary(1),
      spawn(?MODULE,writeToLogs,[TimeStart,TimeEnd,Pack,Binary,encode]),
      <<Flag/binary,Binary/binary>>;
    Error ->
      {ok, Bin} = 'ACP':encode('ANYPACK',#'ANYPACK'{arg = term_to_binary(TempResult)}),
      Flag = integer_to_binary(2),
      log:error([asn1error], "Error:~p~nPacket:~p~n",[Error,Pack]),
      <<Flag/binary,Bin/binary>>
  end.


change(Pack) -> changePack(change(Pack, 1, []),1,[]).
% change packet for asn.1 formats (to list)
change(Pack, N, Res) ->
  Size = size(Pack),
  if N =< Size ->
    El = element(N,Pack),
    if is_tuple(El) ->
      if is_number(element(1,El)) , is_number(element(2,El)) , is_number(element(3,El)) -> % EventTime
        change(Pack, N + 1, Res ++ [change(El, 1, ['EventTime'])]);
      element(1, El) == sdp -> % TODO: сделать более простую сериализацию для SDPType
        change(Pack, N + 1, Res ++ term_to_binary(El));
      element(1, El) == refer -> % list with refer
        [Term] = element(2,El),
        Temp = list_to_tuple(change(Term,1,[])),
        Result = change(Pack, N + 1, Res ++ [{refer, [{element(1,Temp),list_to_tuple(element(2,Temp))}]}]),
        Result;
      true->
        change(Pack, N + 1, Res ++ [change(El, 1, [])])
      end;
    is_list(El) -> % changes in list
      change(Pack, N + 1, Res ++ [change(list_to_tuple(El), 1, [])]);
    El == undefined -> % change "undefined" on "asn1_NOVALUE"
      change(Pack, N + 1, Res ++ [asn1_NOVALUE]);
    N /= 1 , is_atom(El) ->
      List = atom_to_list(El),
      [First | _] = List,
      if First >= 65 , First =< 90 -> % text constant-atom (example: 'CHOLD')
        Atom = list_to_atom(string:to_lower(List)),
        change(Pack, N + 1, Res ++ [Atom]);
      true ->
        T = string:words(List, $@),
        if T >= 2 ->
          change(Pack, N + 1, Res ++ [List]);
        true ->
          change(Pack, N + 1, Res ++ [El])
        end
      end;
    is_atom(El) ->
      T = string:words(atom_to_list(El), $_),
      if T >= 2 -> % atoms with _ (example: call_id)
        List = atom_to_list(El),
        Position = string:chr(List,$_),
        ResList = list_to_atom(lists:sublist(List,Position-1) ++ [$-] ++ lists:nthtail(Position,List)),
        change(Pack, N + 1, Res ++ [ResList]);
      true ->
        change(Pack, N + 1, Res ++ [El])
      end;
    true ->
      change(Pack, N + 1, Res ++ [El])
    end;
  true ->
    Res
  end.

% change packet for asn.1 formats (to tuple)
changePack(Pack, N, Res) ->
  Size = length(Pack),
  if N =< Size ->
    El = element(N,list_to_tuple(Pack)),
    if N /= 1 ->
      if is_list(El) ->
        changePack(Pack, N + 1, Res ++ [changePack(El, 1, [])]);
      true->
        changePack(Pack, N + 1, Res ++ [El])
      end;
    is_list(El) ->
      Res ++ [changePack(El, 1, [])] ++ changePack(Pack, N + 1, []);
    is_number(El) ->
      tuple_to_list({El}) ++ changePack(Pack, N + 1, Res);
    true ->
      list_to_tuple([El] ++ changePack(Pack, N + 1, Res))
    end;
  true ->
    Res
  end.

% after decode process (change AcpMessage.body type)
afterDecode(Pack) ->
  TPack = rechange(Pack),
  {TempTerm, TempP} = TPack#'AcpMessage'.body,
  [H1 | T1] = atom_to_list(TempTerm),
  Term = list_to_atom([H1 - 32 | T1]),
  TPack#'AcpMessage'{body = {Term, TempP}}.

% decode packet
decode(Bytes) ->
  TimeStart = os:system_time(),
  Flag = binary:part(Bytes, {0,1}),
  BinPack = binary:part(Bytes, {1,byte_size(Bytes)-1}),
  case Flag of
    <<"1">> ->
      {ok, Pack} = 'ACP':decode('AcpMessage',BinPack),
      Result = afterDecode(Pack),
      TimeEnd = os:system_time(),
      spawn(?MODULE,writeToLogs,[TimeStart,TimeEnd,Result,Bytes,decode]),
      Result;
    <<"2">> ->
      {ok, Pack} = 'ACP':decode('ANYPACK',BinPack),
      TempPack = binary_to_term(Pack#'ANYPACK'.arg),
      afterDecode(TempPack);
    _ ->
      error
  end.

rechange(Pack) -> rechangePack(rechange(Pack, 1, []),1,[]).
% analogue change
rechange(Pack, N, Res) ->
  Size = length(tuple_to_list(Pack)),
  if N =< Size ->
    El = element(N,Pack),
    if is_tuple(El) ->
      case element(1,El) of
        'EventTime' ->
          rechange(Pack, N + 1, Res ++ [list_to_tuple(change(El, 2, []))]);
        'SDPType' ->
          {'SDPType', Type, Binary} = El,
          rechange(Pack, N + 1, Res ++ [{'SDPType', Type, [binary_to_term(Binary)]}]);
        refer ->
          [Term] = element(2,El),
          {_, Value} = element(2,Term),
          rechange(Pack, N + 1, Res ++ [{refer, [{'CHUNT', {chunt_refer, Value}}]}]);
        _ ->
          rechange(Pack, N + 1, Res ++ [rechange(El, 1, [])])
      end;
    is_list(El) -> % changes in list
      T = string:words(El, $@),
      if T >= 2 ->
        Res ++ [list_to_atom(El)];
      true ->
        rechange(Pack, N + 1, Res ++ [rechange(list_to_tuple(El), 1, [])])
      end;
    El == asn1_NOVALUE ->
      rechange(Pack, N + 1, Res ++ [undefined]);
    is_atom(El) ->
      Atom = atom_to_list(El),
      T = string:words(Atom, $-),
      if T >= 2 -> % atoms with _ (example: call_id)
        Position = string:chr(Atom,$-),
        ResList = list_to_atom(lists:sublist(Atom,Position-1) ++ [$_] ++ lists:nthtail(Position,Atom)),
        rechange(Pack, N + 1, Res ++ [ResList]);
      true ->
        List = [way, acb, cfb, cfsip, cfnr, cfu, cgg, chold, chunt, cidb, ctr, dnd, mgm, pickup, hole],
        Search = [Res || Res <- List, Res == El],
        if Search /= [] ->
          Atom = list_to_atom(string:to_upper(atom_to_list(El))),
          rechange(Pack, N + 1, Res ++ [Atom]);
        true ->
          rechange(Pack, N + 1, Res ++ [El])
        end
      end;
    true ->
      rechange(Pack, N + 1, Res ++ [El])
    end;
  true ->
    Res
  end.

rechangePack(Pack, N, Res) ->
  Size = length(Pack),
  if N =< Size ->
    El = element(N,list_to_tuple(Pack)),
    if N /= 1 ->
      if is_list(El) ->
        rechangePack(Pack, N + 1, Res ++ [rechangePack(El, 1, [])]);
      true->
        rechangePack(Pack, N + 1, Res ++ [El])
      end;
    is_list(El) ->
        Res ++ [rechangePack(El, 1, [])] ++ rechangePack(Pack, N + 1, []);
    is_number(El) ->
      tuple_to_list({El}) ++ rechangePack(Pack, N + 1, Res);
    true ->
      list_to_tuple([El] ++ rechangePack(Pack, N + 1, Res))
    end;
    true ->
      Res
  end.

% method with list for testing asn1encode
test([Pack | T]) ->
  Bytes = asn1:encode(Pack),
  Result = asn1:decode(Bytes),
  if Result == Pack ->
    io:format("OK~n");
  true ->
    io:format("Error~nPacket:~n~p~nResult:~n~p~n~n",[Pack,Result])
  end,
  ?MODULE:test(T);
test([]) -> ok.

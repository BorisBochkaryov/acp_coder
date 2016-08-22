-module(acp_coder).
-author("borisbochkarev").
-include("../include/ACP.hrl").
-include("../include/ACPdatatypes.hrl").
-include("../include/INCS3datatypes.hrl").
-include("../include/INCS3Internals.hrl").
-include("../include/INCS3opsargs.hrl").
-include_lib("chronica/include/chronica.hrl").

-export([encode/1, decode/1, test/1, rechange/2]).

% logs for encode/decode
writeToLogs(TimeStart, Packet, Result, Status) ->
  Calc = fun(Start, End, Pack, Res, Stat) ->
    Time = (End - Start) / 1000,
    SizeBefore = erlang:length(lists:flatten(io_lib:format("~p",[Pack]))),
    SizeAfter = erlang:size(Res),
    case Stat of
      encode ->
        [Time, SizeBefore, SizeAfter, (1 - SizeAfter / SizeBefore) * 100];
      decode ->
        [Time, SizeBefore, SizeAfter]
    end
  end,
  case Status of
    encode ->
      log:info([asn1encode],"~nEncode Time: | Size before,after: | Compress:~n~p~n",[Calc(TimeStart,os:system_time(),Packet,Result,Status)]);
    decode ->
      log:info([asn1encode],"~nDecode Time: | Size before,after:~n~p~n",[Calc(TimeStart,os:system_time(),Packet,Result,Status)])
  end.

% encode packet
encode(Pack) ->
  TimeStart = os:system_time(),
  Packet = list_to_tuple(change(Pack,1)),
  % change packet type (for AcpBody)
  {TempTerm,TempPack} = Packet#'AcpMessage'.body,
  [H | T] = erlang:atom_to_list(TempTerm),
  Term = erlang:list_to_atom([H + 32 | T]),
  TempResult = Packet#'AcpMessage'{body = {Term, TempPack}},
  % encode
  case 'ACP':encode('AcpMessage',TempResult) of
    {_, Binary} ->
      writeToLogs(TimeStart,Pack,Binary,encode),
      <<49,Binary/binary>>;
    Error ->
      {_, Bin} = 'ACP':encode('ANYPACK',#'ANYPACK'{arg = term_to_binary(TempResult)}),
      log:error([asn1error], "Error:~p~nPacket:~p~n",[Error,Pack]),
      <<50,Bin/binary>>
  end.

% bypassing the packet in one pass for encode
change(Pack, N) ->
  Size = erlang:size(Pack),
  if N =< Size ->
    El = erlang:element(N,Pack),
    if erlang:is_atom(El) ; erlang:is_binary(El) ; erlang:is_number(El) ->
      if El == undefined -> % change undefined on asn1_NOVALUE
        [asn1_NOVALUE | change(Pack,N+1)];
      erlang:is_atom(El) ->
        List = erlang:atom_to_list(El),
        [First | _] = List,
        if First >= 65 , First =< 90 , N /= 1 -> % text constant-atom (example: 'CHOLD')
          Atom = erlang:list_to_atom(string:to_lower(List)),
          [Atom | change(Pack,N+1)];
        true ->
          T2 = string:words(List, $@),
          T11 = string:words(List, $_),
          if T2 >= 2 -> % text constant-atom with @ (example: sip1@ecss1)
            [List | change(Pack,N+1)];
          T11 >= 2 -> % text constant with _ (example: remote_number)
            Position = string:chr(List,$_),
            ResList = erlang:list_to_atom(lists:sublist(List,Position-1) ++ [$-] ++ lists:nthtail(Position,List)),
            [ResList | change(Pack,N+1)];
          true -> % other atoms
            [El | change(Pack,N+1)]
          end
        end;
      true ->
        [El | change(Pack,N+1)]
      end;
    erlang:is_tuple(El) ->
      if erlang:size(El) == 3 , erlang:is_number(erlang:element(1, El)) ,
          erlang:is_number(erlang:element(2, El)) , erlang:is_number(erlang:element(3, El)) -> % 'EventTime'
        [erlang:list_to_tuple(['EventTime'] ++ change(El,1)) | change(Pack, N+1)];

      % TODO: исправить для SDPType
      erlang:size(El) == 4 , erlang:element(1,El) == attribute ,
          erlang:is_binary(erlang:element(2, El)) == false -> % attribute в SDPType TODO: исправить
        {_,ToBin,_,_} = El,
        Elem = setelement(2,El,binary:list_to_bin(erlang:atom_to_list(ToBin))),
        [erlang:list_to_tuple(change(Elem,1)) | change(Pack, N+1)];
      erlang:element(1,El) == sdp, erlang:element(8, El) == <<>> -> % connection [8] в SDPType TODO: исправить
        Elem = setelement(8,El,undefined),
        if erlang:element(9, El) == <<>> ->
          ResElem = setelement(9,El,undefined);
        true ->
          ResElem = Elem
        end,
        [erlang:list_to_tuple(change(ResElem,1)) | change(Pack, N + 1)];
      erlang:element(1,El) == media_description , element(4, El) == <<>> -> % connection [2] в SDPType - media-description TODO: исправить
        Elem = setelement(4,El,undefined),
        [erlang:list_to_tuple(change(Elem,1)) | change(Pack, N+1)];
      erlang:element(1,El) == sdp, erlang:element(9, El) == <<>> -> % atr6 [9] в SDPType TODO: исправить
        Elem = setelement(9,El,undefined),
        [erlang:list_to_tuple(change(Elem,1)) | change(Pack, N+1)];

        true ->
        [erlang:list_to_tuple(change(El,1)) | change(Pack, N+1)]
      end;
    erlang:is_list(El) ->
      [change(erlang:list_to_tuple(El),1) | change(Pack, N+1)];
    true ->
      El
    end;
  true ->
      []
  end.

% bypassing the packet in one pass for decode
rechange(Pack, N) ->
  Size = erlang:size(Pack),
  if N =< Size ->
    El = erlang:element(N,Pack),
    if erlang:is_atom(El) ; erlang:is_binary(El) ; erlang:is_number(El) ->
      if El == asn1_NOVALUE -> % change asn1_NOVALUE on undefined
        [undefined | rechange(Pack,N+1)];
      erlang:is_atom(El) ->
        List = erlang:atom_to_list(El),
        [First | _] = List,
        if First >= 65 , First =< 90 , N /= 1 -> % text constant-atom (example: 'CHOLD')
          Atom = erlang:list_to_atom(string:to_lower(List)),
          [Atom | rechange(Pack,N+1)];
        true ->
          T11 = string:words(List, $-),
          if T11 >= 2 -> % text constant with - (example: 'remote-number')
            Position = string:chr(List,$-),
            ResList = erlang:list_to_atom(lists:sublist(List,Position-1) ++ [$_] ++ lists:nthtail(Position,List)),
            [ResList | rechange(Pack,N+1)];
          true -> % other atoms
            Atoms = [way, acb, cfb, cfsip, cfnr, cfu, cgg, chold, chunt, cidb, ctr, dnd, mgm, pickup],
            Search = [Res || Res <- Atoms, Res == El],
            if Search /= [] ->
              Atom = erlang:list_to_atom(string:to_upper(erlang:atom_to_list(El))),
              [Atom | rechange(Pack,N+1)];
            true ->
              [El | rechange(Pack,N+1)]
            end
          end
        end;
      true ->
        [El | rechange(Pack,N+1)]
      end;
    erlang:is_tuple(El) ->
      if erlang:size(El) == 4 , erlang:element(1, El) == 'EventTime' -> % 'EventTime'
        [erlang:list_to_tuple(rechange(El,2)) | rechange(Pack, N+1)];
      true ->
        [erlang:list_to_tuple(rechange(El,1)) | rechange(Pack, N+1)]
      end;
    erlang:is_list(El) ->
      T11 = string:words(El, $@),
      if T11 >= 2 ->
        [list_to_atom(El) | rechange(Pack, N+1)];
      true ->
        [rechange(erlang:list_to_tuple(El),1) | rechange(Pack, N+1)]
      end;
    true ->
      El
    end;
  true ->
    []
  end.

% after decode process (change AcpMessage.body type)
afterDecode(Pack) ->
  %TPack = rechange(Pack),
  TPack = list_to_tuple(rechange(Pack,1)),
  {TempTerm, TempP} = TPack#'AcpMessage'.body,
  [H1 | T1] = erlang:atom_to_list(TempTerm),
  Term = erlang:list_to_atom([H1 - 32 | T1]),
  TPack#'AcpMessage'{body = {Term, TempP}}.

% decode packet
decode(<<49,BinPack/binary>>) ->
  TimeStart = os:system_time(),
  {_, Pack} = 'ACP':decode('AcpMessage',BinPack),
  Result = afterDecode(Pack),
  writeToLogs(TimeStart,Result,BinPack,decode),
  Result;
decode(<<50,BinPack/binary>>) ->
  {_, Pack} = 'ACP':decode('ANYPACK',BinPack),
  TempPack = erlang:binary_to_term(Pack#'ANYPACK'.arg),
  afterDecode(TempPack).

% method with list for testing asn1encode
test([Pack | T]) ->
  Bytes = encode(Pack),
  Result = decode(Bytes),
  if Result == Pack ->
    io:format("OK~n");
  true ->
    io:format("Error~nPacket:~n~p~nResult:~n~p~n~n",[Pack,Result])
  end,
  ?MODULE:test(T);
test([]) -> ok.

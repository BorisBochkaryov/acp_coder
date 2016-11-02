%%%-------------------------------------------------------------------
%%% -*- coding: utf-8 -*-
%%% @author Boris Bochkarev
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(acp_coder).

-compile(inline).
-compile({inline_size, 128}).

-export([test/1, encode/1, decode/1]).
-include_lib("chronica/include/chronica.hrl").
-include("INCS3datatypes.hrl").
-include("INCS3Internals.hrl").
-include("ISUP.hrl").
-include("ACP.hrl").


%%%%%% Defines
-define(VERSION,
  begin
      {_, Commit} = lists:split(7,os:cmd("git log -1 | head -1")),
      string:strip(Commit, both, $\n)
  end).
-define(Z_Undefined, <<0>>).
-define(Undefined, 0).

% Simple types
-define(T_Binary, 1).
-define(T_Integer8, 2).
-define(T_Tuple, 3).
-define(T_List, 4).
-define(T_Bool, 5).
-define(T_String, 6).
-define(T_IrregularList, 7).
-define(T_Bool_true, 8).
-define(T_Bool_false, 9).
-define(T_Binary_Long, 10).
-define(T_Integer24, 11).

% Records
-define(R_AcpMessage,1).
-define(R_ServiceFeatureType, 2).
-define(R_ServiceFeatureInd, 3).
-define(R_ReleaseReq, 4).
-define(R_SetupReqAck, 5).
-define(R_SetupAckType, 6).
-define(R_ReferType, 7).
-define(R_SetupIndAck, 8).
-define(R_CallProgressReq, 9).
-define(R_CallProgressType, 10).
-define(R_EventInformation, 11).
-define(R_OptionalBackwardCallIndicators, 12).
-define(R_RedirectingNumber, 13).
-define(R_CallDiversionInformation, 14).
-define(R_SDPType, 15).
-define(R_TrunkGroupId, 16).
-define(R_CallTransferNumber, 17).
-define(R_CallProgressInd, 18).
-define(R_RedirectionNumber, 19).
-define(R_ReleaseType, 20).
-define(R_ReleaseInd, 21).
-define(R_SetupInd, 22).
-define(R_SetupIRType, 23).
-define(R_CalledPartyNumber, 24).
-define(R_CallingPartyNumber, 25).
-define(R_OriginalCalledNumber, 26).
-define(R_RedirectionInformation, 27).
-define(R_MLPPPrecedence, 28).
-define(R_SetupReq, 29).
-define(R_Refer, 30).
-define(R_SetupResp, 31).
-define(R_SetupCRType, 32).
-define(R_SetupConf, 33).
-define(R_EventTime, 34).
-define(R_Tuple, 35).
-define(R_LocationNumber, 36).
-define(R_CallerDisplayInformation, 37).
-define(R_CallId, 38).
-define(R_SubsequentAddressReq, 39).
-define(R_SubsequentAddressType, 40).
-define(R_ErrorRecord, 253).
-define(UNKNOWN, 254).

% Enumerated
-define(E_CauseLittle, 1).
-define(E_Cause, 2).
-define(E_CauseInitiator, 3).
-define(E_EventIndicator, 4).
-define(E_EventPresentationRestrictedIndicator, 5).
-define(E_InBandInfoIndicator, 6).
-define(E_CallDiversionIndicator, 7).
-define(E_SimpleSegmentationIndicator, 8).
-define(E_MLPPUserIndicator, 9).
-define(E_NAIType, 10).
-define(E_NIType, 11).
-define(E_NPIType, 12).
-define(E_APRIType, 13).
-define(E_RedirectionRestrictionIndicator, 14).
-define(E_NotificationSubscriptionOptions, 15).
-define(E_RedirectingReason, 16).
-define(E_TypeSDPType, 17).
-define(E_ScreeningType, 18).
-define(E_INNIType, 19).
-define(E_CalledPartysCategory, 20).
-define(E_CallingPartysCategory, 21).
-define(E_SetupModeType, 22).
-define(E_OriginalRedirectionReason, 23).
-define(E_RedirectingIndicator, 24).
-define(E_RedirectionCounter, 26).
-define(E_ServingSide, 27).

% Tuples
-define(Tu_sipInfo, 1).
-define(Tu_sdp, 2).
-define(Tu_origin, 3).
-define(Tu_connection, 4).
-define(Tu_media_description, 5).
-define(Tu_keyvalue, 6).
-define(Tu_media, 7).
-define(Tu_attribute, 8).
-define(Tu_format, 9).
-define(Tu_MediaId, 10).
-define(Tu_SSNotification, 11).
-define(Tu_AdditionalISUP, 12).
-define(Tu_AdditionalISUPParam, 13).
-define(Tu_AdditionalSIP, 14).
-define(Tu_AdditionalSIPParam, 15).
-define(Tu_SSEntity, 16).
-define(Tu_forSIP, 17).
-define(B_Undefined, 63).

% Encode types
-define(RECORD(Term), {record, Term}).
-define(BINARY(Term), {binary, Term}).
-define(INTEGER(Term), {integer, Term}).
-define(ENUM(Enum,Term), {enum, Enum, Term}).
-define(LIST(Term), {list, Term}).
-define(BOOL(Term), {bool, Term}).
-define(TIME(Term), {time, Term}).
-define(UNKNOWN(Term), {unknown, Term}).
-define(TUPLE(Term), {tuple, Term}).
-define(STRING(Term), {string, Term}).

%%%%%% Macros
-define(SIZE(Bin), erlang:size(Bin)).
-define(GET_EL(N,Tuple), erlang:element(N,Tuple)).

-define(ENCODE_BINARY(Bin), <<?T_Binary, (?SIZE(Bin)):8, Bin/binary>>).
-define(ENCODE_LONG_BINARY(Bin), <<?T_Binary_Long, (?SIZE(Bin)):24, Bin/binary>>).
% -define(ENCODE_LONG_INTEGER(Bin, Size), encode_long_integer(Bin, Size)).
-define(ENCODE_INTEGER(Int), encode_integer_any(Int)).
-define(ENCODE_STRING(Str), encode_string(Str)).
-define(ENCODE_BOOL(Atom), encode_bool(Atom)).
-define(ENCODE_UNKNOWN(Term), encode_unknown(Term)).
-define(ENCODE_TIME(MegaSec,Sec,MicSec), encode_time(MegaSec,Sec,MicSec)).

-define(DECODE_BINARY, <<?T_Binary, Size:8, _Bin:Size/binary, _Rest/binary>>).
-define(DECODE_LONG_BINARY, <<?T_Binary_Long, Size:24, _Bin:Size/binary, _Rest/binary>>).
-define(DECODE_INTEGER, <<?T_Integer8, _Size:8, _Bin/binary>>).
-define(DECODE_LONG_INTEGER, <<?T_Integer24, Size:24, _Bin:Size/binary, _Rest/binary>>).
-define(DECODE_STRING, <<?T_String, _Size, _Bin/binary>>).
-define(DECODE_LIST, <<?T_List, _Bin/binary>>).
-define(DECODE_UNKNOWN, <<?UNKNOWN, _Bin/binary>>).
-define(DECODE_UNDEFINED, <<?Undefined, _Bin/binary>>).
-define(DECODE_TIME, <<?R_EventTime, SizeBinMeSec, _BinMeSec:SizeBinMeSec/binary, SizeSec, _BinSec:SizeSec/binary,
  SizeMicSec, _BinMicSec:SizeMicSec/binary, _Bin/binary>>).


%%%%%% Encode
encode(Pack) ->
  log:debug("encode args: ~p", [Pack]),
  TimeStart = erlang:monotonic_time(micro_seconds),
  try encode_record(Pack) of
    Result ->
      writeToLogs(TimeStart, Pack, Result, encode),
      log:debug("encode result: ~p", [Result]),
      Result
  catch
    error:_ ->
      writeToError(["Unknown pack",Pack]),
      ?ENCODE_UNKNOWN(Pack)
  end.

encodeFromList([{binary, H} | T]) ->
  <<(encode_binary(H))/binary, (encodeFromList(T))/binary>>;
encodeFromList([{integer, H} | T]) ->
  <<(encode_integer(H))/binary, (encodeFromList(T))/binary>>;
encodeFromList([{record, H} | T]) ->
  <<(encode_record(H))/binary, (encodeFromList(T))/binary>>;
encodeFromList([{enum, H1, H2} | T]) ->
  <<(encode_enum(H1, H2))/binary, (encodeFromList(T))/binary>>;
encodeFromList([{list, H} | T]) ->
  <<(encode_list(H))/binary, (encodeFromList(T))/binary>>;
encodeFromList([{bool, H} | T]) ->
  <<(?ENCODE_BOOL(H))/binary, (encodeFromList(T))/binary>>;
encodeFromList([{time, H} | T]) ->
  <<(encode_time(H))/binary, (encodeFromList(T))/binary>>;
encodeFromList([{unknown, H} | T]) ->
  <<(?ENCODE_UNKNOWN(H))/binary, (encodeFromList(T))/binary>>;
encodeFromList([{typle, H} | T]) ->
  <<(encode_tuple(H))/binary, (encodeFromList(T))/binary>>;
encodeFromList([]) ->
  <<>>.

encode_record(#'AcpMessage'{uri = Uri, callRef = CallRef, body = Body}) ->
  Bin = encodeFromList([?BINARY(Uri),
                        ?INTEGER(CallRef),
                        ?RECORD(Body)]),
  <<?R_AcpMessage,
    (?ENCODE_STRING(?VERSION))/binary,
    Bin/binary>>;
encode_record(#'ServiceFeatureInd'{arg = Arg}) ->
  Bin = encodeFromList([?RECORD(Arg)]),
  <<?R_ServiceFeatureInd,
    Bin/binary>>;
encode_record(#'ServiceFeatureType'{cause = Cause, additionalInfo = AddInfo}) ->
  Bin = encodeFromList([?ENUM(?E_CauseLittle,Cause),
                        ?LIST(AddInfo)]),
  <<?R_ServiceFeatureType,
    Bin/binary>>;
encode_record(#'SetupReqAck'{arg = Arg}) ->
  Bin = encodeFromList([?RECORD(Arg)]),
  <<?R_SetupReqAck,
    Bin/binary>>;
encode_record(#'SetupIndAck'{arg = Arg}) ->
  Bin = encodeFromList([?RECORD(Arg)]),
  <<?R_SetupIndAck,
    Bin/binary>>;
encode_record(#'SetupAckType'{refer = Refer, trunkGroupId = TrunkGId}) ->
  Bin = encodeFromList([?RECORD(Refer),
                        ?RECORD(TrunkGId)]),
  % Arg
  <<?R_SetupAckType,
    Bin/binary>>;
encode_record(#'CallProgressReq'{arg = Arg}) ->
  Bin = encodeFromList([?RECORD(Arg)]),
  <<?R_CallProgressReq,
    Bin/binary>>;
encode_record(#'CallProgressInd'{arg = Arg}) ->
  Bin = encodeFromList([?RECORD(Arg)]),
  <<?R_CallProgressInd,
    Bin/binary>>;
encode_record(#'CallProgressType'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_Cause,Term#'CallProgressType'.cause),
                        ?ENUM(?E_CauseInitiator,Term#'CallProgressType'.causeInitiator),
                        ?LIST(Term#'CallProgressType'.causeDescription),
                        ?BINARY(Term#'CallProgressType'.causeIsup),
                        ?RECORD(Term#'CallProgressType'.eventInformation),
                        ?LIST(Term#'CallProgressType'.additionalInfo),
                        ?RECORD(Term#'CallProgressType'.oBCI),
                        ?LIST(Term#'CallProgressType'.gNotification),
                        ?RECORD(Term#'CallProgressType'.redirectionNumber),
                        ?ENUM(?E_RedirectionRestrictionIndicator,Term#'CallProgressType'.redirectionRestInd),
                        ?RECORD(Term#'CallProgressType'.callDiversionInfo),
                        ?LIST(Term#'CallProgressType'.facility),
                        ?RECORD(Term#'CallProgressType'.callId),
                        ?RECORD(Term#'CallProgressType'.sdp),
                        ?RECORD(Term#'CallProgressType'.trunkGroupId),
                        ?RECORD(Term#'CallProgressType'.callTransferNumber),
                        ?RECORD(Term#'CallProgressType'.refer),
                        ?TIME(Term#'CallProgressType'.eventTime)]),
  <<?R_CallProgressType,
    Bin/binary>>;
encode_record(#'EventInformation'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_EventIndicator,Term#'EventInformation'.eventIndicator),
                        ?ENUM(?E_EventPresentationRestrictedIndicator,Term#'EventInformation'.eventPresentationIndicator)]),
  <<?R_EventInformation,
    Bin/binary>>;
encode_record(#'OptionalBackwardCallIndicators'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_InBandInfoIndicator,Term#'OptionalBackwardCallIndicators'.inbInfoInd),
                        ?ENUM(?E_CallDiversionIndicator,Term#'OptionalBackwardCallIndicators'.callDiversionInd),
                        ?ENUM(?E_SimpleSegmentationIndicator,Term#'OptionalBackwardCallIndicators'.simpleSegmentationInf),
                        ?ENUM(?E_MLPPUserIndicator,Term#'OptionalBackwardCallIndicators'.mlppUserInd)]),
  <<?R_OptionalBackwardCallIndicators,
    Bin/binary>>;
encode_record(#'RedirectingNumber'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_NAIType, Term#'RedirectingNumber'.nai),
                        ?ENUM(?E_NIType, Term#'RedirectingNumber'.ni),
                        ?BOOL(Term#'RedirectingNumber'.incomplete),
                        ?ENUM(?E_NPIType,Term#'RedirectingNumber'.npi),
                        ?ENUM(?E_APRIType,Term#'RedirectingNumber'.apri),
                        ?ENUM(?E_CallingPartysCategory,Term#'RedirectingNumber'.category),
                        ?LIST(Term#'RedirectingNumber'.digits),
                        ?LIST(Term#'RedirectingNumber'.displayName),
                        ?LIST(Term#'RedirectingNumber'.sipUri),
                        ?LIST(Term#'RedirectingNumber'.vdn)]),
  <<?R_RedirectingNumber,
    Bin/binary>>;
encode_record(#'CallDiversionInformation'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_NotificationSubscriptionOptions, Term#'CallDiversionInformation'.notificationSubsOpts),
                        ?ENUM(?E_RedirectingReason, Term#'CallDiversionInformation'.redirectingReason)]),
  <<?R_CallDiversionInformation,
    Bin/binary>>;
encode_record(#'SDPType'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_TypeSDPType, Term#'SDPType'.type),
                        ?LIST(Term#'SDPType'.body)]),
  <<?R_SDPType,
    Bin/binary>>;
encode_record(#'TrunkGroupId'{} = Term) ->
  Bin = encodeFromList([?LIST(Term#'TrunkGroupId'.trunkGroupId),
                        ?LIST(Term#'TrunkGroupId'.trunkId),
                        ?LIST(Term#'TrunkGroupId'.pCMId),
                        ?INTEGER(Term#'TrunkGroupId'.channelNumber)]),
  <<?R_TrunkGroupId,
    Bin/binary>>;
encode_record(#'CallTransferNumber'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_NAIType, Term#'CallTransferNumber'.nai),
                        ?ENUM(?E_NIType, Term#'CallTransferNumber'.ni),
                        ?BOOL(Term#'CallTransferNumber'.incomplete),
                        ?ENUM(?E_NPIType,Term#'CallTransferNumber'.npi),
                        ?ENUM(?E_APRIType,Term#'CallTransferNumber'.apri),
                        ?ENUM(?E_ScreeningType,Term#'CallTransferNumber'.screening),
                        ?ENUM(?E_CallingPartysCategory,Term#'CallTransferNumber'.category),
                        ?LIST(Term#'CallTransferNumber'.digits),
                        ?LIST(Term#'CallTransferNumber'.displayName),
                        ?LIST(Term#'CallTransferNumber'.sipUri),
                        ?LIST(Term#'CallTransferNumber'.vdn)]),
  <<?R_CallTransferNumber,
    Bin/binary>>;
encode_record({refer, List}) ->
  Bin = encodeFromList([?LIST(List)]),
  <<?R_Refer,
    Bin/binary>>;
encode_record(#'ReferType'{} = Term) ->
  Bin = encodeFromList([?LIST(Term#'ReferType'.exchange),
                        ?LIST(Term#'ReferType'.routingKey),
                        ?BINARY(Term#'ReferType'.sid),
                        ?INTEGER(Term#'ReferType'.callRef),
                        ?LIST(Term#'ReferType'.conf_id)]),
  <<?R_ReferType,
    Bin/binary>>;
encode_record(#'RedirectionNumber'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_NAIType, Term#'RedirectionNumber'.nai),
                        ?ENUM(?E_NIType, Term#'RedirectionNumber'.ni),
                        ?BOOL(Term#'RedirectionNumber'.incomplete),
                        ?ENUM(?E_INNIType, Term#'RedirectionNumber'.inni),
                        ?ENUM(?E_NPIType,Term#'RedirectionNumber'.npi),
                        ?ENUM(?E_CallingPartysCategory,Term#'RedirectionNumber'.category),
                        ?LIST(Term#'RedirectionNumber'.digits),
                        ?LIST(Term#'RedirectionNumber'.displayName),
                        ?LIST(Term#'RedirectionNumber'.sipUri),
                        ?LIST(Term#'RedirectionNumber'.vdn)]),
  <<?R_RedirectionNumber,
    Bin/binary>>;
encode_record(#'ReleaseReq'{arg = Arg}) ->
  Bin = encodeFromList([?RECORD(Arg)]),
  <<?R_ReleaseReq,
    Bin/binary>>;
encode_record(#'ReleaseType'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_Cause, Term#'ReleaseType'.cause),
                        ?ENUM(?E_CauseInitiator, Term#'ReleaseType'.causeInitiator),
                        ?LIST(Term#'ReleaseType'.causeDescription),
                        ?LIST(Term#'ReleaseType'.dialledDigits),
                        ?BINARY(Term#'ReleaseType'.causeIsup),
                        ?LIST(Term#'ReleaseType'.additionalInfo),
                        ?RECORD(Term#'ReleaseType'.trunkGroupId),
                        ?RECORD(Term#'ReleaseType'.refer),
                        ?BOOL(Term#'ReleaseType'.need_ack),
                        ?TIME(Term#'ReleaseType'.eventTime)]),
  <<?R_ReleaseType,
    Bin/binary>>;
encode_record(#'ReleaseInd'{} = Term) ->
  Bin = encodeFromList([?RECORD(Term#'ReleaseInd'.arg)]),
  <<?R_ReleaseInd,
    Bin/binary>>;
encode_record(#'SetupInd'{} = Term) ->
  Bin = encodeFromList([?RECORD(Term#'SetupInd'.arg)]),
  <<?R_SetupInd,
    Bin/binary>>;
encode_record(#'SetupReq'{} = Term) ->
  Bin = encodeFromList([?RECORD(Term#'SetupReq'.arg)]),
  <<?R_SetupReq,
    Bin/binary>>;
encode_record(#'SetupIRType'{} = Term) ->
  Bin = encodeFromList([?LIST(Term#'SetupIRType'.domain),
                        ?RECORD(Term#'SetupIRType'.calledPartyNumber),
                        ?RECORD(Term#'SetupIRType'.callingPartyNumber),
                        ?LIST(Term#'SetupIRType'.locationNumber),
                        ?RECORD(Term#'SetupIRType'.originalCalledNumber),
                        ?LIST(Term#'SetupIRType'.userTeleserviceInformation),
                        ?LIST(Term#'SetupIRType'.genericNumber),
                        ?LIST(Term#'SetupIRType'.forwardCallIndicators),
                        ?RECORD(Term#'SetupIRType'.redirectingNumber),
                        ?RECORD(Term#'SetupIRType'.redirectingInformation),
                        ?UNKNOWN(Term#'SetupIRType'.uSIServiceIndicator),
                        ?LIST(Term#'SetupIRType'.uSIInformation),
                        ?LIST(Term#'SetupIRType'.isupCallRef),
                        ?RECORD(Term#'SetupIRType'.callId),
                        ?RECORD(Term#'SetupIRType'.sdp),
                        ?BINARY(Term#'SetupIRType'.mediaPoint),
                        ?LIST(Term#'SetupIRType'.additionalInfo),
                        ?RECORD(Term#'SetupIRType'.trunkGroupId),
                        ?LIST(Term#'SetupIRType'.callingPartyInfo),
                        ?LIST(Term#'SetupIRType'.calledPartyInfo),
                        ?LIST(Term#'SetupIRType'.callingIfaceInfo),
                        ?LIST(Term#'SetupIRType'.calledIfaceInfo),
                        ?RECORD(Term#'SetupIRType'.refer),
                        ?ENUM(?E_SetupModeType,Term#'SetupIRType'.mode),
                        ?RECORD(Term#'SetupIRType'.priority),
                        ?TIME(Term#'SetupIRType'.eventTime)]),
  <<?R_SetupIRType,
    Bin/binary>>;
encode_record(#'LocationNumber'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_NAIType, Term#'LocationNumber'.nai),
                        ?ENUM(?E_NIType, Term#'LocationNumber'.ni),
                        ?BOOL(Term#'LocationNumber'.incomplete),
                        ?ENUM(?E_NPIType,Term#'LocationNumber'.npi),
                        ?ENUM(?E_APRIType,Term#'LocationNumber'.apri),
                        ?ENUM(?E_ScreeningType,Term#'LocationNumber'.screening),
                        ?ENUM(?E_CallingPartysCategory,Term#'LocationNumber'.category),
                        ?LIST(Term#'LocationNumber'.digits),
                        ?LIST(Term#'LocationNumber'.displayName),
                        ?LIST(Term#'LocationNumber'.callerId),
                        ?LIST(Term#'LocationNumber'.sipUri),
                        ?LIST(Term#'LocationNumber'.vdn)]),
  <<?R_LocationNumber,
    Bin/binary>>;
encode_record(#'CalledPartyNumber'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_NAIType, Term#'CalledPartyNumber'.nai),
                        ?ENUM(?E_NIType, Term#'CalledPartyNumber'.ni),
                        ?BOOL(Term#'CalledPartyNumber'.incomplete),
                        ?ENUM(?E_INNIType, Term#'CalledPartyNumber'.inni),
                        ?ENUM(?E_NPIType,Term#'CalledPartyNumber'.npi),
                        ?ENUM(?E_CalledPartysCategory,Term#'CalledPartyNumber'.category),
                        ?LIST(Term#'CalledPartyNumber'.digits),
                        ?LIST(Term#'CalledPartyNumber'.displayName),
                        ?LIST(Term#'CalledPartyNumber'.sipUri),
                        ?LIST(Term#'CalledPartyNumber'.vdn)]),
  <<?R_CalledPartyNumber,
    Bin/binary>>;
encode_record(#'CallingPartyNumber'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_NAIType, Term#'CallingPartyNumber'.nai),
                        ?ENUM(?E_NIType, Term#'CallingPartyNumber'.ni),
                        ?BOOL(Term#'CallingPartyNumber'.incomplete),
                        ?ENUM(?E_NPIType,Term#'CallingPartyNumber'.npi),
                        ?ENUM(?E_APRIType,Term#'CallingPartyNumber'.apri),
                        ?ENUM(?E_ScreeningType,Term#'CallingPartyNumber'.screening),
                        ?ENUM(?E_CallingPartysCategory,Term#'CallingPartyNumber'.category),
                        ?LIST(Term#'CallingPartyNumber'.digits),
                        ?RECORD(Term#'CallingPartyNumber'.callerDisplayInformation),
                        ?LIST(Term#'CallingPartyNumber'.sipUri),
                        ?LIST(Term#'CallingPartyNumber'.vdn)]),
  <<?R_CallingPartyNumber,
    Bin/binary>>;
encode_record(#'CallerDisplayInformation'{} = Term) ->
  Bin = encodeFromList([?BOOL(Term#'CallerDisplayInformation'.showDisplayName),
                        ?LIST(Term#'CallerDisplayInformation'.displayName),
                        ?BOOL(Term#'CallerDisplayInformation'.showCallerId),
                        ?LIST(Term#'CallerDisplayInformation'.callerId)]),
  <<?R_CallerDisplayInformation,
    Bin/binary>>;
encode_record(#'SetupCRType'{} = Term) ->
  Bin = encodeFromList([?RECORD(Term#'SetupCRType'.connectedNumber),
                        ?LIST(Term#'SetupCRType'.additionalInfo),
                        ?RECORD(Term#'SetupCRType'.redirectionNumber),
                        ?ENUM(?E_RedirectionRestrictionIndicator, Term#'SetupCRType'.redirectionRestInd),
                        ?RECORD(Term#'SetupCRType'.callId),
                        ?RECORD(Term#'SetupCRType'.sdp),
                        ?RECORD(Term#'SetupCRType'.refer),
                        ?TIME(Term#'SetupCRType'.eventTime)]),
  <<?R_SetupCRType,
    Bin/binary>>;
encode_record(#'MLPPPrecedence'{} = Term) ->
  Bin = encodeFromList([?INTEGER(Term#'MLPPPrecedence'.lfb),
                        ?INTEGER(Term#'MLPPPrecedence'.precedenceLevel),
                        ?INTEGER(Term#'MLPPPrecedence'.network_identity),
                        ?INTEGER(Term#'MLPPPrecedence'.mlpp_service_domain)]),
  <<?R_MLPPPrecedence,
    Bin/binary>>;
encode_record(#'OriginalCalledNumber'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_NAIType, Term#'OriginalCalledNumber'.nai),
                        ?ENUM(?E_NIType, Term#'OriginalCalledNumber'.ni),
                        ?BOOL(Term#'OriginalCalledNumber'.incomplete),
                        ?ENUM(?E_NPIType,Term#'OriginalCalledNumber'.npi),
                        ?ENUM(?E_APRIType,Term#'OriginalCalledNumber'.apri),
                        ?ENUM(?E_CallingPartysCategory,Term#'OriginalCalledNumber'.category),
                        ?LIST(Term#'OriginalCalledNumber'.digits),
                        ?LIST(Term#'OriginalCalledNumber'.displayName),
                        ?LIST(Term#'OriginalCalledNumber'.sipUri),
                        ?LIST(Term#'OriginalCalledNumber'.vdn)]),
  <<?R_OriginalCalledNumber,
    Bin/binary>>;
encode_record(#'SetupResp'{} = Term) ->
  Bin = encodeFromList([?RECORD(Term#'SetupResp'.arg)]),
  <<?R_SetupResp,
    Bin/binary>>;
encode_record(#'SetupConf'{} = Term) ->
  Bin = encodeFromList([?RECORD(Term#'SetupConf'.arg)]),
  <<?R_SetupConf,
    Bin/binary>>;
encode_record(#'RedirectionInformation'{} = Term) ->
  Bin = encodeFromList([?ENUM(?E_OriginalRedirectionReason,Term#'RedirectionInformation'.originalRedirectionReason),
                        ?ENUM(?E_RedirectingIndicator,Term#'RedirectionInformation'.redirectingIndicator),
                        ?ENUM(?E_RedirectingReason,Term#'RedirectionInformation'.redirectingReason),
                        ?ENUM(?E_RedirectionCounter,Term#'RedirectionInformation'.redirectionCounter)]),
  <<?R_RedirectionInformation,
    Bin/binary>>;
encode_record(#'CallId'{} = Term) ->
  Bin = encodeFromList([?BINARY(Term#'CallId'.id),
                        ?BINARY(Term#'CallId'.fromTag),
                        ?BINARY(Term#'CallId'.toTag)]),
  <<?R_CallId,
    Bin/binary>>;
encode_record(#'SubsequentAddressReq'{} = Term) ->
  Bin = encodeFromList([?RECORD(Term#'SubsequentAddressReq'.arg)]),
  <<?R_SubsequentAddressReq,
    Bin/binary>>;
encode_record(#'SubsequentAddressType'{} = Term) ->
  Bin = encodeFromList([?LIST(Term#'SubsequentAddressType'.digits),
                        ?RECORD(Term#'SubsequentAddressType'.sdp)]),
  <<?R_SubsequentAddressType,
    Bin/binary>>;
encode_record(undefined) -> ?Z_Undefined;
encode_record(Pack) -> writeToError(["Unknown Pack",Pack]), ?ENCODE_UNKNOWN(Pack).


encode_enum(?E_CauseLittle,flash) -> <<?E_CauseLittle,0>>;
encode_enum(?E_CauseLittle,refer) -> <<?E_CauseLittle,1>>;
encode_enum(?E_Cause, normal) -> <<?E_Cause, 1>>;
encode_enum(?E_Cause, originationDenied) -> <<?E_Cause, 2>>;
encode_enum(?E_Cause, collectDigits) -> <<?E_Cause, 3>>;
encode_enum(?E_Cause, authorisationFailure) -> <<?E_Cause, 4>>;
encode_enum(?E_Cause, bPtyAlerted) -> <<?E_Cause, 5>>;
encode_enum(?E_Cause, noIndication) -> <<?E_Cause, 6>>;
encode_enum(?E_Cause, aPtyAbandon) -> <<?E_Cause, 7>>;
encode_enum(?E_Cause, invalidCollectedInformation) -> <<?E_Cause, 8>>;
encode_enum(?E_Cause, collectInformationFailure) -> <<?E_Cause, 9>>;
encode_enum(?E_Cause, aPtyDisc) -> <<?E_Cause, 10>>;
encode_enum(?E_Cause, bPtyDisc) -> <<?E_Cause, 11>>;
encode_enum(?E_Cause, routeSelectFailure) -> <<?E_Cause, 12>>;
encode_enum(?E_Cause, oNoAnswer) -> <<?E_Cause, 13>>;
encode_enum(?E_Cause, terminationDenied) -> <<?E_Cause, 14>>;
encode_enum(?E_Cause, notReachable) -> <<?E_Cause, 15>>;
encode_enum(?E_Cause, bPtyNoAnswer) -> <<?E_Cause, 16>>;
encode_enum(?E_Cause, bPtyBusyUDUB) -> <<?E_Cause, 17>>;
encode_enum(?E_Cause, bPtyBusyNDUB) -> <<?E_Cause, 18>>;
encode_enum(?E_Cause, ss7Failure) -> <<?E_Cause, 19>>;
encode_enum(?E_Cause, calledPartyRejected) -> <<?E_Cause, 20>>;
encode_enum(?E_Cause, tException) -> <<?E_Cause, 21>>;
encode_enum(?E_Cause, routeFailure1) -> <<?E_Cause, 22>>;
encode_enum(?E_Cause, routeFailure2) -> <<?E_Cause, 23>>;
encode_enum(?E_Cause, ssActivating) -> <<?E_Cause, 24>>;
encode_enum(?E_Cause, conversationTimeout) -> <<?E_Cause, 25>>;
encode_enum(?E_Cause, noCircuitAvailable) -> <<?E_Cause, 26>>;
encode_enum(?E_Cause, coreNotification) -> <<?E_Cause, 27>>;
encode_enum(?E_Cause, unsupportedMedia) -> <<?E_Cause, 28>>;
encode_enum(?E_Cause, numberIncomplete) -> <<?E_Cause, 29>>;
encode_enum(?E_Cause, invalidNumber) -> <<?E_Cause, 30>>;
encode_enum(?E_Cause, unassignedNumber) -> <<?E_Cause, 31>>;
encode_enum(?E_Cause, doNotDisturb) -> <<?E_Cause, 32>>;
encode_enum(?E_Cause, externalControlled) -> <<?E_Cause, 33>>;
encode_enum(?E_Cause, systemFailure) -> <<?E_Cause, 34>>;
encode_enum(?E_Cause, applicationNotification) -> <<?E_Cause, 35>>;
encode_enum(?E_Cause, refer_ok) -> <<?E_Cause, 36>>;
encode_enum(?E_Cause, refer_failure) -> <<?E_Cause, 37>>;
encode_enum(?E_Cause, uaPreemption) -> <<?E_Cause, 38>>;
encode_enum(?E_Cause, reservedResourcesPreemted) -> <<?E_Cause, 39>>;
encode_enum(?E_Cause, genericPreemption) -> <<?E_Cause, 40>>;
encode_enum(?E_Cause, nonIpPreemption) -> <<?E_Cause, 41>>;
encode_enum(?E_Cause, hole) -> <<?E_Cause, 42>>;
encode_enum(?E_Cause, hold) -> <<?E_Cause, 43>>;
encode_enum(?E_Cause, session_timeout) -> <<?E_Cause, 44>>;
encode_enum(?E_CauseInitiator, user) -> <<?E_CauseInitiator, 0>>;
encode_enum(?E_CauseInitiator, isup_network) -> <<?E_CauseInitiator, 1>>;
encode_enum(?E_CauseInitiator, non_isup_network) -> <<?E_CauseInitiator, 2>>;
encode_enum(?E_CauseInitiator, system) -> <<?E_CauseInitiator, 3>>;
encode_enum(?E_EventIndicator, alerting) -> <<?E_EventIndicator, 0>>;
encode_enum(?E_EventIndicator, progress) -> <<?E_EventIndicator, 1>>;
encode_enum(?E_EventIndicator, in_band_info) -> <<?E_EventIndicator, 2>>;
encode_enum(?E_EventIndicator, cfb) -> <<?E_EventIndicator, 3>>;
encode_enum(?E_EventIndicator, cfnr) -> <<?E_EventIndicator, 4>>;
encode_enum(?E_EventIndicator, cfu) -> <<?E_EventIndicator, 5>>;
encode_enum(?E_EventPresentationRestrictedIndicator, no_indication) -> <<?E_EventPresentationRestrictedIndicator, 0>>;
encode_enum(?E_EventPresentationRestrictedIndicator, restricted) -> <<?E_EventPresentationRestrictedIndicator, 1>>;
encode_enum(?E_InBandInfoIndicator, noIndication) -> <<?E_InBandInfoIndicator, 0>>;
encode_enum(?E_InBandInfoIndicator, inBandInfoOrPatternAvailable) -> <<?E_InBandInfoIndicator, 1>>;
encode_enum(?E_CallDiversionIndicator, noIndication) -> <<?E_CallDiversionIndicator, 0>>;
encode_enum(?E_CallDiversionIndicator, callDiversionMayOccur) -> <<?E_CallDiversionIndicator, 1>>;
encode_enum(?E_SimpleSegmentationIndicator, noAdditionalInformation) -> <<?E_SimpleSegmentationIndicator, 0>>;
encode_enum(?E_SimpleSegmentationIndicator, additionalInformationWillBeSent) -> <<?E_SimpleSegmentationIndicator, 1>>;
encode_enum(?E_MLPPUserIndicator, noIndication) -> <<?E_MLPPUserIndicator, 0>>;
encode_enum(?E_MLPPUserIndicator, mlppUser) -> <<?E_MLPPUserIndicator, 1>>;
encode_enum(?E_NAIType, spare) -> <<?E_NAIType, 0>>;
encode_enum(?E_NAIType, subscriberNumber) -> <<?E_NAIType, 1>>;
encode_enum(?E_NAIType, unknown) -> <<?E_NAIType, 2>>;
encode_enum(?E_NAIType, nationalNumber) -> <<?E_NAIType, 3>>;
encode_enum(?E_NAIType, internationNumber) -> <<?E_NAIType, 4>>;
encode_enum(?E_NIType, private) -> <<?E_NIType, 0>>;
encode_enum(?E_NIType, local) -> <<?E_NIType, 1>>;
encode_enum(?E_NIType, zone) -> <<?E_NIType, 2>>;
encode_enum(?E_NIType, intercity) -> <<?E_NIType, 3>>;
encode_enum(?E_NIType, international) -> <<?E_NIType, 4>>;
encode_enum(?E_NIType, emergency) -> <<?E_NIType, 5>>;
encode_enum(?E_NPIType, spare) -> <<?E_NPIType, 0>>;
encode_enum(?E_NPIType, isdnTelephony) -> <<?E_NPIType, 1>>;
encode_enum(?E_NPIType, dataNumberingPlan) -> <<?E_NPIType, 2>>;
encode_enum(?E_NPIType, telexNumberingPlan) -> <<?E_NPIType, 3>>;
encode_enum(?E_NPIType, reserved1) -> <<?E_NPIType, 4>>;
encode_enum(?E_NPIType, reserved2) -> <<?E_NPIType, 5>>;
encode_enum(?E_NPIType, reserved3) -> <<?E_NPIType, 6>>;
encode_enum(?E_INNIType, routingToInternalNumberAllowed) -> <<?E_INNIType, 0>>;
encode_enum(?E_INNIType, routingToInternalNumberNotAllowed) -> <<?E_INNIType, 1>>;
encode_enum(?E_APRIType, presentationAllowed) -> <<?E_APRIType, 0>>;
encode_enum(?E_APRIType, presentationRestricted) -> <<?E_APRIType, 1>>;
encode_enum(?E_APRIType, addressNotAvailable) -> <<?E_APRIType, 2>>;
encode_enum(?E_APRIType, spare) -> <<?E_APRIType, 3>>;
encode_enum(?E_RedirectionRestrictionIndicator, presentation_allowed) -> <<?E_RedirectionRestrictionIndicator, 0>>;
encode_enum(?E_RedirectionRestrictionIndicator, presentation_restricted) -> <<?E_RedirectionRestrictionIndicator, 1>>;
encode_enum(?E_NotificationSubscriptionOptions, unknown) -> <<?E_NotificationSubscriptionOptions, 0>>;
encode_enum(?E_NotificationSubscriptionOptions, presentation_not_allowed) -> <<?E_NotificationSubscriptionOptions, 1>>;
encode_enum(?E_NotificationSubscriptionOptions, presentation_allowed_with_redirecting_number) -> <<?E_NotificationSubscriptionOptions, 2>>;
encode_enum(?E_NotificationSubscriptionOptions, presentation_allowed_without_redirecting_number) -> <<?E_NotificationSubscriptionOptions, 3>>;
encode_enum(?E_RedirectingReason, unknown) -> <<?E_RedirectingReason, 0>>;
encode_enum(?E_RedirectingReason, busy) -> <<?E_RedirectingReason, 1>>;
encode_enum(?E_RedirectingReason, noReply) -> <<?E_RedirectingReason, 2>>;
encode_enum(?E_RedirectingReason, unconditional) -> <<?E_RedirectingReason, 3>>;
encode_enum(?E_RedirectingReason, deflectAlert) -> <<?E_RedirectingReason, 4>>;
encode_enum(?E_RedirectingReason, deflectResp) -> <<?E_RedirectingReason, 5>>;
encode_enum(?E_RedirectingReason, notReachable) -> <<?E_RedirectingReason, 6>>;
encode_enum(?E_RedirectingReason, timeOfDay) -> <<?E_RedirectingReason, 7>>;
encode_enum(?E_RedirectingReason, doNotDisturb) -> <<?E_RedirectingReason, 8>>;
encode_enum(?E_RedirectingReason, followMe) -> <<?E_RedirectingReason, 9>>;
encode_enum(?E_RedirectingReason, outOfService) -> <<?E_RedirectingReason, 10>>;
encode_enum(?E_RedirectingReason, away) -> <<?E_RedirectingReason, 11>>;
encode_enum(?E_RedirectingReason, undefined) -> <<?E_RedirectingReason, 16#FF>>;
encode_enum(?E_TypeSDPType, offer) -> <<?E_TypeSDPType, 0>>;
encode_enum(?E_TypeSDPType, answer) -> <<?E_TypeSDPType, 1>>;
encode_enum(?E_TypeSDPType, unknown) -> <<?E_TypeSDPType, 2>>;
encode_enum(?E_ScreeningType, userProvidedNotVerified) -> <<?E_ScreeningType, 0>>;
encode_enum(?E_ScreeningType, userProvidedVerifiedAndPassed) -> <<?E_ScreeningType, 1>>;
encode_enum(?E_ScreeningType, userProvidedVerifiedAndFailed) -> <<?E_ScreeningType, 2>>;
encode_enum(?E_ScreeningType, networkProvided) -> <<?E_ScreeningType, 3>>;
encode_enum(?E_CalledPartysCategory, unknownAtThisTime) -> <<?E_CalledPartysCategory, 0>>;
encode_enum(?E_CalledPartysCategory, operatorFrench) -> <<?E_CalledPartysCategory, 1>>;
encode_enum(?E_CalledPartysCategory, operatorEngish) -> <<?E_CalledPartysCategory, 2>>;
encode_enum(?E_CalledPartysCategory, operatorGerman) -> <<?E_CalledPartysCategory, 3>>;
encode_enum(?E_CalledPartysCategory, operatorRussian) -> <<?E_CalledPartysCategory, 4>>;
encode_enum(?E_CalledPartysCategory, operatorSpanish) -> <<?E_CalledPartysCategory, 5>>;
encode_enum(?E_CalledPartysCategory, reserved) -> <<?E_CalledPartysCategory, 6>>;
encode_enum(?E_CalledPartysCategory, ordinarySubscriber) -> <<?E_CalledPartysCategory, 7>>;
encode_enum(?E_CalledPartysCategory, subscriberWithPriority) -> <<?E_CalledPartysCategory, 8>>;
encode_enum(?E_CalledPartysCategory, dataCall) -> <<?E_CalledPartysCategory, 9>>;
encode_enum(?E_CalledPartysCategory, testCall) -> <<?E_CalledPartysCategory, 10>>;
encode_enum(?E_CalledPartysCategory, spare) -> <<?E_CalledPartysCategory, 11>>;
encode_enum(?E_CalledPartysCategory, payphone) -> <<?E_CalledPartysCategory, 12>>;
encode_enum(?E_CalledPartysCategory, category0) -> <<?E_CalledPartysCategory, 13>>;
encode_enum(?E_CalledPartysCategory, hotelsSubscriber) -> <<?E_CalledPartysCategory, 14>>;
encode_enum(?E_CalledPartysCategory, freeSubscriber) -> <<?E_CalledPartysCategory, 15>>;
encode_enum(?E_CalledPartysCategory, paidSubscriber) -> <<?E_CalledPartysCategory, 16>>;
encode_enum(?E_CalledPartysCategory, localSubscriber) -> <<?E_CalledPartysCategory, 17>>;
encode_enum(?E_CalledPartysCategory, localTaksofon) -> <<?E_CalledPartysCategory, 18>>;
encode_enum(?E_CalledPartysCategory, autoCallI) -> <<?E_CalledPartysCategory, 19>>;
encode_enum(?E_CalledPartysCategory, semiautoCallI) -> <<?E_CalledPartysCategory, 20>>;
encode_enum(?E_CalledPartysCategory, autoCallII) -> <<?E_CalledPartysCategory, 21>>;
encode_enum(?E_CalledPartysCategory, semiautoCallII) -> <<?E_CalledPartysCategory, 22>>;
encode_enum(?E_CalledPartysCategory, autoCallIII) -> <<?E_CalledPartysCategory, 23>>;
encode_enum(?E_CalledPartysCategory, semiautoCallIII) -> <<?E_CalledPartysCategory, 24>>;
encode_enum(?E_CalledPartysCategory, autoCallIV) -> <<?E_CalledPartysCategory, 25>>;
encode_enum(?E_CalledPartysCategory, semiautoCallIV) -> <<?E_CalledPartysCategory, 26>>;
encode_enum(?E_CallingPartysCategory, unknownAtThisTime) -> <<?E_CallingPartysCategory, 0>>;
encode_enum(?E_CallingPartysCategory, operatorFrench) -> <<?E_CallingPartysCategory, 1>>;
encode_enum(?E_CallingPartysCategory, operatorEngish) -> <<?E_CallingPartysCategory, 2>>;
encode_enum(?E_CallingPartysCategory, operatorGerman) -> <<?E_CallingPartysCategory, 3>>;
encode_enum(?E_CallingPartysCategory, operatorRussian) -> <<?E_CallingPartysCategory, 4>>;
encode_enum(?E_CallingPartysCategory, operatorSpanish) -> <<?E_CallingPartysCategory, 5>>;
encode_enum(?E_CallingPartysCategory, reserved) -> <<?E_CallingPartysCategory, 6>>;
encode_enum(?E_CallingPartysCategory, ordinarySubscriber) -> <<?E_CallingPartysCategory, 7>>;
encode_enum(?E_CallingPartysCategory, subscriberWithPriority) -> <<?E_CallingPartysCategory, 8>>;
encode_enum(?E_CallingPartysCategory, dataCall) -> <<?E_CallingPartysCategory, 9>>;
encode_enum(?E_CallingPartysCategory, testCall) -> <<?E_CallingPartysCategory, 10>>;
encode_enum(?E_CallingPartysCategory, spare) -> <<?E_CallingPartysCategory, 11>>;
encode_enum(?E_CallingPartysCategory, payphone) -> <<?E_CallingPartysCategory, 12>>;
encode_enum(?E_CallingPartysCategory, category0) -> <<?E_CallingPartysCategory, 13>>;
encode_enum(?E_CallingPartysCategory, hotelsSubscriber) -> <<?E_CallingPartysCategory, 14>>;
encode_enum(?E_CallingPartysCategory, freeSubscriber) -> <<?E_CallingPartysCategory, 15>>;
encode_enum(?E_CallingPartysCategory, paidSubscriber) -> <<?E_CallingPartysCategory, 16>>;
encode_enum(?E_CallingPartysCategory, localSubscriber) -> <<?E_CallingPartysCategory, 17>>;
encode_enum(?E_CallingPartysCategory, localTaksofon) -> <<?E_CallingPartysCategory, 18>>;
encode_enum(?E_CallingPartysCategory, autoCallI) -> <<?E_CallingPartysCategory, 19>>;
encode_enum(?E_CallingPartysCategory, semiautoCallI) -> <<?E_CallingPartysCategory, 20>>;
encode_enum(?E_CallingPartysCategory, autoCallII) -> <<?E_CallingPartysCategory, 21>>;
encode_enum(?E_CallingPartysCategory, semiautoCallII) -> <<?E_CallingPartysCategory, 22>>;
encode_enum(?E_CallingPartysCategory, autoCallIII) -> <<?E_CallingPartysCategory, 23>>;
encode_enum(?E_CallingPartysCategory, semiautoCallIII) -> <<?E_CallingPartysCategory, 24>>;
encode_enum(?E_CallingPartysCategory, autoCallIV) -> <<?E_CallingPartysCategory, 25>>;
encode_enum(?E_CallingPartysCategory, semiautoCallIV) -> <<?E_CallingPartysCategory, 26>>;
encode_enum(?E_SetupModeType, normal) -> <<?E_SetupModeType, 0>>;
encode_enum(?E_SetupModeType, dummy) -> <<?E_SetupModeType, 1>>;
encode_enum(?E_SetupModeType, internal) -> <<?E_SetupModeType, 2>>;
encode_enum(?E_SetupModeType, callback) -> <<?E_SetupModeType, 3>>;
encode_enum(?E_SetupModeType, parking) -> <<?E_SetupModeType, 4>>;
encode_enum(?E_SetupModeType, supervise) -> <<?E_SetupModeType, 5>>;
encode_enum(?E_SetupModeType, acd) -> <<?E_SetupModeType, 6>>;
encode_enum(?E_OriginalRedirectionReason, undefined) -> <<?E_OriginalRedirectionReason, 16#FF>>;
encode_enum(?E_OriginalRedirectionReason, unknown) -> <<?E_OriginalRedirectionReason, 0>>;
encode_enum(?E_OriginalRedirectionReason, busy) -> <<?E_OriginalRedirectionReason, 1>>;
encode_enum(?E_OriginalRedirectionReason, noReply) -> <<?E_OriginalRedirectionReason, 2>>;
encode_enum(?E_OriginalRedirectionReason, unconditional) -> <<?E_OriginalRedirectionReason, 3>>;
encode_enum(?E_RedirectingIndicator, noRedirection) -> <<?E_RedirectingIndicator, 0>>;
encode_enum(?E_RedirectingIndicator, reRouted) -> <<?E_RedirectingIndicator, 1>>;
encode_enum(?E_RedirectingIndicator, reRoutedAllRestricted) -> <<?E_RedirectingIndicator, 2>>;
encode_enum(?E_RedirectingIndicator, diversion) -> <<?E_RedirectingIndicator, 3>>;
encode_enum(?E_RedirectingIndicator, diversionAllRestricted) -> <<?E_RedirectingIndicator, 4>>;
encode_enum(?E_RedirectingIndicator, reRoutedRestricted) -> <<?E_RedirectingIndicator, 5>>;
encode_enum(?E_RedirectingIndicator, diversionRestricted) -> <<?E_RedirectingIndicator, 6>>;
encode_enum(?E_RedirectingIndicator, spare) -> <<?E_RedirectingIndicator, 7>>;
encode_enum(?E_RedirectingIndicator, undefined) -> <<?E_RedirectingIndicator, 16#FF:8>>;
encode_enum(?E_RedirectionCounter, undefined) -> <<?E_RedirectionCounter, 16#FF:8>>;
encode_enum(?E_RedirectionCounter, Num) -> <<?E_RedirectionCounter, Num:8>>;
encode_enum(?E_ServingSide, calling) -> <<?E_ServingSide, 0>>;
encode_enum(?E_ServingSide, called) -> <<?E_ServingSide, 1>>;
encode_enum(_,undefined) -> ?Z_Undefined;
encode_enum(Enum,Atom) -> writeToError(["Unknown Enum",Enum,Atom]), ?ENCODE_UNKNOWN(Atom).


encode_integer(undefined) -> ?Z_Undefined;
encode_integer(Int) when erlang:is_number(Int) -> ?ENCODE_INTEGER(Int).


encode_time({MegaSec, Sec, MicSec}) -> ?ENCODE_TIME(MegaSec, Sec, MicSec).


encode_binary(undefined) -> ?Z_Undefined;
encode_binary(Bin) when erlang:is_binary(Bin) , ?SIZE(Bin) =< 16#FF -> ?ENCODE_BINARY(Bin);
encode_binary(Bin) when erlang:is_binary(Bin) , ?SIZE(Bin) =< 16#FFFFFF -> ?ENCODE_LONG_BINARY(Bin);
encode_binary(Atom) when erlang:is_atom(Atom) -> ?ENCODE_UNKNOWN(Atom);
encode_binary(Term) -> writeToError(["Unknows Binary",Term]), ?ENCODE_UNKNOWN(Term).


encode_list(undefined) -> ?Z_Undefined;
encode_list([]) -> <<?T_List, 0>>;
encode_list(List) -> % список с элементами
  case is_string(List) of
    true ->
      ?ENCODE_STRING(List);
    _ ->
      Res = encode_list_(List,<<>>,[]),
      Size = ?ENCODE_INTEGER(?SIZE(Res)),
      <<?T_List, Size/binary, Res/binary>>
  end.


encode_list_([H | T], Res, _) when erlang:is_number(H) ->
  R = ?ENCODE_INTEGER(H),
  encode_list_(T, <<Res/binary, R/binary>>,number);
encode_list_([H | T], Res, Flag) when erlang:is_list(H) ->
  case Flag of
    list ->
      <<?T_List, R/binary>> = encode_list(H),
      encode_list_(T,<<Res/binary, R/binary>>,list);
    _ ->
      R = encode_list(H),
      encode_list_(T,<<Res/binary, R/binary>>,list)
  end;
encode_list_([H | T], Res, _) when erlang:is_binary(H) ->
  R = ?ENCODE_BINARY(H),
  encode_list_(T, <<Res/binary, R/binary>>,binary);
encode_list_([H | T], Res, _) when erlang:is_tuple(H) ->
  Bin = <<?R_Tuple, (encode_tuple(H))/binary>>,
  encode_list_(T, <<Res/binary, Bin/binary>>, tuple);
encode_list_([H | T], Res, _) when erlang:is_atom(H) ->
  Bin = ?ENCODE_UNKNOWN(H),
  encode_list_(T, <<Res/binary, Bin/binary>>, unknown);
encode_list_([H | T], Res, _) ->
  writeToError(["Unknown list",H]),
  Bin = ?ENCODE_UNKNOWN(H),
  encode_list_(T, <<Res/binary, Bin/binary>>, unknown);
encode_list_([], Res, _) -> Res.


encode_tuple({sipInfo, List} = Tuple) when ?SIZE(Tuple) == 2 ->
  Bin = encodeFromList([?LIST(List)]),
  <<?Tu_sipInfo, 
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == sdp , ?SIZE(Tuple) == 16 ->
  Bin = encodeFromList([?BINARY(?GET_EL(2,Tuple)),
                        ?TUPLE(?GET_EL(3,Tuple)),
                        ?BINARY(?GET_EL(4,Tuple)),
                        ?BINARY(?GET_EL(5,Tuple)),
                        ?BINARY(?GET_EL(6,Tuple)),
                        ?BINARY(?GET_EL(7,Tuple)),
                        ?BINARY(?GET_EL(8,Tuple)),
                        ?TUPLE(?GET_EL(9,Tuple)),
                        ?LIST(?GET_EL(10,Tuple)),
                        ?BINARY(?GET_EL(11,Tuple)),
                        ?LIST(?GET_EL(12,Tuple)),
                        ?BINARY(?GET_EL(13,Tuple)),
                        ?BINARY(?GET_EL(14,Tuple)),
                        ?LIST(?GET_EL(15,Tuple)),
                        ?LIST(?GET_EL(16,Tuple))]),
  <<?Tu_sdp,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == origin , ?SIZE(Tuple) == 7 ->
  Bin = encodeFromList([?BINARY(?GET_EL(2,Tuple)),
                        ?BINARY(?GET_EL(3,Tuple)),
                        ?BINARY(?GET_EL(4,Tuple)),
                        ?BINARY(?GET_EL(5,Tuple)),
                        ?BINARY(?GET_EL(6,Tuple)),
                        ?BINARY(?GET_EL(7,Tuple))]),
  <<?Tu_origin,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == connection , ?SIZE(Tuple) == 4->
  Bin = encodeFromList([?BINARY(?GET_EL(2,Tuple)),
                        ?BINARY(?GET_EL(3,Tuple)),
                        ?BINARY(?GET_EL(4,Tuple))]),
  <<?Tu_connection,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == media_description , ?SIZE(Tuple) == 7 ->
  Bin = encodeFromList([?TUPLE(?GET_EL(2,Tuple)),
                        ?BINARY(?GET_EL(3,Tuple)),
                        ?TUPLE(?GET_EL(4,Tuple)),
                        ?LIST(?GET_EL(5,Tuple)),
                        ?BINARY(?GET_EL(6,Tuple)),
                        ?LIST(?GET_EL(7,Tuple))]),
  <<?Tu_media_description,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == media , ?SIZE(Tuple) == 6 ->
  Bin = encodeFromList([?BINARY(?GET_EL(2,Tuple)),
                        ?BINARY(?GET_EL(3,Tuple)),
                        ?BINARY(?GET_EL(4,Tuple)),
                        ?BINARY(?GET_EL(5,Tuple)),
                        ?LIST(?GET_EL(6,Tuple))]),
  <<?Tu_media,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == attribute , ?SIZE(Tuple) == 4 ->
  Bin = encodeFromList([?BINARY(?GET_EL(2,Tuple)),
                        ?BINARY(?GET_EL(3,Tuple)),
                        ?BINARY(?GET_EL(4,Tuple))]),
  <<?Tu_attribute,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == ss_entity , ?SIZE(Tuple) == 10 ->
  Bin = encodeFromList([?BINARY(?GET_EL(2,Tuple)),
                        ?BINARY(?GET_EL(3,Tuple)),
                        ?INTEGER(?GET_EL(4,Tuple)),
                        ?BOOL(?GET_EL(5,Tuple)),
                        ?BOOL(?GET_EL(6,Tuple)),
                        ?BINARY(?GET_EL(7,Tuple)),
                        ?LIST(?GET_EL(8,Tuple)),
                        ?LIST(?GET_EL(9,Tuple)),
                        ?LIST(?GET_EL(10,Tuple))]),
  <<?Tu_SSEntity,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == format , ?SIZE(Tuple) == 8 ->
  Bin = encodeFromList([?BINARY(?GET_EL(2,Tuple)),
                        ?BINARY(?GET_EL(3,Tuple)),
                        ?BINARY(?GET_EL(4,Tuple))]),
  Version = if
    erlang:is_list(?GET_EL(5,Tuple)) == true ->
      encode_list(?GET_EL(5,Tuple));
    true ->
      encode_binary(?GET_EL(5,Tuple))
  end,
  Bin1 = encodeFromList([?BINARY(?GET_EL(6,Tuple)),
                         ?BOOL(?GET_EL(7,Tuple)),
                         ?BOOL(?GET_EL(8,Tuple))]),
  <<?Tu_format,
    Bin/binary,
    Version/binary,
    Bin1/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'MediaId' , ?SIZE(Tuple) == 2 ->
  Bin = encodeFromList([?BINARY(?GET_EL(2,Tuple))]),
  <<?Tu_MediaId,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'AdditionalISUP' , ?SIZE(Tuple) == 3 ->
  Bin = encodeFromList([?STRING(?GET_EL(2,Tuple)),
                        ?LIST(?GET_EL(3,Tuple))]),
  <<?Tu_AdditionalISUP,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'AdditionalSIP' , ?SIZE(Tuple) == 3 ->
  Bin = encodeFromList([?LIST(?GET_EL(2,Tuple)),
                        ?LIST(?GET_EL(3,Tuple))]),
  <<?Tu_AdditionalSIP,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'AdditionalSIPParam' , ?SIZE(Tuple) == 3 ->
  Bin = encodeFromList([?LIST(?GET_EL(2,Tuple)),
                        ?LIST(?GET_EL(3,Tuple))]),
  <<?Tu_AdditionalSIPParam,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'AdditionalISUPParam' , ?SIZE(Tuple) == 4 ->
  Bin = encodeFromList([?INTEGER(?GET_EL(2,Tuple)),
                        ?INTEGER(?GET_EL(3,Tuple)),
                        ?BINARY(?GET_EL(4,Tuple))]),
  <<?Tu_AdditionalISUPParam,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'SSNotification' , ?SIZE(Tuple) == 9 ->
  Bin = encodeFromList([?UNKNOWN(?GET_EL(2,Tuple)),
                        ?RECORD(?GET_EL(3,Tuple)),
                        ?RECORD(?GET_EL(4,Tuple)),
                        ?ENUM(?E_ServingSide,?GET_EL(5,Tuple)),
                        ?TIME(?GET_EL(6,Tuple)),
                        ?LIST(?GET_EL(7,Tuple)),
                        ?BOOL(?GET_EL(8,Tuple)),
                        ?TUPLE(?GET_EL(9,Tuple))]),
  <<?Tu_SSNotification,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == forSip , ?SIZE(Tuple) == 2 ->
  Bin = encodeFromList([?TUPLE(?GET_EL(2,Tuple))]),
  <<?Tu_forSIP, 
    Bin/binary>>;
encode_tuple({Key, Value} = Tuple) when ?SIZE(Tuple) == 2 ->
  BinKey     = erlang:term_to_binary(Key),
  SizeKey    = ?SIZE(BinKey),
  BinValue   = encode_value(Value),
  <<?Tu_keyvalue, SizeKey, BinKey/binary, BinValue/binary>>;
encode_tuple(<<>>) ->
  <<?B_Undefined>>;
encode_tuple(Term) ->
  ?ENCODE_UNKNOWN(Term).


encode_value(Value) when erlang:is_list(Value) ->
  StrStatus = is_string(Value),
  case StrStatus of
    true -> ?ENCODE_STRING(Value);
    _ -> encode_list(Value)
  end;
encode_value(Value) when erlang:is_tuple(Value) ->
  <<?R_Tuple,(encode_tuple(Value))/binary>>;
encode_value(Value) when erlang:is_binary(Value) ->
  encode_binary(Value);
encode_value(Value) when erlang:is_number(Value) ->
  ?ENCODE_INTEGER(Value);
encode_value(Value) when erlang:is_atom(Value) ->
  ?ENCODE_UNKNOWN(Value);
encode_value(Value) ->
  writeToError(["Unknown value in {Key, Value}",Value]),
  ?ENCODE_UNKNOWN(Value).

encode_integer_any(Int) ->
  BinInt = erlang:term_to_binary(Int, [{compressed,4}]),
  Size = ?SIZE(BinInt),
  Result = if
    Size =< 16#FF ->
      <<?T_Integer8, Size:8, BinInt/binary>>;
    Size =< 16#FFFFFF ->
      <<?T_Integer24, Size:24, BinInt/binary>>;
    true ->
      erlang:throw({error_encode_long_integer, Int})
  end,
  Result.

encode_string(Str) ->
  BinStr = erlang:list_to_binary(Str),
  <<?T_String, (?SIZE(BinStr)), BinStr/binary>>.

encode_bool(Atom) ->
  case Atom of
    false -> <<?T_Bool_false>>;
    true -> <<?T_Bool_true>>;
    undefined -> <<?Undefined>>
  end.

encode_unknown(Term) ->
  BinTerm = erlang:term_to_binary(Term,[{compressed,4}]),
  <<?UNKNOWN,
    (?ENCODE_INTEGER(?SIZE(BinTerm)))/binary,
    BinTerm/binary>>.

encode_time(MegaSec,Sec,MicSec) ->
  BinMeSec = erlang:integer_to_binary(MegaSec),
  BinSec = erlang:integer_to_binary(Sec),
  BinMicSec = erlang:integer_to_binary(MicSec),
  <<?R_EventTime, (?SIZE(BinMeSec)), BinMeSec/binary, (?SIZE(BinSec)), BinSec/binary,
  (?SIZE(BinMicSec)), BinMicSec/binary>>.

%%%%%% Decode
-define(WIRESHARK, 0). % 1 - включено, 0 - выключено

decode(Pack) ->
  case ?WIRESHARK of
      1 ->
        try decode_record(Pack) of
          Packet ->
            case Packet of
              {Fields, Result, Commit, _Bin} ->
                {erlang:length(Fields), Fields, Result, Commit};
              _ ->
                Packet
            end
        catch
          _:_ ->
            log:error("decode error for ~p",[Pack])
        end;
      0 ->
        log:debug("decode args: ~p", [Pack]),
        TimeStart = erlang:monotonic_time(micro_seconds),
        case decode_record(Pack) of
          Packet ->
            case Packet of
              {Result, Commit, _Bin} ->
                writeToLogs(TimeStart, Pack, Result, decode),
                {Result, Commit};
            _ ->
              erlang:throw({decode_error, Pack})
            end
        % catch
        %   _:Error ->
        %     log:error("decode error for ~p: ~p",[Pack,Error])
        end
  end.

decode([binary | T], Bin) ->
  {Binary, Bin1, _} = decode_binary(Bin),
  [Binary] ++ decode(T, Bin1);
decode([integer | T], Bin) ->
  {Integer, Bin1, _} = decode_integer(Bin),
  [Integer] ++ decode(T, Bin1);
decode([record | T], Bin) ->
  case ?WIRESHARK of
    1 ->
      {Record, Bin1, Fields} = decode_record(Bin),
      [{erlang:length(Fields), Fields, Record}] ++ decode(T, Bin1);
    _ ->
      {Record, Bin1, _} = decode_record(Bin),
      [Record] ++ decode(T, Bin1)
  end;
decode([enum | T], Bin) ->
  {Enum, Bin1} = decode_enum(Bin),
  [Enum] ++ decode(T, Bin1);
decode([list | T], Bin) ->
  {List, Bin1, _} = decode_list(Bin),
  [List] ++ decode(T, Bin1);
decode([bool | T], Bin) ->
  {Bool, Bin1, _} = decode_bool(Bin),
  [Bool] ++ decode(T, Bin1);
decode([time | T], Bin) ->
  {Time, Bin1, _} = decode_time(Bin),
  [Time] ++ decode(T, Bin1);
decode([unknown | T], Bin) ->
  {Unknown, Bin1, _} = decode_unknown(Bin),
  [Unknown] ++ decode(T, Bin1);
decode([tuple | T], Bin) ->
  case ?WIRESHARK of
      1 ->
          {Tuple, Bin1, Fields} = decode_tuple(Bin),
          [{erlang:length(Fields), Fields, Tuple}] ++ decode(T, Bin1);
      _ ->
          {Tuple, Bin1, _} = decode_tuple(Bin),
          [Tuple] ++ decode(T, Bin1)
  end;
decode([], <<>>) -> [<<>>];
decode([], Bin) -> [Bin].

decode_record(?DECODE_UNDEFINED) -> {undefined, _Bin, []};
decode_record(<<?UNKNOWN, _/binary>> = Term) -> decode_unknown(Term);
decode_record(<<?R_AcpMessage, ?T_String, Bin/binary>>) ->
  _Bin = <<?T_String, Bin/binary>>,
  [Commit, Uri, CallRef, Body, Bin1] = decode([list, binary, integer, record], _Bin),
  case ?WIRESHARK of
    1 -> {record_info(fields, 'AcpMessage'),
           #'AcpMessage'{
             uri     = Uri,
             callRef = CallRef,
             body    = Body
           }, Commit, Bin1};
    0 -> {#'AcpMessage'{
             uri     = Uri,
             callRef = CallRef,
             body    = Body
           }, Commit, Bin1}
  end;
decode_record(<<?R_AcpMessage, Bin/binary>>) ->
  [Uri, CallRef, Body, Bin1] = decode([binary, integer, record], Bin),
  case ?WIRESHARK of
    1 -> {record_info(fields, 'AcpMessage'),
         #'AcpMessage'{
           uri     = Uri,
           callRef = CallRef,
           body    = Body
         }, undefined, Bin1};
    0 -> {#'AcpMessage'{
           uri     = Uri,
           callRef = CallRef,
           body    = Body
         }, undefined, Bin1}
  end;
decode_record(<<?R_ServiceFeatureInd, Bin/binary>>) ->
  [Arg, Bin1] = decode([record], Bin),
  {#'ServiceFeatureInd'{
    arg = Arg
  }, Bin1, record_info(fields, 'ServiceFeatureInd')};
decode_record(<<?R_ServiceFeatureType, Bin/binary>>) ->
  [Cause, AddInfo, Bin1] = decode([enum, list], Bin),
  {#'ServiceFeatureType'{
    cause          = Cause,
    additionalInfo = AddInfo
  }, Bin1, record_info(fields, 'ServiceFeatureType')};
decode_record(<<?R_SetupReqAck, Bin/binary>>) ->
  [Arg, Bin1] = decode([record], Bin),
  {#'SetupReqAck'{
    arg = Arg
  }, Bin1, record_info(fields, 'SetupReqAck')};
decode_record(<<?R_SetupIndAck, Bin/binary>>) ->
  [Arg, Bin1] = decode([record],Bin),
  {#'SetupIndAck'{
    arg = Arg
  }, Bin1, record_info(fields, 'SetupIndAck')};
decode_record(<<?R_SetupReq, Bin/binary>>) ->
  [Arg, Bin1] = decode([record], Bin),
  {#'SetupReq'{
    arg = Arg
  }, Bin1, record_info(fields, 'SetupReq')};
decode_record(<<?R_SetupAckType, Bin/binary>>) ->
  [Refer, TrunkGroupId, Bin1] = decode([record, record], Bin),
  {#'SetupAckType'{
    refer        = Refer,
    trunkGroupId = TrunkGroupId
  }, Bin1, record_info(fields, 'SetupAckType')};
decode_record(<<?R_CallProgressReq, Bin/binary>>) ->
  [Arg, Bin1] = decode([record], Bin),
  {#'CallProgressReq'{
    arg = Arg
  }, Bin1, record_info(fields, 'CallProgressReq')};
decode_record(<<?R_CallProgressInd, Bin/binary>>) ->
  [Arg, Bin1] = decode([record], Bin),
  {#'CallProgressInd'{
    arg = Arg
  }, Bin1, record_info(fields, 'CallProgressInd')};
decode_record(<<?R_CallProgressType, Bin/binary>>) ->
  [Cause, CauseInitiator, CauseDescription, CauseIsup, EventInformation, 
    AdditionalInfo, Bin1] = decode([enum, enum, list, binary, record, list], Bin),
  [OBCI, GNotification, RedirectionNumber, RedirectionResInd, CallDiversionInfo,
    Bin2] = decode([record, list, record, enum, record], Bin1),
  [Facility, CallId, SDP, TrunkGroupId, CallTransferNumber, Refer, EventTime, 
    Bin3] = decode([list, record, record, record, record, record, time], Bin2),
  {#'CallProgressType'{
    cause              = Cause,
    causeInitiator     = CauseInitiator,
    causeDescription   = CauseDescription,
    causeIsup          = CauseIsup,
    eventInformation   = EventInformation,
    additionalInfo     = AdditionalInfo,
    oBCI               = OBCI,
    gNotification      = GNotification,
    redirectionNumber  = RedirectionNumber,
    redirectionRestInd = RedirectionResInd,
    callDiversionInfo  = CallDiversionInfo,
    facility           = Facility,
    callId             = CallId,
    sdp                = SDP,
    trunkGroupId       = TrunkGroupId,
    callTransferNumber = CallTransferNumber,
    refer              = Refer,
    eventTime          = EventTime
  }, Bin3, record_info(fields, 'CallProgressType')};
decode_record(<<?R_EventInformation, Bin/binary>>) ->
  [EvInd, EvPrInd, Bin1] = decode([enum, enum], Bin),
  {#'EventInformation'{
    eventIndicator             = EvInd,
    eventPresentationIndicator = EvPrInd
  }, Bin1, record_info(fields, 'EventInformation')};
decode_record(<<?R_OptionalBackwardCallIndicators, Bin/binary>>) ->
  [InbII, CallDiversionInd, SimpleSegmentationInf, MlppUserInd, Bin1] =
  decode([enum, enum, enum, enum], Bin),
  {#'OptionalBackwardCallIndicators'{
    inbInfoInd            = InbII,
    callDiversionInd      = CallDiversionInd,
    simpleSegmentationInf = SimpleSegmentationInf,
    mlppUserInd           = MlppUserInd
  }, Bin1, record_info(fields, 'OptionalBackwardCallIndicators')};
decode_record(<<?R_RedirectingNumber, Bin/binary>>) ->
  [Nai, Ni, Incomplete, Npi, Apri, Category, Bin1] = decode([enum, enum, bool, 
    enum, enum, enum], Bin),
  [Digits, DisplayName, SipUri, VDN, Bin2] = decode([list, list, list, list], Bin1),
  {#'RedirectingNumber'{
    nai        = Nai,
    ni         = Ni,
    incomplete = Incomplete,
    npi        = Npi,
    apri       = Apri,
    category   = Category,
    digits     = Digits,
    displayName= DisplayName,
    sipUri     = SipUri,
    vdn        = VDN
  },Bin2, record_info(fields, 'RedirectingNumber')};
decode_record(<<?R_CallDiversionInformation, Bin/binary>>) ->
  [NotifSubsOpts, RedirectingReason, Bin1] = decode([enum, enum], Bin),
  {#'CallDiversionInformation'{
    notificationSubsOpts = NotifSubsOpts,
    redirectingReason    = RedirectingReason
  }, Bin1, record_info(fields, 'CallDiversionInformation')};
decode_record(<<?R_SDPType, Bin/binary>>) ->
  [Type, Body, Bin1] = decode([enum, list], Bin),
  {#'SDPType'{
    type = Type,
    body = Body
  }, Bin1, record_info(fields, 'SDPType')};
decode_record(<<?R_TrunkGroupId, Bin/binary>>) ->
  [TrunkGroupId, TrunkId, PCMId, ChannelNumber, Bin1] = decode([list,
    list, list, integer], Bin),
  {#'TrunkGroupId'{
    trunkGroupId  = TrunkGroupId,
    trunkId       = TrunkId,
    pCMId         = PCMId,
    channelNumber = ChannelNumber
  }, Bin1, record_info(fields, 'TrunkGroupId')};
decode_record(<<?R_CallTransferNumber, Bin/binary>>) ->
  [Nai, Ni, Incomplete, Npi, Apri, Screening, Category, Bin1] = decode([enum, enum, 
    bool, enum, enum, enum, enum], Bin),
  [Digits, DisplayName, SipUri, VDN, Bin2] = decode([list, list, list, list], Bin1),
  {#'CallTransferNumber'{
    nai        = Nai,
    ni         = Ni,
    incomplete = Incomplete,
    npi        = Npi,
    apri       = Apri,
    screening  = Screening,
    category   = Category,
    digits     = Digits,
    displayName= DisplayName,
    sipUri     = SipUri,
    vdn        = VDN
  }, Bin2, record_info(fields, 'CallTransferNumber')};
decode_record(<<?R_Refer, Bin/binary>>) ->
  [List, Bin1] = decode([list],Bin),
  {{refer, List}, Bin1, [refer]};
decode_record(<<?R_ReferType, Bin/binary>>) ->
  [Exchange, RoutingKey, Sid, CallRef, ConfId, Bin1] = decode([list, list, binary,
    integer, list], Bin),
  {#'ReferType'{
    exchange   = Exchange,
    routingKey = RoutingKey,
    sid        = Sid,
    callRef    = CallRef,
    conf_id    = ConfId
  }, Bin1, record_info(fields, 'ReferType')};
decode_record(<<?R_RedirectionNumber, Bin/binary>>) ->
  [Nai, Ni, Incomplete, Inni, Npi, Category, Digits, DisplayName, SipUri, VDN, Bin1] =
    decode([enum, enum, bool, enum, enum, enum, list, list, list, list], Bin),
  {#'RedirectionNumber'{
    nai         = Nai,
    ni          = Ni,
    incomplete  = Incomplete,
    inni        = Inni,
    npi         = Npi,
    category    = Category,
    digits      = Digits,
    displayName = DisplayName,
    sipUri      = SipUri,
    vdn         = VDN
  }, Bin1, record_info(fields, 'RedirectionNumber')};
decode_record(<<?R_ReleaseReq, Bin/binary>>) ->
  [Arg, Bin1] = decode([record], Bin),
  {#'ReleaseReq'{
    arg = Arg
  }, Bin1, record_info(fields, 'ReleaseReq')};
decode_record(<<?R_ReleaseType, Bin/binary>>) ->
  [Cause, CauseInitiator, CauseDescription, Digits, CauseIsup, AdditionalInfo,
    TrunkGroupId, Refer, NeedAck, EventTime, Bin1] = decode([enum, enum,
    list, list, binary, list, record, record, bool, time], Bin),
  {#'ReleaseType'{
    cause            = Cause,
    causeInitiator   = CauseInitiator,
    causeDescription = CauseDescription,
    dialledDigits    = Digits,
    causeIsup        = CauseIsup,
    additionalInfo   = AdditionalInfo,
    trunkGroupId     = TrunkGroupId,
    refer            = Refer,
    need_ack         = NeedAck,
    eventTime        = EventTime
  }, Bin1, record_info(fields, 'ReleaseType')};
decode_record(<<?R_ReleaseInd, Bin/binary>>) ->
  [Arg, Bin1] = decode([record],Bin),
  {#'ReleaseInd'{
    arg = Arg
  }, Bin1, record_info(fields, 'ReleaseInd')};
decode_record(<<?R_CalledPartyNumber, Bin/binary>>) ->
  [Nai, Ni, Incomplete, Inni, Npi, Category, Digits, DisplayName, SipUri, VDN, Bin1] =
    decode([enum, enum, bool, enum, enum, enum, list, list, list, list], Bin),
  {#'CalledPartyNumber'{
    nai         = Nai,
    ni          = Ni,
    incomplete  = Incomplete,
    inni        = Inni,
    npi         = Npi,
    category    = Category,
    digits      = Digits,
    displayName = DisplayName,
    sipUri      = SipUri,
    vdn         = VDN
  }, Bin1, record_info(fields, 'CalledPartyNumber')};
decode_record(<<?R_CallingPartyNumber, Bin/binary>>) ->
  [Nai, Ni, Incomplete, Npi, Apri, Screening, Category, Digits, CRDI, SipUri, VDN, Bin1] =
    decode([enum, enum, bool, enum, enum, enum, enum, list, record, list, list], Bin),
  {#'CallingPartyNumber'{
    nai         = Nai,
    ni          = Ni,
    incomplete  = Incomplete,
    npi         = Npi,
    apri        = Apri,
    screening   = Screening,
    category    = Category,
    digits      = Digits,
    callerDisplayInformation = CRDI,
    sipUri      = SipUri,
    vdn         = VDN
  }, Bin1, record_info(fields, 'CallingPartyNumber')};
decode_record(<<?R_CallerDisplayInformation, Bin/binary>>) ->
  [ShowDisplayName, DisplayName, ShowCallerId, CallerId, Bin1] = decode([enum,
    list, bool, list], Bin),
  {#'CallerDisplayInformation'{
    showDisplayName = ShowDisplayName,
    displayName = DisplayName,
    showCallerId = ShowCallerId,
    callerId = CallerId
  }, Bin1, record_info(fields, 'CallerDisplayInformation')};
decode_record(<<?R_SetupResp, Bin/binary>>) ->
  [Arg, Bin1] = decode([record],Bin),
  {#'SetupResp'{
    arg = Arg
  }, Bin1, record_info(fields, 'SetupResp')};
decode_record(<<?R_SetupCRType, Bin/binary>>) ->
  [ConnectedNumber, AdditionalInfo, RedirectionNumber, RedirectionRestInd, CallId, Sdp,
    Refer, EventTime, Bin1] = decode([record, list, record,
    enum, record, record, record, time], Bin),
  {#'SetupCRType'{
    connectedNumber    = ConnectedNumber,
    additionalInfo     = AdditionalInfo,
    redirectionNumber  = RedirectionNumber,
    redirectionRestInd = RedirectionRestInd,
    callId             = CallId,
    sdp                = Sdp,
    refer              = Refer,
    eventTime          = EventTime
  }, Bin1, record_info(fields, 'SetupCRType')};
decode_record(<<?R_OriginalCalledNumber, Bin/binary>>) ->
  [Nai, Ni, Incomplete, Npi, Apri, Category, Digits, DisplayName, SipUri, VDN, Bin1] =
    decode([enum, enum, bool, enum, enum, enum, list, list, list, list], Bin),
  {#'OriginalCalledNumber'{
    nai         = Nai,
    ni          = Ni,
    incomplete  = Incomplete,
    npi         = Npi,
    apri        = Apri,
    category    = Category,
    digits      = Digits,
    displayName = DisplayName,
    sipUri      = SipUri,
    vdn         = VDN
  }, Bin1, record_info(fields, 'OriginalCalledNumber')};
decode_record(<<?R_RedirectionInformation, Bin/binary>>) ->
  [OriginalRedirectionReason, RedirectingIndicator, RedirectingReason, RedirectionCounter,
    Bin1] = decode([enum, enum, enum, enum], Bin),
  {#'RedirectionInformation'{
    originalRedirectionReason = OriginalRedirectionReason,
    redirectingIndicator      = RedirectingIndicator,
    redirectingReason         = RedirectingReason,
    redirectionCounter        = RedirectionCounter
  }, Bin1, record_info(fields, 'RedirectionInformation')};
decode_record(<<?R_MLPPPrecedence, Bin/binary>>) ->
  [Lfb, PrecedenceLevel, NetworkIdentity, MlppServiceDomain, Bin1] = decode([
    integer, integer, integer, integer], Bin),
  {#'MLPPPrecedence'{
    lfb                 = Lfb,
    precedenceLevel     = PrecedenceLevel,
    network_identity    = NetworkIdentity,
    mlpp_service_domain = MlppServiceDomain
  }, Bin1, record_info(fields, 'MLPPPrecedence')};
decode_record(<<?R_SetupInd, Bin/binary>>) ->
  [Arg, Bin1] = decode([record],Bin),
  {#'SetupInd'{
    arg = Arg
  }, Bin1, record_info(fields, 'SetupInd')};
decode_record(<<?R_SetupConf, Bin/binary>>) ->
  [Arg, Bin1] = decode([record],Bin),
  {#'SetupConf'{
    arg = Arg
  }, Bin1, record_info(fields, 'SetupConf')};
decode_record(<<?R_CallId, Bin/binary>>) ->
  [Id, FromTag, ToTag, Bin1] = decode([binary, binary, binary], Bin),
  {#'CallId'{
    id = Id,
    fromTag = FromTag,
    toTag = ToTag
  }, Bin1, record_info(fields, 'CallId')};
decode_record(<<?R_SubsequentAddressReq, Bin/binary>>) ->
  [Arg, Bin1] = decode([record], Bin),
  {#'SubsequentAddressReq'{
    arg = Arg
  }, Bin1, record_info(fields, 'SubsequentAddressReq')};
decode_record(<<?R_SubsequentAddressType, Bin/binary>>) ->
  [Digits, Sdp, Bin1] = decode([list, record], Bin),
  {#'SubsequentAddressType'{
    digits = Digits,
    sdp = Sdp
  }, Bin1, record_info(fields, 'SubsequentAddressType')};
decode_record(<<?R_LocationNumber, Bin/binary>>) ->
  [Nai, Ni, Incomplete, Npi, Apri, Screening, Category, Digits, DisplayName, CallerId,
    SipUri, VDN, Bin1] = decode([enum, enum, bool, enum, enum, enum, enum,
    list, list, list, list, list], Bin),
  {#'LocationNumber'{
    nai         = Nai,
    ni          = Ni,
    incomplete  = Incomplete,
    npi         = Npi,
    apri        = Apri,
    screening   = Screening,
    category    = Category,
    digits      = Digits,
    displayName = DisplayName,
    callerId    = CallerId,
    sipUri      = SipUri,
    vdn         = VDN
  }, Bin1, record_info(fields, 'LocationNumber')};
decode_record(<<?R_SetupIRType, Bin/binary>>) ->
  [Domain, CalledPartyNumber, CallingPartyNumber, LocationNumber, OriginalCalledNumber,
    UserTeleserviceInformation, Bin1] = decode([list, record, record, record, record, list], Bin),
  [GenericNumbers, ForwardCallIndicators, RedirectingNumber, RedirectionInformation, 
    USIServiceIndicator, USIInformation, Bin2] = decode([list, list, record, record, unknown, list], Bin1),
  [ISUPCallReference, CallId, SDPType, MediaPoint, AdditionalInfo, TrunkGroupId, CallingPartyInfo,
    CalledPartyInfo, CallingIfaceInfo, CalledIfaceInfo, Refer, SetupModeType, Priority,
    EventTime, Bin3] = decode([list, record, record, binary, list, record, list,
    list, list, list, record, enum, record, time], Bin2),
  {#'SetupIRType'{
    domain                     = Domain,
    calledPartyNumber          = CalledPartyNumber,
    callingPartyNumber         = CallingPartyNumber,
    locationNumber             = LocationNumber,
    originalCalledNumber       = OriginalCalledNumber,
    userTeleserviceInformation = UserTeleserviceInformation,
    genericNumber              = GenericNumbers,
    forwardCallIndicators      = ForwardCallIndicators,
    redirectingNumber          = RedirectingNumber,
    redirectingInformation     = RedirectionInformation,
    uSIServiceIndicator        = USIServiceIndicator,
    uSIInformation             = USIInformation,
    isupCallRef                = ISUPCallReference,
    callId                     = CallId,
    sdp                        = SDPType,
    mediaPoint                 = MediaPoint,
    additionalInfo             = AdditionalInfo,
    trunkGroupId               = TrunkGroupId,
    callingPartyInfo           = CallingPartyInfo,
    calledPartyInfo            = CalledPartyInfo,
    callingIfaceInfo           = CallingIfaceInfo,
    calledIfaceInfo            = CalledIfaceInfo,
    refer                      = Refer,
    mode                       = SetupModeType,
    priority                   = Priority,
    eventTime                  = EventTime
  }, Bin3, record_info(fields, 'SetupIRType')}.

decode_enum(?DECODE_UNKNOWN = BinTerm) -> 
    {Res, Bin, _} = decode_unknown(BinTerm),
    {Res, Bin};
decode_enum(?DECODE_UNDEFINED) -> {undefined, _Bin};
decode_enum(<<?E_CauseLittle, 0, Bin/binary>>) -> {flash, Bin};
decode_enum(<<?E_CauseLittle, 1, Bin/binary>>) -> {refer, Bin};
decode_enum(<<?E_Cause, 1, Bin/binary>>)  -> {normal, Bin};
decode_enum(<<?E_Cause, 2, Bin/binary>>)  -> {originationDenied, Bin};
decode_enum(<<?E_Cause, 3, Bin/binary>>)  -> {collectDigits, Bin};
decode_enum(<<?E_Cause, 4, Bin/binary>>)  -> {authorisationFailure, Bin};
decode_enum(<<?E_Cause, 5, Bin/binary>>)  -> {bPtyAlerted, Bin};
decode_enum(<<?E_Cause, 6, Bin/binary>>)  -> {noIndication, Bin};
decode_enum(<<?E_Cause, 7, Bin/binary>>)  -> {aPtyAbandon, Bin};
decode_enum(<<?E_Cause, 8, Bin/binary>>)  -> {invalidCollectedInformation, Bin};
decode_enum(<<?E_Cause, 9, Bin/binary>>)  -> {collectInformationFailure, Bin};
decode_enum(<<?E_Cause, 10, Bin/binary>>) -> {aPtyDisc, Bin};
decode_enum(<<?E_Cause, 11, Bin/binary>>) -> {bPtyDisc, Bin};
decode_enum(<<?E_Cause, 12, Bin/binary>>) -> {routeSelectFailure, Bin};
decode_enum(<<?E_Cause, 13, Bin/binary>>) -> {oNoAnswer, Bin};
decode_enum(<<?E_Cause, 14, Bin/binary>>) -> {terminationDenied, Bin};
decode_enum(<<?E_Cause, 15, Bin/binary>>) -> {notReachable, Bin};
decode_enum(<<?E_Cause, 16, Bin/binary>>) -> {bPtyNoAnswer, Bin};
decode_enum(<<?E_Cause, 17, Bin/binary>>) -> {bPtyBusyUDUB, Bin};
decode_enum(<<?E_Cause, 18, Bin/binary>>) -> {bPtyBusyNDUB, Bin};
decode_enum(<<?E_Cause, 19, Bin/binary>>) -> {ss7Failure, Bin};
decode_enum(<<?E_Cause, 20, Bin/binary>>) -> {calledPartyRejected, Bin};
decode_enum(<<?E_Cause, 21, Bin/binary>>) -> {tException, Bin};
decode_enum(<<?E_Cause, 22, Bin/binary>>) -> {routeFailure1, Bin};
decode_enum(<<?E_Cause, 23, Bin/binary>>) -> {routeFailure2, Bin};
decode_enum(<<?E_Cause, 24, Bin/binary>>) -> {ssActivating, Bin};
decode_enum(<<?E_Cause, 25, Bin/binary>>) -> {conversationTimeout, Bin};
decode_enum(<<?E_Cause, 26, Bin/binary>>) -> {noCircuitAvailable, Bin};
decode_enum(<<?E_Cause, 27, Bin/binary>>) -> {coreNotification, Bin};
decode_enum(<<?E_Cause, 28, Bin/binary>>) -> {unsupportedMedia, Bin};
decode_enum(<<?E_Cause, 29, Bin/binary>>) -> {numberIncomplete, Bin};
decode_enum(<<?E_Cause, 30, Bin/binary>>) -> {invalidNumber, Bin};
decode_enum(<<?E_Cause, 31, Bin/binary>>) -> {unassignedNumber, Bin};
decode_enum(<<?E_Cause, 32, Bin/binary>>) -> {doNotDisturb, Bin};
decode_enum(<<?E_Cause, 33, Bin/binary>>) -> {externalControlled, Bin};
decode_enum(<<?E_Cause, 34, Bin/binary>>) -> {systemFailure, Bin};
decode_enum(<<?E_Cause, 35, Bin/binary>>) -> {applicationNotification, Bin};
decode_enum(<<?E_Cause, 36, Bin/binary>>) -> {refer_ok, Bin};
decode_enum(<<?E_Cause, 37, Bin/binary>>) -> {refer_failure, Bin};
decode_enum(<<?E_Cause, 38, Bin/binary>>) -> {uaPreemption, Bin};
decode_enum(<<?E_Cause, 39, Bin/binary>>) -> {reservedResourcesPreemted, Bin};
decode_enum(<<?E_Cause, 40, Bin/binary>>) -> {genericPreemption, Bin};
decode_enum(<<?E_Cause, 41, Bin/binary>>) -> {nonIpPreemption, Bin};
decode_enum(<<?E_Cause, 42, Bin/binary>>) -> {hole, Bin};
decode_enum(<<?E_Cause, 43, Bin/binary>>) -> {hold, Bin};
decode_enum(<<?E_Cause, 44, Bin/binary>>) -> {session_timeout, Bin};
decode_enum(<<?E_CauseInitiator, 0, Bin/binary>>) -> {user, Bin};
decode_enum(<<?E_CauseInitiator, 1, Bin/binary>>) -> {isup_network, Bin};
decode_enum(<<?E_CauseInitiator, 2, Bin/binary>>) -> {non_isup_network, Bin};
decode_enum(<<?E_CauseInitiator, 3, Bin/binary>>) -> {system, Bin};
decode_enum(<<?E_EventIndicator, 0, Bin/binary>>) -> {alerting, Bin};
decode_enum(<<?E_EventIndicator, 1, Bin/binary>>) -> {progress, Bin};
decode_enum(<<?E_EventIndicator, 2, Bin/binary>>) -> {in_band_info, Bin};
decode_enum(<<?E_EventIndicator, 3, Bin/binary>>) -> {cfb, Bin};
decode_enum(<<?E_EventIndicator, 4, Bin/binary>>) -> {cfnr, Bin};
decode_enum(<<?E_EventIndicator, 5, Bin/binary>>) -> {cfu, Bin};
decode_enum(<<?E_EventPresentationRestrictedIndicator, 0, Bin/binary>>) -> {no_indication, Bin};
decode_enum(<<?E_EventPresentationRestrictedIndicator, 1, Bin/binary>>) -> {restricted, Bin};
decode_enum(<<?E_InBandInfoIndicator, 0, Bin/binary>>) -> {noIndication, Bin};
decode_enum(<<?E_InBandInfoIndicator, 1, Bin/binary>>) -> {inBandInfoOrPatternAvailable, Bin};
decode_enum(<<?E_CallDiversionIndicator, 0, Bin/binary>>) -> {noIndication, Bin};
decode_enum(<<?E_CallDiversionIndicator, 1, Bin/binary>>) -> {callDiversionMayOccur, Bin};
decode_enum(<<?E_SimpleSegmentationIndicator, 0, Bin/binary>>) -> {noAdditionalInformation, Bin};
decode_enum(<<?E_SimpleSegmentationIndicator, 1, Bin/binary>>) -> {additionalInformationWillBeSent, Bin};
decode_enum(<<?E_MLPPUserIndicator, 0, Bin/binary>>) -> {noIndication, Bin};
decode_enum(<<?E_MLPPUserIndicator, 1, Bin/binary>>) -> {mlppUser, Bin};
decode_enum(<<?E_NAIType, 0, Bin/binary>>) -> {spare, Bin};
decode_enum(<<?E_NAIType, 1, Bin/binary>>) -> {subscriberNumber, Bin};
decode_enum(<<?E_NAIType, 2, Bin/binary>>) -> {unknown, Bin};
decode_enum(<<?E_NAIType, 3, Bin/binary>>) -> {nationalNumber, Bin};
decode_enum(<<?E_NAIType, 4, Bin/binary>>) -> {internationNumber, Bin};
decode_enum(<<?E_NIType, 0, Bin/binary>>) -> {private, Bin};
decode_enum(<<?E_NIType, 1, Bin/binary>>) -> {local, Bin};
decode_enum(<<?E_NIType, 2, Bin/binary>>) -> {zone, Bin};
decode_enum(<<?E_NIType, 3, Bin/binary>>) -> {intercity, Bin};
decode_enum(<<?E_NIType, 4, Bin/binary>>) -> {international, Bin};
decode_enum(<<?E_NIType, 5, Bin/binary>>) -> {emergency, Bin};
decode_enum(<<?E_INNIType, 0, Bin/binary>>) -> {routingToInternalNumberAllowed, Bin};
decode_enum(<<?E_INNIType, 1, Bin/binary>>) -> {routingToInternalNumberNotAllowed, Bin};
decode_enum(<<?E_NPIType, 0, Bin/binary>>) -> {spare, Bin};
decode_enum(<<?E_NPIType, 1, Bin/binary>>) -> {isdnTelephony, Bin};
decode_enum(<<?E_NPIType, 2, Bin/binary>>) -> {dataNumberingPlan, Bin};
decode_enum(<<?E_NPIType, 3, Bin/binary>>) -> {telexNumberingPlan, Bin};
decode_enum(<<?E_NPIType, 4, Bin/binary>>) -> {reserved1, Bin};
decode_enum(<<?E_NPIType, 5, Bin/binary>>) -> {reserved2, Bin};
decode_enum(<<?E_NPIType, 6, Bin/binary>>) -> {reserved3, Bin};
decode_enum(<<?E_APRIType, 0, Bin/binary>>) -> {presentationAllowed, Bin};
decode_enum(<<?E_APRIType, 1, Bin/binary>>) -> {presentationRestricted, Bin};
decode_enum(<<?E_APRIType, 2, Bin/binary>>) -> {addressNotAvailable, Bin};
decode_enum(<<?E_APRIType, 3, Bin/binary>>) -> {spare, Bin};
decode_enum(<<?E_RedirectionRestrictionIndicator, 0, Bin/binary>>) -> {presentation_allowed, Bin};
decode_enum(<<?E_RedirectionRestrictionIndicator, 1, Bin/binary>>) -> {presentation_restricted, Bin};
decode_enum(<<?E_NotificationSubscriptionOptions, 0, Bin/binary>>) -> {unknown, Bin};
decode_enum(<<?E_NotificationSubscriptionOptions, 1, Bin/binary>>) -> {presentation_not_allowed, Bin};
decode_enum(<<?E_NotificationSubscriptionOptions, 2, Bin/binary>>) -> {presentation_allowed_with_redirecting_number, Bin};
decode_enum(<<?E_NotificationSubscriptionOptions, 3, Bin/binary>>) -> {presentation_allowed_without_redirecting_number, Bin};
decode_enum(<<?E_RedirectingReason, 0, Bin/binary>>)  -> {unknown, Bin};
decode_enum(<<?E_RedirectingReason, 1, Bin/binary>>)  -> {busy, Bin};
decode_enum(<<?E_RedirectingReason, 2, Bin/binary>>)  -> {noReply, Bin};
decode_enum(<<?E_RedirectingReason, 3, Bin/binary>>)  -> {unconditional, Bin};
decode_enum(<<?E_RedirectingReason, 4, Bin/binary>>)  -> {deflectAlert, Bin};
decode_enum(<<?E_RedirectingReason, 5, Bin/binary>>)  -> {deflectResp, Bin};
decode_enum(<<?E_RedirectingReason, 6, Bin/binary>>)  -> {notReachable, Bin};
decode_enum(<<?E_RedirectingReason, 7, Bin/binary>>)  -> {timeOfDay, Bin};
decode_enum(<<?E_RedirectingReason, 8, Bin/binary>>)  -> {doNotDisturb, Bin};
decode_enum(<<?E_RedirectingReason, 9, Bin/binary>>)  -> {followMe, Bin};
decode_enum(<<?E_RedirectingReason, 10, Bin/binary>>) -> {outOfService, Bin};
decode_enum(<<?E_RedirectingReason, 11, Bin/binary>>) -> {away, Bin};
decode_enum(<<?E_RedirectingReason, 16#FF, Bin/binary>>) -> {undefined, Bin};
decode_enum(<<?E_TypeSDPType, 0, Bin/binary>>) -> {offer, Bin};
decode_enum(<<?E_TypeSDPType, 1, Bin/binary>>) -> {answer, Bin};
decode_enum(<<?E_TypeSDPType, 2, Bin/binary>>) -> {unknown, Bin};
decode_enum(<<?E_ScreeningType, 0, Bin/binary>>) -> {userProvidedNotVerified, Bin};
decode_enum(<<?E_ScreeningType, 1, Bin/binary>>) -> {userProvidedVerifiedAndPassed, Bin};
decode_enum(<<?E_ScreeningType, 2, Bin/binary>>) -> {userProvidedVerifiedAndFailed, Bin};
decode_enum(<<?E_ScreeningType, 3, Bin/binary>>) -> {networkProvided, Bin};
decode_enum(<<?E_CalledPartysCategory, 0, Bin/binary>>)  -> {unknownAtThisTime, Bin};
decode_enum(<<?E_CalledPartysCategory, 1, Bin/binary>>)  -> {operatorFrench, Bin};
decode_enum(<<?E_CalledPartysCategory, 2, Bin/binary>>)  -> {operatorEngish, Bin};
decode_enum(<<?E_CalledPartysCategory, 3, Bin/binary>>)  -> {operatorGerman, Bin};
decode_enum(<<?E_CalledPartysCategory, 4, Bin/binary>>)  -> {operatorRussian, Bin};
decode_enum(<<?E_CalledPartysCategory, 5, Bin/binary>>)  -> {operatorSpanish, Bin};
decode_enum(<<?E_CalledPartysCategory, 6, Bin/binary>>)  -> {reserved, Bin};
decode_enum(<<?E_CalledPartysCategory, 7, Bin/binary>>)  -> {ordinarySubscriber, Bin};
decode_enum(<<?E_CalledPartysCategory, 8, Bin/binary>>)  -> {subscriberWithPriority, Bin};
decode_enum(<<?E_CalledPartysCategory, 9, Bin/binary>>)  -> {dataCall, Bin};
decode_enum(<<?E_CalledPartysCategory, 10, Bin/binary>>) -> {testCall, Bin};
decode_enum(<<?E_CalledPartysCategory, 11, Bin/binary>>) -> {spare, Bin};
decode_enum(<<?E_CalledPartysCategory, 12, Bin/binary>>) -> {payphone, Bin};
decode_enum(<<?E_CalledPartysCategory, 13, Bin/binary>>) -> {category0, Bin};
decode_enum(<<?E_CalledPartysCategory, 14, Bin/binary>>) -> {hotelsSubscriber, Bin};
decode_enum(<<?E_CalledPartysCategory, 15, Bin/binary>>) -> {freeSubscriber, Bin};
decode_enum(<<?E_CalledPartysCategory, 16, Bin/binary>>) -> {paidSubscriber, Bin};
decode_enum(<<?E_CalledPartysCategory, 17, Bin/binary>>) -> {localSubscriber, Bin};
decode_enum(<<?E_CalledPartysCategory, 18, Bin/binary>>) -> {localTaksofon, Bin};
decode_enum(<<?E_CalledPartysCategory, 19, Bin/binary>>) -> {autoCallI, Bin};
decode_enum(<<?E_CalledPartysCategory, 20, Bin/binary>>) -> {semiautoCallI, Bin};
decode_enum(<<?E_CalledPartysCategory, 21, Bin/binary>>) -> {autoCallII, Bin};
decode_enum(<<?E_CalledPartysCategory, 22, Bin/binary>>) -> {semiautoCallII, Bin};
decode_enum(<<?E_CalledPartysCategory, 23, Bin/binary>>) -> {autoCallIII, Bin};
decode_enum(<<?E_CalledPartysCategory, 24, Bin/binary>>) -> {semiautoCallIII, Bin};
decode_enum(<<?E_CalledPartysCategory, 25, Bin/binary>>) -> {autoCallIV, Bin};
decode_enum(<<?E_CalledPartysCategory, 26, Bin/binary>>) -> {semiautoCallIV, Bin};
decode_enum(<<?E_CallingPartysCategory, 0, Bin/binary>>)  -> {unknownAtThisTime, Bin};
decode_enum(<<?E_CallingPartysCategory, 1, Bin/binary>>)  -> {operatorFrench, Bin};
decode_enum(<<?E_CallingPartysCategory, 2, Bin/binary>>)  -> {operatorEngish, Bin};
decode_enum(<<?E_CallingPartysCategory, 3, Bin/binary>>)  -> {operatorGerman, Bin};
decode_enum(<<?E_CallingPartysCategory, 4, Bin/binary>>)  -> {operatorRussian, Bin};
decode_enum(<<?E_CallingPartysCategory, 5, Bin/binary>>)  -> {operatorSpanish, Bin};
decode_enum(<<?E_CallingPartysCategory, 6, Bin/binary>>)  -> {reserved, Bin};
decode_enum(<<?E_CallingPartysCategory, 7, Bin/binary>>)  -> {ordinarySubscriber, Bin};
decode_enum(<<?E_CallingPartysCategory, 8, Bin/binary>>)  -> {subscriberWithPriority, Bin};
decode_enum(<<?E_CallingPartysCategory, 9, Bin/binary>>)  -> {dataCall, Bin};
decode_enum(<<?E_CallingPartysCategory, 10, Bin/binary>>) -> {testCall, Bin};
decode_enum(<<?E_CallingPartysCategory, 11, Bin/binary>>) -> {spare, Bin};
decode_enum(<<?E_CallingPartysCategory, 12, Bin/binary>>) -> {payphone, Bin};
decode_enum(<<?E_CallingPartysCategory, 13, Bin/binary>>) -> {category0, Bin};
decode_enum(<<?E_CallingPartysCategory, 14, Bin/binary>>) -> {hotelsSubscriber, Bin};
decode_enum(<<?E_CallingPartysCategory, 15, Bin/binary>>) -> {freeSubscriber, Bin};
decode_enum(<<?E_CallingPartysCategory, 16, Bin/binary>>) -> {paidSubscriber, Bin};
decode_enum(<<?E_CallingPartysCategory, 17, Bin/binary>>) -> {localSubscriber, Bin};
decode_enum(<<?E_CallingPartysCategory, 18, Bin/binary>>) -> {localTaksofon, Bin};
decode_enum(<<?E_CallingPartysCategory, 19, Bin/binary>>) -> {autoCallI, Bin};
decode_enum(<<?E_CallingPartysCategory, 20, Bin/binary>>) -> {semiautoCallI, Bin};
decode_enum(<<?E_CallingPartysCategory, 21, Bin/binary>>) -> {autoCallII, Bin};
decode_enum(<<?E_CallingPartysCategory, 22, Bin/binary>>) -> {semiautoCallII, Bin};
decode_enum(<<?E_CallingPartysCategory, 23, Bin/binary>>) -> {autoCallIII, Bin};
decode_enum(<<?E_CallingPartysCategory, 24, Bin/binary>>) -> {semiautoCallIII, Bin};
decode_enum(<<?E_CallingPartysCategory, 25, Bin/binary>>) -> {autoCallIV, Bin};
decode_enum(<<?E_CallingPartysCategory, 26, Bin/binary>>) -> {semiautoCallIV, Bin};
decode_enum(<<?E_SetupModeType, 0, Bin/binary>>) -> {normal, Bin};
decode_enum(<<?E_SetupModeType, 1, Bin/binary>>) -> {dummy, Bin};
decode_enum(<<?E_SetupModeType, 2, Bin/binary>>) -> {internal, Bin};
decode_enum(<<?E_SetupModeType, 3, Bin/binary>>) -> {callback, Bin};
decode_enum(<<?E_SetupModeType, 4, Bin/binary>>) -> {parking, Bin};
decode_enum(<<?E_SetupModeType, 5, Bin/binary>>) -> {supervise, Bin};
decode_enum(<<?E_SetupModeType, 6, Bin/binary>>) -> {acd, Bin};
decode_enum(<<?E_OriginalRedirectionReason, 0, Bin/binary>>) -> {unknown, Bin};
decode_enum(<<?E_OriginalRedirectionReason, 1, Bin/binary>>) -> {busy, Bin};
decode_enum(<<?E_OriginalRedirectionReason, 2, Bin/binary>>) -> {noReply, Bin};
decode_enum(<<?E_OriginalRedirectionReason, 3, Bin/binary>>) -> {unconditional, Bin};
decode_enum(<<?E_OriginalRedirectionReason, 16#FF, Bin/binary>>) -> {undefined, Bin};
decode_enum(<<?E_RedirectingIndicator, 0, Bin/binary>>) -> {noRedirection, Bin};
decode_enum(<<?E_RedirectingIndicator, 1, Bin/binary>>) -> {reRouted, Bin};
decode_enum(<<?E_RedirectingIndicator, 2, Bin/binary>>) -> {reRoutedAllRestricted, Bin};
decode_enum(<<?E_RedirectingIndicator, 3, Bin/binary>>) -> {diversion, Bin};
decode_enum(<<?E_RedirectingIndicator, 4, Bin/binary>>) -> {diversionAllRestricted, Bin};
decode_enum(<<?E_RedirectingIndicator, 5, Bin/binary>>) -> {reRoutedRestricted, Bin};
decode_enum(<<?E_RedirectingIndicator, 6, Bin/binary>>) -> {diversionRestricted, Bin};
decode_enum(<<?E_RedirectingIndicator, 7, Bin/binary>>) -> {spare, Bin};
decode_enum(<<?E_RedirectingIndicator, 16#FF:8, Bin/binary>>) -> {undefined, Bin};
decode_enum(<<?E_RedirectionCounter, 16#FF:8, Bin/binary>>) -> {undefined, Bin};
decode_enum(<<?E_RedirectionCounter, Num:8, Bin/binary>>) -> {Num, Bin};
decode_enum(<<?E_ServingSide, 0, Bin/binary>>) -> {calling, Bin};
decode_enum(<<?E_ServingSide, 1, Bin/binary>>) -> {called, Bin}.


decode_binary(?DECODE_UNKNOWN = BinTerm) -> decode_unknown(BinTerm);
decode_binary(?DECODE_UNDEFINED) -> {undefined, _Bin, []};
decode_binary(?DECODE_BINARY) -> {_Bin, _Rest, []};
decode_binary(?DECODE_LONG_BINARY) -> {_Bin, _Rest, []}.


decode_integer(?DECODE_UNDEFINED) -> {undefined, _Bin, []};
decode_integer(?DECODE_UNKNOWN = BinTerm) -> decode_unknown(BinTerm);
decode_integer(?DECODE_INTEGER) ->
  BinInt = binary:part(_Bin, {0,_Size}),
  Int    = erlang:binary_to_term(BinInt),
  {Int, binary:part(_Bin, {_Size, ?SIZE(_Bin) - _Size}), []};
decode_integer(?DECODE_LONG_INTEGER) ->
  {binary_to_term(_Bin), _Rest, []}.


decode_time(?DECODE_TIME) ->
  MeSec  = erlang:binary_to_integer(_BinMeSec),
  Sec    = erlang:binary_to_integer(_BinSec),
  MicSec = erlang:binary_to_integer(_BinMicSec),
  {{MeSec, Sec, MicSec}, _Bin, [eventTime]}.


decode_list(?DECODE_UNKNOWN = BinTerm) -> decode_unknown(BinTerm);
decode_list(?DECODE_UNDEFINED) -> {undefined, _Bin, []};
decode_list(<<?T_List, 0, Rest/binary>>) -> {[], Rest, []};
decode_list(?DECODE_STRING) -> % строка
  BinElem = binary:part(_Bin, {0,_Size}),
  Res     = erlang:binary_to_list(BinElem),
  Bin2    = binary:part(_Bin, {_Size, ?SIZE(_Bin) - _Size}),
  {Res, Bin2, []};
decode_list(?DECODE_LIST) ->
  {Size, Bin1, _} = decode_integer(_Bin),
  BinElem         = binary:part(Bin1, {0,Size}),
  Res             = decode_list_(BinElem,[],[]),
  Bin2            = binary:part(Bin1, {Size, ?SIZE(Bin1) - Size}),
  {Res, Bin2, []}.


decode_list_(?DECODE_UNKNOWN = Temp, Res, _) ->
  {El, Rest, _} = decode_unknown(Temp),
  decode_list_(Rest,Res ++ [El], unknown);
decode_list_(<<?R_Tuple, Bin/binary>>, Res, _) ->
  {Tuple, Bin1, _} = decode_tuple(Bin),
  decode_list_(Bin1, Res ++ [Tuple], tuple);
decode_list_(?DECODE_INTEGER = Temp, Res, _) ->
  {El, Rest, _} = decode_integer(Temp),
  decode_list_(Rest, Res ++ [El], number);
decode_list_(?DECODE_LIST = Temp, Res, _) ->
  {El, Rest, _} = decode_list(Temp),
  decode_list_(Rest, Res ++ [[El]], list);
decode_list_(?DECODE_BINARY = Temp, Res, _) ->
  {El, Rest, _} = decode_binary(Temp),
  decode_list_(Rest, Res ++ [El], binary);
decode_list_(?DECODE_STRING, Res, _) ->
  BinElem = binary:part(_Bin, {0,_Size}),
  El = erlang:binary_to_list(BinElem),
  Bin2 = binary:part(_Bin, {_Size, ?SIZE(_Bin) - _Size}),
  decode_list_(Bin2, Res ++ [El], binary);
decode_list_(<<>>, Res, _) -> Res;
decode_list_(<<Bin/binary>>, Res, Flag) ->
  case Flag of
    list ->
      {El, Rest, _} = decode_list(<<?T_List,Bin/binary>>),
      decode_list_(Rest, Res ++ [[El]], list);
    _ ->
      {Size, Bin1, _} = decode_integer(Bin),
      BinElem = binary:part(Bin1, {0, Size}),
      Res = erlang:binary_to_term(BinElem),
      Bin2 = binary:part(Bin1, {Size, ?SIZE(Bin1) - Size}),
      decode_list_(Bin2, Res ++ [Res], Flag)
  end.


decode_tuple(<<?Tu_sipInfo, Bin/binary>>) ->
  [List, Bin1] = decode([list], Bin),
  {{sipInfo, List}, Bin1, [sipinfo]};
decode_tuple(<<?Tu_sdp, Bin/binary>>) ->
  [Version, Origin, Title, Arg1, Arg2, Arg3, Arg4, Bin1] = decode([binary, tuple, 
    binary, binary, binary, binary, binary], Bin),
  [Connection, List1, Arg5, List2, Arg6, Arg7, List3, List4, Bin2] = decode([tuple, 
    list, binary, list, binary, binary, list, list], Bin1),
  {{sdp,
    Version,
    Origin,
    Title,
    Arg1,
    Arg2,
    Arg3,
    Arg4,
    Connection,
    List1,
    Arg5,
    List2,
    Arg6,
    Arg7,
    List3,
    List4}, Bin2, [version, origin, title, arg1, arg2, arg3, arg4, connection, 
    list1, arg5, list2, arg6, arg7, list3, list4]};
decode_tuple(<<?Tu_SSNotification, Bin/binary>>) ->
  [SSFamily, CGPN, CDPN, ServingSide, ServiceTimeStamp, Participants, Internal,
    Args, Bin1] = decode([unknown, record, record, enum, time, list, bool, tuple], Bin),
  {{'SSNotification',
    SSFamily,
    CGPN,
    CDPN,
    ServingSide,
    ServiceTimeStamp,
    Participants,
    Internal,
    Args}, Bin1, [ssfamily, cgpn, cdpn, servingSide, serviceTimeStamp, participants,
      internal, args]};
decode_tuple(<<?Tu_origin, Bin/binary>>) ->
  [Symbol, Num1, Num2, Type, Prot, Ip, Bin1] = decode([binary, binary, binary,
    binary, binary, binary], Bin),
  {{origin,
    Symbol,
    Num1,
    Num2,
    Type,
    Prot,
    Ip}, Bin1, [symbol, num1, num2, type, prot, ip]};
decode_tuple(<<?Tu_connection, Bin/binary>>) ->
  [Type, Prot, Ip, Bin1] = decode([binary, binary, binary], Bin),
  {{connection,
    Type,
    Prot,
    Ip}, Bin1, [type, prot, ip]};
decode_tuple(<<?Tu_SSEntity, Bin/binary>>) ->
  [Atom1, Atom2, Number, Bool1, Bool2, Arg1, List1, List2, List3, Bin1] = decode([binary,
    binary, integer, bool, bool, binary, list, list, list], Bin),
  {{ss_entity,
    Atom1,
    Atom2,
    Number,
    Bool1,
    Bool2,
    Arg1,
    List1,
    List2,
    List3}, Bin1, [atom1, atom2, number, bool1, bool2, arg1, list1, list2, list3]};
decode_tuple(<<?Tu_media_description, Bin/binary>>) ->
  [Media, Arg1, Arg2, List1, Arg3, List2, Bin1] = decode([tuple, binary, tuple, list, 
    binary, list], Bin),
  {{media_description,
    Media,
    Arg1,
    Arg2,
    List1,
    Arg3,
    List2}, Bin1, [media, arg1, arg2, list1, arg3, list2]};
decode_tuple(<<?Tu_media, Bin/binary>>) ->
  [Type, Size, Arg, Prot, Formats, Bin1] = decode([binary, binary, binary, binary, list], Bin),
  {{media,
    Type,
    Size,
    Arg,
    Prot,
    Formats}, Bin1, [type, size, arg, prot, formats]};
decode_tuple(<<?Tu_attribute, Bin/binary>>) ->
  [Type, Version, Atom, Bin1] = decode([binary, binary, binary], Bin),
  {{attribute,
    Type,
    Version,
    Atom}, Bin1, [type, version, atom]};
decode_tuple(<<?Tu_AdditionalISUP, Bin/binary>>) ->
  [Msg, List, Bin1] = decode([list, list], Bin),
  {{'AdditionalISUP',
    Msg,
    List}, Bin1, [msg, list]};
decode_tuple(<<?Tu_AdditionalISUPParam, Bin/binary>>) ->
  [Type, Length, Value, Bin1] = decode([integer, ineteger, binary], Bin),
  {{'AdditionalISUPParam',
    Type,
    Length,
    Value}, Bin1, [type, length, value]};
decode_tuple(<<?Tu_AdditionalSIP, Bin/binary>>) ->
  [Msg, Params, Bin1] = decode([list, list], Bin),
  {{'AdditionalSIP',
    Msg,
    Params}, Bin1, [msg, params]};
decode_tuple(<<?Tu_AdditionalSIPParam, Bin/binary>>) ->
  [Type, Value, Bin1] = decode([list, list], Bin),
  {{'AdditionalSIPParam',
    Type,
    Value}, Bin1, [type, value]};
decode_tuple(<<?Tu_forSIP, Bin/binary>>) ->
  [Elem, Bin1] = decode([tuple], Bin),
  {{forSIP,
    Elem}, Bin1, [element]};
decode_tuple(<<?Tu_format, Bin/binary>>) ->
  [Bit, Codec, Kb, <<Status, _/binary>> = Bin1] = decode([binary, binary, binary], Bin), 
  [Version, Bin2] = if
    Status == ?T_List ->
      decode([list],Bin1);
    true ->
      decode([binary],Bin1)
  end,
  [Info, Arg1, Arg2, Bin4] = decode([binary, bool, bool], Bin2),
  {{format,
    Bit,
    Codec,
    Kb,
    Version,
    Info,
    Arg1,
    Arg2}, Bin4, [bit, codec, kb, version, info, arg1, arg2]};
decode_tuple(<<?Tu_MediaId, Bin/binary>>) ->
  [ResBin, Bin1] = decode([binary],Bin),
  {{'MediaId',
    ResBin}, Bin1, [resbin]};
decode_tuple(<<?Tu_keyvalue, SizeKey, BinKey:SizeKey/binary, Bin/binary>>) ->
  Key               = erlang:binary_to_term(BinKey),
  {Value, Bin1, _} = decode_value(Bin),
  {{Key, Value}, Bin1, []};
decode_tuple(<<?B_Undefined, Bin/binary>>) ->
  {<<>>, Bin, []};
decode_tuple(<<?UNKNOWN, _/binary>> = Bin) ->
  decode_unknown(Bin).


decode_value(<<?T_String, _/binary>> = Bin) -> decode_list(Bin);
decode_value(<<?T_List, _/binary>> = Bin) -> decode_list(Bin);
decode_value(<<?R_Tuple, Bin/binary>>) -> decode_tuple(Bin);
decode_value(?DECODE_BINARY = Bin) -> decode_binary(Bin);
decode_value(?DECODE_INTEGER = Bin) -> decode_integer(Bin);
decode_value(<<?UNKNOWN, _/binary>> = Bin) -> decode_unknown(Bin).


decode_unknown(?DECODE_UNDEFINED) -> {undefined, _Bin, []};
decode_unknown(?DECODE_UNKNOWN) ->
  {Size,Bin1,_} = decode_integer(_Bin),
  BinTerm = binary:part(Bin1, {0,Size}),
  Bin2 = binary:part(Bin1, {Size, ?SIZE(Bin1) - Size}),
  Term = erlang:binary_to_term(BinTerm),
  {Term, Bin2, []}.


decode_bool(?DECODE_UNKNOWN = BinTerm) -> decode_unknown(BinTerm);
decode_bool(<<?Undefined, Bin/binary>>) -> {undefined, Bin, []};
decode_bool(<<?T_Bool_false, Bin/binary>>) -> {false, Bin, []};
decode_bool(<<?T_Bool_true, Bin/binary>>) -> {true, Bin, []}.


% проверка на строку
is_string(List) ->
  F = fun(El) ->
    if El < 0 -> false;
      El > 255 -> false;
      true -> true
    end
  end,
  case erlang:is_list(List) of
    false -> false;
    true -> lists:all(F, List)
  end.


%%%%%% Test from list with packets
test(File) ->
  {ok, List} = file:consult(File),
  test_(List).

test_([Pack | T]) ->
  Before = ?SIZE(term_to_binary(Pack)),
  Time = erlang:monotonic_time(micro_seconds),
  Binary = encode(Pack),
  <<Type, _/binary>> = Binary,
  if % TODO: убрать перед push !!!!!
    Type == 254 -> io:format("Not optimal!~n~p~n",[Pack]);
    true -> false
  end,
  log:trace([acpCoderTrace],"Encode time = ~p~n",[(erlang:monotonic_time(micro_seconds) - Time)]),
  After = ?SIZE(Binary),
  Time1 = erlang:monotonic_time(micro_seconds),
  ResPack = decode(Binary),
  if Pack /= ResPack ->
      log:trace([acpCoderTrace],"Error Packet~n~p~nResPack~p~n~n",[Pack,ResPack]);
    true ->
      log:trace([acpCoderTrace],"Decode time = ~p~n",[(erlang:monotonic_time(micro_seconds) - Time1)]),
      log:trace([acpCoderTrace],"Before size = ~p~nAfter size = ~p~nCompress = ~.2f%~n~n",[Before,After,(1-After/Before)*100])
  end,
  test_(T);
test_([]) -> ok.


%%%%%% Logs
writeToLogs(TimeStart, Packet, Result, Status) ->
  log:debug([acpCoderTrace],"~n~p~n",[Packet]),
  Calc = fun(Start, Pack, Res, Stat) ->
    End = erlang:monotonic_time(micro_seconds),
    Time = End - Start,
    {SizeBefore, SizeAfter} = case Stat of
      encode ->
        {?SIZE(term_to_binary(Pack)), ?SIZE(Res)};
      _ ->
        {?SIZE(term_to_binary(Res)), ?SIZE(Pack)}
    end,
    [Time, SizeBefore, SizeAfter, (1 - SizeAfter / SizeBefore) * 100]
  end,
  log:debug([acpCoder],"~nTime: | Size before,after: | Compress:~n~p~n",[Calc(TimeStart,Packet,Result, Status)]).


writeToError(Packet) ->
  log:warning([acpCoderError],"~nError packet:~p~n~n",[Packet]).

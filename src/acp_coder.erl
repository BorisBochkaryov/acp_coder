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

-export([test/0, encode/1, decode/1]).
-include_lib("chronica/include/chronica.hrl").
-include("../include/INCS3datatypes.hrl").
-include("../include/INCS3Internals.hrl").
-include("../include/ISUP.hrl").
-include("../include/ACP.hrl").


%%%%%% Defines
-define(Z_Undefined, <<0>>).
-define(Undefined, 0).

% Simple types
-define(T_Binary, 1).
-define(T_Integer, 2).
-define(T_Tuple, 3).
-define(T_List, 4).
-define(T_Bool, 5).
-define(T_String, 6).
-define(T_IrregularList, 7).

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
-define(E_RedirectingReasonInfo, 25).
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
-define(B_Undefined, 63).


%%%%%% Macros
-define(SIZE(Bin), erlang:size(Bin)).
-define(GET_EL(N,Tuple), erlang:element(N,Tuple)).

-define(ENCODE_BINARY(Bin), <<?T_Binary:3, (?SIZE(Bin)):5, Bin/binary>>).
-define(ENCODE_INTEGER(Int),
  begin
    BinInt = erlang:term_to_binary(Int, [{compressed,4}]),
    <<?T_Integer:3, (?SIZE(BinInt)):5, BinInt/binary>>
  end).
-define(ENCODE_STRING(Str),
  begin
    BinStr = erlang:list_to_binary(Str),
    <<?T_String, (?SIZE(BinStr)), BinStr/binary>>
  end).
-define(ENCODE_BOOL(Atom), case Atom of false -> <<?T_Bool:3, 0:5>>; _ -> <<?T_Bool:3, 1:5>> end).
-define(ENCODE_UNKNOWN(Term),
  begin
    BinTerm = erlang:term_to_binary(Term,[{compressed,4}]),
    <<?UNKNOWN,
      (?ENCODE_INTEGER(?SIZE(BinTerm)))/binary,
      BinTerm/binary>>
  end).
-define(ENCODE_TIME(MegaSec,Sec,MicSec),
  begin
    BinMeSec = erlang:integer_to_binary(MegaSec),
    BinSec = erlang:integer_to_binary(Sec),
    BinMicSec = erlang:integer_to_binary(MicSec),
    <<?R_EventTime, (?SIZE(BinMeSec)), BinMeSec/binary, (?SIZE(BinSec)), BinSec/binary,
      (?SIZE(BinMicSec)), BinMicSec/binary>>
  end).


-define(DECODE_BINARY, <<?T_Binary:3, Size:5, _Bin:Size/binary, _Rest/binary>>).
-define(DECODE_INTEGER, <<?T_Integer:3, _Size:5, _Bin/binary>>).
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
  Result = encode_record(Pack),
  writeToLogs(TimeStart, Pack, Result),
  log:debug("encode result: ~p", [Result]),
  Result.

encode_record(#'AcpMessage'{uri = Uri, callRef = CallRef, body = Body}) ->
  ResUri     = encode_binary(Uri),
  ResCallRef = encode_integer(CallRef),
  ResBody    = encode_record(Body),
  % result
  <<?R_AcpMessage,
    ResUri/binary,
    ResCallRef/binary,
    ResBody/binary>>;
encode_record(#'ServiceFeatureInd'{arg = Arg}) ->
  ResArg = encode_record(Arg),
  <<?R_ServiceFeatureInd,
    ResArg/binary>>;
encode_record(#'ServiceFeatureType'{cause = Cause, additionalInfo = AddInfo}) ->
  ResCause   = encode_enum(?E_CauseLittle,Cause),
  ResAddInfo = encode_list(AddInfo),
  <<?R_ServiceFeatureType,
    ResCause/binary,
    ResAddInfo/binary>>;
encode_record(#'SetupReqAck'{arg = Arg}) ->
  ResArg = encode_record(Arg),
  <<?R_SetupReqAck,
    ResArg/binary>>;
encode_record(#'SetupIndAck'{arg = Arg}) ->
  ResArg = encode_record(Arg),
  <<?R_SetupIndAck,
    ResArg/binary>>;
encode_record(#'SetupAckType'{refer = Refer, trunkGroupId = TrunkGId}) ->
  ResRefer    = encode_record(Refer),
  ResTrunkGId = encode_record(TrunkGId),
  % Arg
  <<?R_SetupAckType,
    ResRefer/binary,
    ResTrunkGId/binary>>;
encode_record(#'CallProgressReq'{arg = Arg}) ->
  ResArg = encode_record(Arg),
  <<?R_CallProgressReq,
    ResArg/binary>>;
encode_record(#'CallProgressInd'{arg = Arg}) ->
  ResArg = encode_record(Arg),
  <<?R_CallProgressInd,
    ResArg/binary>>;
encode_record(#'CallProgressType'{} = Term) ->
  Cause              = encode_enum(?E_Cause,Term#'CallProgressType'.cause),
  CauseInitiator     = encode_enum(?E_CauseInitiator,Term#'CallProgressType'.causeInitiator),
  CauseDescription   = encode_list(Term#'CallProgressType'.causeDescription),
  CauseIsup          = encode_binary(Term#'CallProgressType'.causeIsup),
  EventInformation   = encode_record(Term#'CallProgressType'.eventInformation),
  AdditionalInfo     = encode_list(Term#'CallProgressType'.additionalInfo),
  OBCI               = encode_record(Term#'CallProgressType'.oBCI),
  GNotification      = encode_list(Term#'CallProgressType'.gNotification),
  RedirectionNumber  = encode_record(Term#'CallProgressType'.redirectionNumber),
  RedirectionResInd  = encode_enum(?E_RedirectionRestrictionIndicator,Term#'CallProgressType'.redirectionRestInd),
  CallDiversionInfo  = encode_record(Term#'CallProgressType'.callDiversionInfo),
  Facility           = encode_list(Term#'CallProgressType'.facility),
  SDP                = encode_record(Term#'CallProgressType'.sdp),
  MediaPoint         = encode_binary(Term#'CallProgressType'.mediaPoint),
  TrunkGroupId       = encode_record(Term#'CallProgressType'.trunkGroupId),
  CallTransferNumber = encode_record(Term#'CallProgressType'.callTransferNumber),
  Refer              = encode_record(Term#'CallProgressType'.refer),
  EventTime          = encode_time(Term#'CallProgressType'.eventTime),
  <<?R_CallProgressType,
    Cause/binary,
    CauseInitiator/binary,
    CauseDescription/binary,
    CauseIsup/binary,
    EventInformation/binary,
    AdditionalInfo/binary,
    OBCI/binary,
    GNotification/binary,
    RedirectionNumber/binary,
    RedirectionResInd/binary,
    CallDiversionInfo/binary,
    Facility/binary,
    SDP/binary,
    MediaPoint/binary,
    TrunkGroupId/binary,
    CallTransferNumber/binary,
    Refer/binary,
    EventTime/binary>>;
encode_record(#'EventInformation'{} = Term) ->
  EvInd   = encode_enum(?E_EventIndicator,Term#'EventInformation'.eventIndicator),
  EvPrInd = encode_enum(?E_EventPresentationRestrictedIndicator,Term#'EventInformation'.eventPresentationIndicator),
  <<?R_EventInformation,
    EvInd/binary,
    EvPrInd/binary>>;
encode_record(#'OptionalBackwardCallIndicators'{} = Term) ->
  InbII                 = encode_enum(?E_InBandInfoIndicator,Term#'OptionalBackwardCallIndicators'.inbInfoInd),
  CallDiversionInd      = encode_enum(?E_CallDiversionIndicator,Term#'OptionalBackwardCallIndicators'.callDiversionInd),
  SimpleSegmentationInf = encode_enum(?E_SimpleSegmentationIndicator,Term#'OptionalBackwardCallIndicators'.simpleSegmentationInf),
  MlppUserInd           = encode_enum(?E_MLPPUserIndicator,Term#'OptionalBackwardCallIndicators'.mlppUserInd),
  <<?R_OptionalBackwardCallIndicators,
    InbII/binary,
    CallDiversionInd/binary,
    SimpleSegmentationInf/binary,
    MlppUserInd/binary>>;
encode_record(#'RedirectingNumber'{} = Term) ->
  Nai        = encode_enum(?E_NAIType, Term#'RedirectingNumber'.nai),
  Ni         = encode_enum(?E_NIType, Term#'RedirectingNumber'.ni),
  Incomplete = ?ENCODE_BOOL(Term#'RedirectingNumber'.incomplete),
  Npi        = encode_enum(?E_NPIType,Term#'RedirectingNumber'.npi),
  Apri       = encode_enum(?E_APRIType,Term#'RedirectingNumber'.apri),
  Digits     = encode_list(Term#'RedirectingNumber'.digits),
  <<?R_RedirectingNumber,
    Nai/binary,
    Ni/binary,
    Incomplete/binary,
    Npi/binary, Apri/binary,
    Digits/binary>>;
encode_record(#'CallDiversionInformation'{} = Term) ->
  NotifSubsOpts     = encode_enum(?E_NotificationSubscriptionOptions, Term#'CallDiversionInformation'.notificationSubsOpts),
  RedirectingReason = encode_enum(?E_RedirectingReason, Term#'CallDiversionInformation'.redirectingReason),
  <<?R_CallDiversionInformation,
    NotifSubsOpts/binary,
    RedirectingReason/binary>>;
encode_record(#'SDPType'{} = Term) ->
  Type = encode_enum(?E_TypeSDPType, Term#'SDPType'.type),
  Body = encode_list(Term#'SDPType'.body),
  <<?R_SDPType,
    Type/binary,
    Body/binary>>;
encode_record(#'TrunkGroupId'{} = Term) ->
  TrunkGroupId  = encode_list(Term#'TrunkGroupId'.trunkGroupId),
  TrunkId       = encode_list(Term#'TrunkGroupId'.trunkId),
  PCMId         = encode_list(Term#'TrunkGroupId'.pCMId),
  ChannelNumber = encode_integer(Term#'TrunkGroupId'.channelNumber),
  <<?R_TrunkGroupId,
    TrunkGroupId/binary,
    TrunkId/binary,
    PCMId/binary,
    ChannelNumber/binary>>;
encode_record(#'CallTransferNumber'{} = Term) ->
  Nai        = encode_enum(?E_NAIType, Term#'CallTransferNumber'.nai),
  Ni         = encode_enum(?E_NIType, Term#'CallTransferNumber'.ni),
  Incomplete = ?ENCODE_BOOL(Term#'CallTransferNumber'.incomplete),
  Npi        = encode_enum(?E_NPIType,Term#'CallTransferNumber'.npi),
  Apri       = encode_enum(?E_APRIType,Term#'CallTransferNumber'.apri),
  Screening  = encode_enum(?E_ScreeningType,Term#'CallTransferNumber'.screening),
  Digits     = encode_list(Term#'CallTransferNumber'.digits),
  <<?R_CallTransferNumber,
    Nai/binary,
    Ni/binary,
    Incomplete/binary,
    Npi/binary,
    Apri/binary,
    Screening/binary,
    Digits/binary>>;
encode_record({refer, List}) ->
  Chunt = encode_list(List),
  <<?R_Refer,
    Chunt/binary>>;
encode_record(#'ReferType'{} = Term) ->
  Exchange   = encode_list(Term#'ReferType'.exchange),
  RoutingKey = encode_list(Term#'ReferType'.routingKey),
  Sid        = encode_binary(Term#'ReferType'.sid),
  CallRef    = encode_integer(Term#'ReferType'.callRef),
  ConfId     = encode_list(Term#'ReferType'.conf_id),
  <<?R_ReferType,
    Exchange/binary,
    RoutingKey/binary,
    Sid/binary,
    CallRef/binary,
    ConfId/binary>>;
encode_record(#'RedirectionNumber'{} = Term) ->
  Nai         = encode_enum(?E_NAIType, Term#'RedirectionNumber'.nai),
  Ni          = encode_enum(?E_NIType, Term#'RedirectionNumber'.ni),
  Incomplete  = ?ENCODE_BOOL(Term#'RedirectionNumber'.incomplete),
  Inni        = encode_enum(?E_INNIType, Term#'RedirectionNumber'.inni),
  Npi         = encode_enum(?E_NPIType,Term#'RedirectionNumber'.npi),
  Digits      = encode_list(Term#'RedirectionNumber'.digits),
  DisplayName = encode_list(Term#'RedirectionNumber'.displayName),
  SipUri      = encode_list(Term#'RedirectionNumber'.sipUri),
  <<?R_RedirectionNumber,
    Nai/binary,
    Ni/binary,
    Incomplete/binary,
    Npi/binary,
    Inni/binary,
    Npi/binary,
    Digits/binary,
    DisplayName/binary,
    SipUri/binary>>;
encode_record(#'ReleaseReq'{} = Term) ->
  ResArg = encode_record(Term#'ReleaseReq'.arg),
  <<?R_ReleaseReq,
    ResArg/binary>>;
encode_record(#'ReleaseType'{} = Term) ->
  Cause            = encode_enum(?E_Cause, Term#'ReleaseType'.cause),
  CauseInitiator   = encode_enum(?E_CauseInitiator, Term#'ReleaseType'.causeInitiator),
  CauseDescription = encode_list(Term#'ReleaseType'.causeDescription),
  Digits           = encode_list(Term#'ReleaseType'.dialledDigits),
  CauseIsup        = encode_binary(Term#'ReleaseType'.causeIsup),
  AdditionalInfo   = encode_list(Term#'ReleaseType'.additionalInfo),
  TrunkGroupId     = encode_record(Term#'ReleaseType'.trunkGroupId),
  Sid              = encode_binary(Term#'ReleaseType'.sid),
  Refer            = encode_record(Term#'ReleaseType'.refer),
  NeedAck          = ?ENCODE_BOOL(Term#'ReleaseType'.need_ack),
  EventTime        = encode_time(Term#'ReleaseType'.eventTime),
  <<?R_ReleaseType,
    Cause/binary,
    CauseInitiator/binary,
    CauseDescription/binary,
    Digits/binary,
    CauseIsup/binary,
    AdditionalInfo/binary,
    TrunkGroupId/binary, Sid/binary, Refer/binary, NeedAck/binary, EventTime/binary>>;
encode_record(#'ReleaseInd'{} = Term) ->
  ResArg = encode_record(Term#'ReleaseInd'.arg),
  <<?R_ReleaseInd,
    ResArg/binary>>;
encode_record(#'SetupInd'{} = Term) ->
  ResArg = encode_record(Term#'SetupInd'.arg),
  <<?R_SetupInd,
    ResArg/binary>>;
encode_record(#'SetupReq'{} = Term) ->
  ResArg = encode_record(Term#'SetupReq'.arg),
  <<?R_SetupReq,
    ResArg/binary>>;
encode_record(#'SetupIRType'{} = Term) ->
  Domain                     = encode_list(Term#'SetupIRType'.domain),
  CalledPartyNumber          = encode_record(Term#'SetupIRType'.calledPartyNumber),
  CalledPartysCategory       = encode_enum(?E_CalledPartysCategory,Term#'SetupIRType'.calledPartysCategory),
  CallingPartyNumber         = encode_record(Term#'SetupIRType'.callingPartyNumber),
  CallingPartysCategory      = encode_enum(?E_CallingPartysCategory,Term#'SetupIRType'.callingPartysCategory),
  LocationNumber             = encode_list(Term#'SetupIRType'.locationNumber),
  OriginalCalledNumber       = encode_record(Term#'SetupIRType'.originalCalledNumber),
  UserTeleserviceInformation = encode_list(Term#'SetupIRType'.userTeleserviceInformation),
  GenericNumbers             = encode_list(Term#'SetupIRType'.genericNumber),
  ForwardCallIndicators      = encode_list(Term#'SetupIRType'.forwardCallIndicators),
  RedirectingNumber          = encode_record(Term#'SetupIRType'.redirectingNumber),
  RedirectionInformation     = encode_record(Term#'SetupIRType'.redirectingInformation),
  USIServiceIndicator        = ?ENCODE_UNKNOWN(Term#'SetupIRType'.uSIServiceIndicator),
  USIInformation             = encode_list(Term#'SetupIRType'.uSIInformation),
  ISUPCallReference          = encode_list(Term#'SetupIRType'.isupCallRef),
  SDPType                    = encode_record(Term#'SetupIRType'.sdp),
  MediaPoint                 = encode_binary(Term#'SetupIRType'.mediaPoint),
  AdditionalInfo             = encode_list(Term#'SetupIRType'.additionalInfo),
  TrunkGroupId               = encode_record(Term#'SetupIRType'.trunkGroupId),
  CallingPartyInfo           = encode_list(Term#'SetupIRType'.callingPartyInfo),
  CalledPartyInfo            = encode_list(Term#'SetupIRType'.calledPartyInfo),
  CallingIfaceInfo           = encode_list(Term#'SetupIRType'.callingIfaceInfo),
  CalledIfaceInfo            = encode_list(Term#'SetupIRType'.calledIfaceInfo),
  Refer                      = encode_record(Term#'SetupIRType'.refer),
  SetupModeType              = encode_enum(?E_SetupModeType,Term#'SetupIRType'.mode),
  Priority                   = encode_record(Term#'SetupIRType'.priority),
  EventTime                  = encode_time(Term#'SetupIRType'.eventTime),
  <<?R_SetupIRType,
    Domain/binary,
    CalledPartyNumber/binary,
    CalledPartysCategory/binary,
    CallingPartyNumber/binary,
    CallingPartysCategory/binary,
    LocationNumber/binary,
    OriginalCalledNumber/binary,
    UserTeleserviceInformation/binary,
    GenericNumbers/binary,
    ForwardCallIndicators/binary,
    RedirectingNumber/binary,
    RedirectionInformation/binary,
    USIServiceIndicator/binary,
    USIInformation/binary,
    ISUPCallReference/binary,
    SDPType/binary,
    MediaPoint/binary,
    AdditionalInfo/binary,
    TrunkGroupId/binary,
    CallingPartyInfo/binary,
    CalledPartyInfo/binary,
    CallingIfaceInfo/binary,
    CalledIfaceInfo/binary,
    Refer/binary,
    SetupModeType/binary,
    Priority/binary,
    EventTime/binary>>;
encode_record(#'CalledPartyNumber'{} = Term) ->
  Nai         = encode_enum(?E_NAIType, Term#'CalledPartyNumber'.nai),
  Ni          = encode_enum(?E_NIType, Term#'CalledPartyNumber'.ni),
  Incomplete  = ?ENCODE_BOOL(Term#'CalledPartyNumber'.incomplete),
  Inni        = encode_enum(?E_INNIType, Term#'CalledPartyNumber'.inni),
  Npi         = encode_enum(?E_NPIType,Term#'CalledPartyNumber'.npi),
  Digits      = encode_list(Term#'CalledPartyNumber'.digits),
  DisplayName = encode_list(Term#'CalledPartyNumber'.displayName),
  SipUri      = encode_list(Term#'CalledPartyNumber'.sipUri),
  <<?R_CalledPartyNumber,
    Nai/binary,
    Ni/binary,
    Incomplete/binary,
    Inni/binary,
    Npi/binary,
    Digits/binary,
    DisplayName/binary,
    SipUri/binary>>;
encode_record(#'CallingPartyNumber'{} = Term) ->
  Nai         = encode_enum(?E_NAIType, Term#'CallingPartyNumber'.nai),
  Ni          = encode_enum(?E_NIType, Term#'CallingPartyNumber'.ni),
  Incomplete  = ?ENCODE_BOOL(Term#'CallingPartyNumber'.incomplete),
  Npi         = encode_enum(?E_NPIType,Term#'CallingPartyNumber'.npi),
  Apri        = encode_enum(?E_APRIType,Term#'CallingPartyNumber'.apri),
  Screening   = encode_enum(?E_ScreeningType,Term#'CallingPartyNumber'.screening),
  Digits      = encode_list(Term#'CallingPartyNumber'.digits),
  DisplayName = encode_list(Term#'CallingPartyNumber'.displayName),
  CallerId    = encode_list(Term#'CallingPartyNumber'.callerId),
  SipUri      = encode_list(Term#'CallingPartyNumber'.sipUri),
  <<?R_CallingPartyNumber,
    Nai/binary,
    Ni/binary,
    Incomplete/binary,
    Npi/binary,
    Apri/binary,
    Screening/binary,
    Digits/binary,
    DisplayName/binary,
    CallerId/binary,
    SipUri/binary>>;
encode_record(#'SetupCRType'{} = Term) ->
  ConnectedNumber    = encode_record(Term#'SetupCRType'.connectedNumber),
  AdditionalInfo     = encode_list(Term#'SetupCRType'.additionalInfo),
  RedirectionNumber  = encode_record(Term#'SetupCRType'.redirectionNumber),
  RedirectionRestInd = encode_enum(?E_RedirectionRestrictionIndicator, Term#'SetupCRType'.redirectionRestInd),
  Sdp                = encode_record(Term#'SetupCRType'.sdp),
  ObsoleteMediaPoint = encode_binary(Term#'SetupCRType'.obsoleteMediaPoint),
  Refer              = encode_record(Term#'SetupCRType'.refer),
  EventTime          = encode_time(Term#'SetupCRType'.eventTime),
  <<?R_SetupCRType,
    ConnectedNumber/binary,
    AdditionalInfo/binary,
    RedirectionNumber/binary,
    RedirectionRestInd/binary,
    Sdp/binary,
    ObsoleteMediaPoint/binary,
    Refer/binary,
    EventTime/binary>>;
encode_record(#'MLPPPrecedence'{} = Term) ->
  Lfb               = encode_integer(Term#'MLPPPrecedence'.lfb),
  PrecedenceLevel   = encode_integer(Term#'MLPPPrecedence'.precedenceLevel),
  NetworkIdentity   = encode_integer(Term#'MLPPPrecedence'.network_identity),
  MlppServiceDomain = encode_integer(Term#'MLPPPrecedence'.mlpp_service_domain),
  <<?R_MLPPPrecedence,
    Lfb/binary,
    PrecedenceLevel/binary,
    NetworkIdentity/binary,
    MlppServiceDomain/binary>>;
encode_record(#'OriginalCalledNumber'{} = Term) ->
  Nai         = encode_enum(?E_NAIType, Term#'OriginalCalledNumber'.nai),
  Ni          = encode_enum(?E_NIType, Term#'OriginalCalledNumber'.ni),
  Incomplete  = ?ENCODE_BOOL(Term#'OriginalCalledNumber'.incomplete),
  Npi         = encode_enum(?E_NPIType,Term#'OriginalCalledNumber'.npi),
  Apri        = encode_enum(?E_APRIType,Term#'OriginalCalledNumber'.apri),
  Digits      = encode_list(Term#'OriginalCalledNumber'.digits),
  DisplayName = encode_list(Term#'OriginalCalledNumber'.displayName),
  SipUri      = encode_list(Term#'OriginalCalledNumber'.sipUri),
  <<?R_OriginalCalledNumber,
    Nai/binary,
    Ni/binary,
    Incomplete/binary,
    Npi/binary,
    Apri/binary,
    Digits/binary,
    DisplayName/binary,
    SipUri/binary>>;
encode_record(#'SetupResp'{} = Term) ->
  ResArg = encode_record(Term#'SetupResp'.arg),
  <<?R_SetupResp,
    ResArg/binary>>;
encode_record(#'SetupConf'{} = Term) ->
  ResArg = encode_record(Term#'SetupConf'.arg),
  <<?R_SetupConf,
    ResArg/binary>>;
encode_record(#'RedirectionInformation'{} = Term) ->
  OriginalRedirectionReason = encode_enum(?E_OriginalRedirectionReason,Term#'RedirectionInformation'.originalRedirectionReason),
  RedirectingIndicator = encode_enum(?E_OriginalRedirectionReason,Term#'RedirectionInformation'.originalRedirectionReason),
  RedirectingReason = encode_enum(?E_RedirectingReasonInfo,Term#'RedirectionInformation'.redirectingReason),
  RedirectionCounter = encode_enum(?E_RedirectionCounter,Term#'RedirectionInformation'.redirectionCounter),
  <<?R_RedirectionInformation,
    OriginalRedirectionReason/binary,
    RedirectingIndicator/binary,
    RedirectingReason/binary,
    RedirectionCounter/binary>>;
encode_record(undefined) -> ?Z_Undefined;
encode_record(Pack) -> writeToError(["Pack",Pack]), ?ENCODE_UNKNOWN(Pack).


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
encode_enum(?E_NotificationSubscriptionOptions, unknown) -> <<?E_NotificationSubscriptionOptions, 1>>;
encode_enum(?E_NotificationSubscriptionOptions, presentation_not_allowed) -> <<?E_NotificationSubscriptionOptions, 2>>;
encode_enum(?E_NotificationSubscriptionOptions, presentation_allowed_with_redirecting_number) -> <<?E_NotificationSubscriptionOptions, 3>>;
encode_enum(?E_NotificationSubscriptionOptions, presentation_allowed_without_redirecting_number) -> <<?E_NotificationSubscriptionOptions, 4>>;
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
encode_enum(?E_OriginalRedirectionReason, 'OriginalRedirectionReason') -> <<?E_OriginalRedirectionReason, 0>>;
encode_enum(?E_RedirectingIndicator, 'RedirectingIndicator') -> <<?E_RedirectingIndicator, 0>>;
encode_enum(?E_RedirectingIndicator, diversion) -> <<?E_RedirectingIndicator, 1>>;
encode_enum(?E_RedirectingReasonInfo, 'RedirectingReason') -> <<?E_RedirectingReasonInfo, 0>>;
encode_enum(?E_RedirectionCounter, Atom) -> <<?E_RedirectionCounter, Atom/binary>>;
encode_enum(?E_ServingSide, calling) -> <<?E_ServingSide, 0>>;
encode_enum(?E_ServingSide, called) -> <<?E_ServingSide, 1>>;
encode_enum(_,undefined) -> ?Z_Undefined;
encode_enum(Enum,Atom) -> writeToError(["Enum",Enum,Atom]), ?ENCODE_UNKNOWN(Atom).


encode_integer(undefined) -> ?Z_Undefined;
encode_integer(Int) when erlang:is_number(Int) -> ?ENCODE_INTEGER(Int).


encode_time({MegaSec, Sec, MicSec}) -> ?ENCODE_TIME(MegaSec, Sec, MicSec).


encode_binary(undefined) -> ?Z_Undefined;
encode_binary(Bin) when erlang:is_binary(Bin)-> ?ENCODE_BINARY(Bin);
encode_binary(Atom) when erlang:is_atom(Atom) -> ?ENCODE_UNKNOWN(Atom);
encode_binary(Term) -> writeToError(["Binary",Term]), ?ENCODE_UNKNOWN(Term).


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
  writeToError(["List",H]),
  Bin = ?ENCODE_UNKNOWN(H),
  encode_list_(T, <<Res/binary, Bin/binary>>, unknown);
encode_list_([], Res, _) -> Res.


encode_tuple({sipInfo, List} = Tuple) when ?SIZE(Tuple) == 2 ->
  BinList = encode_list(List),
  <<?Tu_sipInfo, BinList/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == sdp , ?SIZE(Tuple) == 16 ->
  Version    = encode_binary(?GET_EL(2,Tuple)),
  Origin     = encode_tuple(?GET_EL(3,Tuple)),
  Title      = encode_binary(?GET_EL(4,Tuple)),
  Arg1       = encode_binary(?GET_EL(5,Tuple)),
  Arg2       = encode_binary(?GET_EL(6,Tuple)),
  Arg3       = encode_binary(?GET_EL(7,Tuple)),
  Arg4       = encode_binary(?GET_EL(8,Tuple)),
  Connection = encode_tuple(?GET_EL(9,Tuple)),
  List1      = encode_list(?GET_EL(10,Tuple)),
  Arg5       = encode_binary(?GET_EL(11,Tuple)),
  List2      = encode_list(?GET_EL(12,Tuple)),
  Arg6       = encode_binary(?GET_EL(13,Tuple)),
  Arg7       = encode_binary(?GET_EL(14,Tuple)),
  List3      = encode_list(?GET_EL(15,Tuple)),
  List4      = encode_list(?GET_EL(16,Tuple)),
  <<?Tu_sdp,
    Version/binary,
    Origin/binary,
    Title/binary,
    Arg1/binary,
    Arg2/binary,
    Arg3/binary,
    Arg4/binary,
    Connection/binary,
    List1/binary,
    Arg5/binary,
    List2/binary,
    Arg6/binary,
    Arg7/binary,
    List3/binary,
    List4/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == origin , ?SIZE(Tuple) == 7 ->
  Symbol = encode_binary(?GET_EL(2,Tuple)),
  Num1   = encode_binary(?GET_EL(3,Tuple)),
  Num2   = encode_binary(?GET_EL(4,Tuple)),
  Type   = encode_binary(?GET_EL(5,Tuple)),
  Prot   = encode_binary(?GET_EL(6,Tuple)),
  Ip     = encode_binary(?GET_EL(7,Tuple)),
  <<?Tu_origin,
    Symbol/binary,
    Num1/binary,
    Num2/binary,
    Type/binary,
    Prot/binary,
    Ip/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == connection , ?SIZE(Tuple) == 4->
  Type = encode_binary(?GET_EL(2,Tuple)),
  Prot = encode_binary(?GET_EL(3,Tuple)),
  Ip   = encode_binary(?GET_EL(4,Tuple)),
  <<?Tu_connection,
    Type/binary,
    Prot/binary,
    Ip/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == media_description , ?SIZE(Tuple) == 7 ->
  Media = encode_tuple(?GET_EL(2,Tuple)),
  Arg1  = encode_binary(?GET_EL(3,Tuple)),
  Arg2  = encode_tuple(?GET_EL(4,Tuple)),
  List1 = encode_list(?GET_EL(5,Tuple)),
  Arg3  = encode_binary(?GET_EL(6,Tuple)),
  List2 = encode_list(?GET_EL(7,Tuple)),
  <<?Tu_media_description,
    Media/binary,
    Arg1/binary,
    Arg2/binary,
    List1/binary,
    Arg3/binary,
    List2/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == media , ?SIZE(Tuple) == 6 ->
  Type    = encode_binary(?GET_EL(2,Tuple)),
  Size    = encode_binary(?GET_EL(3,Tuple)),
  Arg     = encode_binary(?GET_EL(4,Tuple)),
  Prot    = encode_binary(?GET_EL(5,Tuple)),
  Formats = encode_list(?GET_EL(6,Tuple)),
  <<?Tu_media,
    Type/binary,
    Size/binary,
    Arg/binary,
    Prot/binary,
    Formats/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == attribute , ?SIZE(Tuple) == 4 ->
  Type    = encode_binary(?GET_EL(2,Tuple)),
  Version = encode_binary(?GET_EL(3,Tuple)),
  Atom    = ?ENCODE_UNKNOWN(?GET_EL(4,Tuple)),
  <<?Tu_attribute,
    Type/binary,
    Version/binary,
    Atom/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == ss_entity ->
  Atom1  = encode_binary(?GET_EL(2,Tuple)),
  Atom2  = encode_binary(?GET_EL(3,Tuple)),
  Number = encode_integer(?GET_EL(4,Tuple)),
  Bool1  = ?ENCODE_BOOL(?GET_EL(5,Tuple)),
  Bool2  = ?ENCODE_BOOL(?GET_EL(6,Tuple)),
  Arg1   = encode_binary(?GET_EL(7,Tuple)),
  List1  = encode_list(?GET_EL(8,Tuple)),
  List2  = encode_list(?GET_EL(9,Tuple)),
  List3  = encode_list(?GET_EL(10,Tuple)),
  <<?Tu_SSEntity,
    Atom1/binary,
    Atom2/binary,
    Number/binary,
    Bool1/binary,
    Bool2/binary,
    Arg1/binary,
    List1/binary,
    List2/binary,
    List3/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == format , ?SIZE(Tuple) == 8 ->
  Bit     = encode_binary(?GET_EL(2,Tuple)),
  Codec   = encode_binary(?GET_EL(3,Tuple)),
  Kb      = encode_binary(?GET_EL(4,Tuple)),
  if
    erlang:is_list(?GET_EL(5,Tuple)) == true ->
      Version = encode_list(?GET_EL(5,Tuple));
    true ->
      Version = encode_binary(?GET_EL(5,Tuple))
  end,
  Info    = encode_binary(?GET_EL(6,Tuple)),
  Arg1    = ?ENCODE_BOOL(?GET_EL(7,Tuple)),
  Arg2    = ?ENCODE_BOOL(?GET_EL(8,Tuple)),
  <<?Tu_format,
    Bit/binary,
    Codec/binary,
    Kb/binary,
    Version/binary,
    Info/binary,
    Arg1/binary,
    Arg2/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'MediaId' , ?SIZE(Tuple) == 2 ->
  Bin = encode_binary(?GET_EL(2,Tuple)),
  <<?Tu_MediaId,
    Bin/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'AdditionalISUP' , ?SIZE(Tuple) == 3 ->
  Msg  = ?ENCODE_STRING(?GET_EL(2,Tuple)),
  List = encode_list(?GET_EL(3,Tuple)),
  <<?Tu_AdditionalISUP,
    Msg/binary,
    List/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'AdditionalSIP' , ?SIZE(Tuple) == 3 ->
  Msg    = encode_list(?GET_EL(2,Tuple)),
  Params = encode_list(?GET_EL(3,Tuple)),
  <<?Tu_AdditionalSIP,
    Msg/binary,
    Params/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'AdditionalSIPParam' , ?SIZE(Tuple) == 3 ->
  Type  = encode_list(?GET_EL(2,Tuple)),
  Value = encode_list(?GET_EL(3,Tuple)),
  <<?Tu_AdditionalSIPParam,
    Type/binary,
    Value/binary>>;
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'AdditionalISUPParam' , ?SIZE(Tuple) == 4 ->
  Type   = encode_integer(?GET_EL(2,Tuple)),
  Length = encode_integer(?GET_EL(3,Tuple)),
  Value  = encode_binary(?GET_EL(4,Tuple)),
  <<?Tu_AdditionalISUPParam,
    Type/binary,
    Length/binary,
    Value/binary>>;  
encode_tuple(Tuple) when ?GET_EL(1,Tuple) == 'SSNotification' , ?SIZE(Tuple) == 9 ->
  SSFamily = ?ENCODE_UNKNOWN(?GET_EL(2,Tuple)),
  CGPN = encode_record(?GET_EL(3,Tuple)),
  CDPN = encode_record(?GET_EL(4,Tuple)),
  ServingSide = encode_enum(?E_ServingSide,?GET_EL(5,Tuple)),
  ServiceTimeStamp = encode_time(?GET_EL(6,Tuple)),
  Participants = encode_list(?GET_EL(7,Tuple)),
  Internal = ?ENCODE_BOOL(?GET_EL(8,Tuple)),
  Args = encode_tuple(?GET_EL(9,Tuple)),
  <<?Tu_SSNotification,
    SSFamily/binary,
    CGPN/binary,
    CDPN/binary,
    ServingSide/binary,
    ServiceTimeStamp/binary,
    Participants/binary,
    Internal/binary,
    Args/binary>>;
encode_tuple({Key, Value} = Tuple) when ?SIZE(Tuple) == 2 , ?SIZE(Tuple) == 2 ->
  BinKey     = erlang:term_to_binary(Key),
  SizeKey    = ?SIZE(BinKey),
  BinValue   = encode_value(Value),
  <<?Tu_keyvalue, SizeKey, BinKey/binary, BinValue/binary>>;
encode_tuple(<<>>) ->
  <<?B_Undefined>>;
encode_tuple(Term) ->
%  writeToError(["Tuple",Term]),
  ?ENCODE_UNKNOWN(Term).

encode_value(Value) when erlang:is_list(Value) ->
  StrStatus = is_string(Value),
  if
    StrStatus == true -> ?ENCODE_STRING(Value);
    true -> encode_list(Value)
  end;
encode_value(Value) when erlang:is_tuple(Value) ->
  <<?R_Tuple,(encode_tuple(Value))/binary>>;
encode_value(Value) when erlang:is_binary(Value) ->
  encode_binary(Value);
encode_value(Value) when erlang:is_number(Value) ->
  ?ENCODE_INTEGER(Value);
encode_value(Value) when erlang:is_atom(Value) ->
  ?ENCODE_UNKNOWN(Value).


%%%%%% Decode
decode(Pack) ->
  log:debug("decode args: ~p", [Pack]),
  TimeStart = erlang:monotonic_time(micro_seconds),
  case decode_record(Pack) of
    {Result, <<>>} ->
      writeToLogs(TimeStart, Pack, Result),
      Result;
    _ ->
      erlang:throw({decode_error, Pack})
  end.


decode_record(?DECODE_UNDEFINED) -> {undefined, _Bin};
decode_record(<<?UNKNOWN, _/binary>> = Term) -> decode_unknown(Term);
decode_record(<<?R_AcpMessage, Bin0/binary>>) ->
  {Uri, Bin1}     = decode_binary(Bin0),
  {CallRef, Bin2} = decode_integer(Bin1),
  {Body, Bin3}    = decode_record(Bin2),
  {#'AcpMessage'{
    uri     = Uri,
    callRef = CallRef,
    body    = Body
  }, Bin3};
decode_record(<<?R_ServiceFeatureInd, Bin/binary>>) ->
  {Arg, Bin1} = decode_record(Bin),
  {#'ServiceFeatureInd'{
    arg = Arg
  }, Bin1};
decode_record(<<?R_ServiceFeatureType, Bin/binary>>) ->
  {Cause, Bin0}   = decode_enum(Bin),
  {AddInfo, Bin1} = decode_list(Bin0),
  {#'ServiceFeatureType'{
    cause          = Cause,
    additionalInfo = AddInfo
  }, Bin1};
decode_record(<<?R_SetupReqAck, Bin/binary>>) ->
  {Arg, Bin1} = decode_record(Bin),
  {#'SetupReqAck'{
    arg = Arg
  }, Bin1};
decode_record(<<?R_SetupIndAck, Bin/binary>>) ->
  {Arg, Bin1} = decode_record(Bin),
  {#'SetupIndAck'{
    arg = Arg
  }, Bin1};
decode_record(<<?R_SetupReq, Bin/binary>>) ->
  {Arg, Bin1} = decode_record(Bin),
  {#'SetupReq'{
    arg = Arg
  }, Bin1};
decode_record(<<?R_SetupAckType, Bin/binary>>) ->
  {Refer, Bin1}        = decode_record(Bin),
  {TrunkGroupId, Bin2} = decode_record(Bin1),
  {#'SetupAckType'{
    refer        = Refer,
    trunkGroupId = TrunkGroupId
  }, Bin2};
decode_record(<<?R_CallProgressReq, Bin/binary>>) ->
  {Arg, Bin1} = decode_record(Bin),
  {#'CallProgressReq'{
    arg = Arg
  }, Bin1};
decode_record(<<?R_CallProgressInd, Bin/binary>>) ->
  {Arg, Bin1} = decode_record(Bin),
  {#'CallProgressInd'{
    arg = Arg
  }, Bin1};
decode_record(<<?R_CallProgressType, Bin/binary>>) ->
  {Cause, Bin1}               = decode_enum(Bin),
  {CauseInitiator, Bin2}      = decode_enum(Bin1),
  {CauseDescription, Bin3}    = decode_list(Bin2),
  {CauseIsup, Bin4}           = decode_binary(Bin3),
  {EventInformation, Bin5}    = decode_record(Bin4),
  {AdditionalInfo, Bin6}      = decode_list(Bin5),
  {OBCI, Bin7}                = decode_record(Bin6),
  {GNotification, Bin8}       = decode_list(Bin7),
  {RedirectionNumber, Bin9}   = decode_record(Bin8),
  {RedirectionResInd, Bin10}  = decode_enum(Bin9),
  {CallDiversionInfo, Bin11}  = decode_record(Bin10),
  {Facility, Bin12}           = decode_list(Bin11),
  {SDP, Bin13}                = decode_record(Bin12),
  {MediaPoint, Bin14}         = decode_binary(Bin13),
  {TrunkGroupId, Bin15}       = decode_record(Bin14),
  {CallTransferNumber, Bin16} = decode_record(Bin15),
  {Refer, Bin17}              = decode_record(Bin16),
  {EventTime, Bin18}          = decode_time(Bin17),
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
    sdp                = SDP,
    mediaPoint         = MediaPoint,
    trunkGroupId       = TrunkGroupId,
    callTransferNumber = CallTransferNumber,
    refer              = Refer,
    eventTime          = EventTime
  }, Bin18};
decode_record(<<?R_EventInformation, Bin/binary>>) ->
  {EvInd, Bin1}   = decode_enum(Bin),
  {EvPrInd, Bin2} = decode_enum(Bin1),
  {#'EventInformation'{
    eventIndicator             = EvInd,
    eventPresentationIndicator = EvPrInd
  }, Bin2};
decode_record(<<?R_OptionalBackwardCallIndicators, Bin/binary>>) ->
  {InbII, Bin1}                 = decode_enum(Bin),
  {CallDiversionInd, Bin2}      = decode_enum(Bin1),
  {SimpleSegmentationInf, Bin3} = decode_enum(Bin2),
  {MlppUserInd, Bin4}           = decode_enum(Bin3),
  {#'OptionalBackwardCallIndicators'{
    inbInfoInd            = InbII,
    callDiversionInd      = CallDiversionInd,
    simpleSegmentationInf = SimpleSegmentationInf,
    mlppUserInd           = MlppUserInd
  }, Bin4};
decode_record(<<?R_RedirectingNumber, Bin/binary>>) ->
  {Nai, Bin1}        = decode_enum(Bin),
  {Ni, Bin2}         = decode_enum(Bin1),
  {Incomplete, Bin3} = decode_bool(Bin2),
  {Npi, Bin4}        = decode_enum(Bin3),
  {Apri, Bin5}       = decode_enum(Bin4),
  {Digits, Bin6}     = decode_list(Bin5),
  {#'RedirectingNumber'{
    nai        = Nai,
    ni         = Ni,
    incomplete = Incomplete,
    npi        = Npi,
    apri       = Apri,
    digits     = Digits
  },Bin6};
decode_record(<<?R_CallDiversionInformation, Bin/binary>>) ->
  {NotifSubsOpts, Bin1}      = decode_enum(Bin),
  {RedirectingReason, Bin2}  = decode_enum(Bin1),
  {#'CallDiversionInformation'{
    notificationSubsOpts = NotifSubsOpts,
    redirectingReason    = RedirectingReason
  }, Bin2};
decode_record(<<?R_SDPType, Bin/binary>>) ->
  {Type, Bin1} = decode_enum(Bin),
  {Body, Bin2} = decode_list(Bin1),
  {#'SDPType'{
    type = Type,
    body = Body
  }, Bin2};
decode_record(<<?R_TrunkGroupId, Bin/binary>>) ->
  {TrunkGroupId, Bin1}  = decode_list(Bin),
  {TrunkId, Bin2}       = decode_list(Bin1),
  {PCMId, Bin3}         = decode_list(Bin2),
  {ChannelNumber, Bin4} = decode_integer(Bin3),
  {#'TrunkGroupId'{
    trunkGroupId  = TrunkGroupId,
    trunkId       = TrunkId,
    pCMId         = PCMId,
    channelNumber = ChannelNumber
  }, Bin4};
decode_record(<<?R_CallTransferNumber, Bin/binary>>) ->
  {Nai, Bin1}        = decode_enum(Bin),
  {Ni, Bin2}         = decode_enum(Bin1),
  {Incomplete, Bin3} = decode_bool(Bin2),
  {Npi, Bin4}        = decode_enum(Bin3),
  {Apri, Bin5}       = decode_enum(Bin4),
  {Screening, Bin6}  = decode_enum(Bin5),
  {Digits, Bin7}     = decode_list(Bin6),
  {#'CallTransferNumber'{
    nai        = Nai,
    ni         = Ni,
    incomplete = Incomplete,
    npi        = Npi,
    apri       = Apri,
    screening  = Screening,
    digits     = Digits
  }, Bin7};
decode_record(<<?R_Refer, Bin/binary>>) ->
  {List, Bin1} = decode_list(Bin),
  {{refer, List}, Bin1};
decode_record(<<?R_ReferType, Bin/binary>>) ->
  {Exchange, Bin1}   = decode_list(Bin),
  {RoutingKey, Bin2} = decode_list(Bin1),
  {Sid, Bin3}        = decode_binary(Bin2),
  {CallRef, Bin4}    = decode_integer(Bin3),
  {ConfId, Bin5}     = decode_list(Bin4),
  {#'ReferType'{
    exchange   = Exchange,
    routingKey = RoutingKey,
    sid        = Sid,
    callRef    = CallRef,
    conf_id    = ConfId
  },Bin5};
decode_record(<<?R_RedirectionNumber, Bin/binary>>) ->
  {Nai, Bin1}         = decode_enum(Bin),
  {Ni, Bin2}          = decode_enum(Bin1),
  {Incomplete, Bin3}  = decode_bool(Bin2),
  {Inni, Bin4}        = decode_enum(Bin3),
  {Npi, Bin5}         = decode_enum(Bin4),
  {Digits, Bin6}      = decode_list(Bin5),
  {DisplayName, Bin7} = decode_list(Bin6),
  {SipUri, Bin8}      = decode_list(Bin7),
  {#'RedirectionNumber'{
    nai         = Nai,
    ni          = Ni,
    incomplete  = Incomplete,
    inni        = Inni,
    npi         = Npi,
    digits      = Digits,
    displayName = DisplayName,
    sipUri      = SipUri
  }, Bin8};
decode_record(<<?R_ReleaseReq, Bin/binary>>) ->
  {Arg, Bin1} = decode_record(Bin),
  {#'ReleaseReq'{
    arg = Arg
  }, Bin1};
decode_record(<<?R_ReleaseType, Bin/binary>>) ->
  {Cause, Bin1}            = decode_enum(Bin),
  {CauseInitiator, Bin2}   = decode_enum(Bin1),
  {CauseDescription, Bin3} = decode_list(Bin2),
  {Digits, Bin4}           = decode_list(Bin3),
  {CauseIsup, Bin5}        = decode_binary(Bin4),
  {AdditionalInfo, Bin6}   = decode_list(Bin5),
  {TrunkGroupId, Bin7}     = decode_record(Bin6),
  {Sid, Bin8}              = decode_binary(Bin7),
  {Refer, Bin9}            = decode_record(Bin8),
  {NeedAck, Bin10}         = decode_bool(Bin9),
  {EventTime, Bin11}       = decode_time(Bin10),
  {#'ReleaseType'{
    cause            = Cause,
    causeInitiator   = CauseInitiator,
    causeDescription = CauseDescription,
    dialledDigits    = Digits,
    causeIsup        = CauseIsup,
    additionalInfo   = AdditionalInfo,
    trunkGroupId     = TrunkGroupId,
    sid              = Sid,
    refer            = Refer,
    need_ack         = NeedAck,
    eventTime        = EventTime
  }, Bin11};
decode_record(<<?R_ReleaseInd, Bin/binary>>) ->
  {Arg, Bin1} = decode_record(Bin),
  {#'ReleaseInd'{
    arg = Arg
  }, Bin1};
decode_record(<<?R_CalledPartyNumber, Bin/binary>>) ->
  {Nai, Bin1}         = decode_enum(Bin),
  {Ni, Bin2}          = decode_enum(Bin1),
  {Incomplete, Bin3}  = decode_bool(Bin2),
  {Inni, Bin4}        = decode_enum(Bin3),
  {Npi, Bin5}         = decode_enum(Bin4),
  {Digits, Bin6}      = decode_list(Bin5),
  {DisplayName, Bin7} = decode_list(Bin6),
  {SipUri, Bin8}      = decode_list(Bin7),
  {#'CalledPartyNumber'{
    nai         = Nai,
    ni          = Ni,
    incomplete  = Incomplete,
    inni        = Inni,
    npi         = Npi,
    digits      = Digits,
    displayName = DisplayName,
    sipUri      = SipUri
  }, Bin8};
decode_record(<<?R_CallingPartyNumber, Bin/binary>>) ->
  {Nai, Bin1}         = decode_enum(Bin),
  {Ni, Bin2}          = decode_enum(Bin1),
  {Incomplete, Bin3}  = decode_bool(Bin2),
  {Npi, Bin4}         = decode_enum(Bin3),
  {Apri, Bin5}        = decode_enum(Bin4),
  {Screening, Bin6}   = decode_enum(Bin5),
  {Digits, Bin7}      = decode_list(Bin6),
  {DisplayName, Bin8} = decode_list(Bin7),
  {CallerId, Bin9}    = decode_list(Bin8),
  {SipUri, Bin10}     = decode_list(Bin9),
  {#'CallingPartyNumber'{
    nai         = Nai,
    ni          = Ni,
    incomplete  = Incomplete,
    npi         = Npi,
    apri        = Apri,
    screening   = Screening,
    digits      = Digits,
    displayName = DisplayName,
    callerId    = CallerId,
    sipUri      = SipUri
  }, Bin10};
decode_record(<<?R_SetupResp, Bin/binary>>) ->
  {Arg, Bin1} = decode_record(Bin),
  {#'SetupResp'{
    arg = Arg
  }, Bin1};
decode_record(<<?R_SetupCRType, Bin/binary>>) ->
  {ConnectedNumber, Bin1}    = decode_record(Bin),
  {AdditionalInfo, Bin2}     = decode_list(Bin1),
  {RedirectionNumber, Bin3}  = decode_record(Bin2),
  {RedirectionRestInd, Bin4} = decode_enum(Bin3),
  {Sdp, Bin5}                = decode_record(Bin4),
  {ObsoleteMediaPoint, Bin6} = decode_binary(Bin5),
  {Refer, Bin7}              = decode_record(Bin6),
  {EventTime, Bin8}          = decode_time(Bin7),
  {#'SetupCRType'{
    connectedNumber    = ConnectedNumber,
    additionalInfo     = AdditionalInfo,
    redirectionNumber  = RedirectionNumber,
    redirectionRestInd = RedirectionRestInd,
    sdp                = Sdp,
    obsoleteMediaPoint = ObsoleteMediaPoint,
    refer              = Refer,
    eventTime          = EventTime
  }, Bin8};
decode_record(<<?R_OriginalCalledNumber, Bin/binary>>) ->
  {Nai, Bin1}         = decode_enum(Bin),
  {Ni, Bin2}          = decode_enum(Bin1),
  {Incomplete, Bin3}  = decode_bool(Bin2),
  {Npi, Bin4}         = decode_enum(Bin3),
  {Apri, Bin5}        = decode_enum(Bin4),
  {Digits, Bin6}      = decode_list(Bin5),
  {DisplayName, Bin7} = decode_list(Bin6),
  {SipUri, Bin8}      = decode_list(Bin7),
  {#'OriginalCalledNumber'{
    nai         = Nai,
    ni          = Ni,
    incomplete  = Incomplete,
    npi         = Npi,
    apri        = Apri,
    digits      = Digits,
    displayName = DisplayName,
    sipUri      = SipUri
  }, Bin8};
decode_record(<<?R_RedirectionInformation, Bin/binary>>) ->
  {OriginalRedirectionReason, Bin1} = decode_enum(Bin),
  {RedirectingIndicator, Bin2} = decode_enum(Bin1),
  {RedirectingReason, Bin3} = decode_enum(Bin2),
  {RedirectionCounter, Bin4} = decode_enum(Bin3),
  {#'RedirectionInformation'{
    originalRedirectionReason = OriginalRedirectionReason,
    redirectingIndicator      = RedirectingIndicator,
    redirectingReason         = RedirectingReason,
    redirectionCounter        = RedirectionCounter
  }, Bin4};
decode_record(<<?R_MLPPPrecedence, Bin/binary>>) ->
  {Lfb, Bin1}               = decode_integer(Bin),
  {PrecedenceLevel, Bin2}   = decode_integer(Bin1),
  {NetworkIdentity, Bin3}   = decode_integer(Bin2),
  {MlppServiceDomain, Bin4} = decode_integer(Bin3),
  {#'MLPPPrecedence'{
    lfb                 = Lfb,
    precedenceLevel     = PrecedenceLevel,
    network_identity    = NetworkIdentity,
    mlpp_service_domain = MlppServiceDomain
  }, Bin4};
decode_record(<<?R_SetupInd, Bin/binary>>) ->
  {Arg, Bin1} = decode_record(Bin),
  {#'SetupInd'{
    arg = Arg
  }, Bin1};
decode_record(<<?R_SetupConf, Bin/binary>>) ->
  {Arg, Bin1} = decode_record(Bin),
  {#'SetupConf'{
    arg = Arg
  }, Bin1};
decode_record(<<?R_SetupIRType, Bin/binary>>) ->
  {Domain, Bin1}                     = decode_list(Bin),
  {CalledPartyNumber, Bin2}          = decode_record(Bin1),
  {CalledPartysCategory, Bin3}       = decode_enum(Bin2),
  {CallingPartyNumber, Bin4}         = decode_record(Bin3),
  {CallingPartysCategory, Bin5}      = decode_enum(Bin4),
  {LocationNumber, Bin6}             = decode_list(Bin5),
  {OriginalCalledNumber, Bin7}       = decode_record(Bin6),
  {UserTeleserviceInformation, Bin8} = decode_list(Bin7),
  {GenericNumbers, Bin9}             = decode_list(Bin8),
  {ForwardCallIndicators, Bin10}     = decode_list(Bin9),
  {RedirectingNumber, Bin11}         = decode_record(Bin10),
  {RedirectionInformation, Bin12}    = decode_record(Bin11),
  {USIServiceIndicator, Bin13}       = decode_unknown(Bin12),
  {USIInformation, Bin14}            = decode_list(Bin13),
  {ISUPCallReference, Bin15}         = decode_list(Bin14),
  {SDPType, Bin16}                   = decode_record(Bin15),
  {MediaPoint, Bin17}                = decode_binary(Bin16),
  {AdditionalInfo, Bin18}            = decode_list(Bin17),
  {TrunkGroupId, Bin19}              = decode_record(Bin18),
  {CallingPartyInfo, Bin20}          = decode_list(Bin19),
  {CalledPartyInfo, Bin21}           = decode_list(Bin20),
  {CallingIfaceInfo, Bin22}          = decode_list(Bin21),
  {CalledIfaceInfo, Bin23}           = decode_list(Bin22),
  {Refer, Bin24}                     = decode_record(Bin23),
  {SetupModeType, Bin25}             = decode_enum(Bin24),
  {Priority, Bin26}                  = decode_record(Bin25),
  {EventTime, Bin27}                 = decode_time(Bin26),
  {#'SetupIRType'{
    domain                     = Domain,
    calledPartyNumber          = CalledPartyNumber,
    calledPartysCategory       = CalledPartysCategory,
    callingPartyNumber         = CallingPartyNumber,
    callingPartysCategory      = CallingPartysCategory,
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
  }, Bin27}.


decode_enum(?DECODE_UNKNOWN) -> decode_unknown(_Bin);
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
decode_enum(<<?E_OriginalRedirectionReason, 0, Bin/binary>>) -> {'OriginalRedirectionReason', Bin};
decode_enum(<<?E_RedirectingIndicator, 0, Bin/binary>>) -> {'RedirectingIndicator', Bin};
decode_enum(<<?E_RedirectingIndicator, 1, Bin/binary>>) -> {diversion, Bin};
decode_enum(<<?E_RedirectingReasonInfo, 0, Bin/binary>>) -> {'RedirectingReason', Bin};
decode_enum(<<?E_RedirectionCounter, Atom, Bin/binary>>) -> {Atom, Bin};
decode_enum(<<?E_ServingSide, 0, Bin/binary>>) -> {calling, Bin};
decode_enum(<<?E_ServingSide, 1, Bin/binary>>) -> {called, Bin}.


decode_binary(?DECODE_UNKNOWN = BinTerm) -> decode_unknown(BinTerm);
decode_binary(?DECODE_UNDEFINED) -> {undefined, _Bin};
decode_binary(?DECODE_BINARY) -> {_Bin, _Rest}.


decode_integer(?DECODE_UNKNOWN = BinTerm) -> decode_unknown(BinTerm);
decode_integer(?DECODE_INTEGER) ->
  BinInt = binary:part(_Bin, {0,_Size}),
  Int = erlang:binary_to_term(BinInt),
  {Int,binary:part(_Bin, {_Size, ?SIZE(_Bin) - _Size})}.


decode_time(?DECODE_TIME) ->
  MeSec = erlang:binary_to_integer(_BinMeSec),
  Sec = erlang:binary_to_integer(_BinSec),
  MicSec = erlang:binary_to_integer(_BinMicSec),
  {{MeSec, Sec, MicSec}, _Bin}.


decode_list(?DECODE_UNKNOWN = BinTerm) -> decode_unknown(BinTerm);
decode_list(?DECODE_UNDEFINED) -> {undefined, _Bin};
decode_list(<<?T_List, 0, Rest/binary>>) -> {[], Rest};
decode_list(?DECODE_STRING) -> % строка
  BinElem = binary:part(_Bin, {0,_Size}),
  Res = erlang:binary_to_list(BinElem),
  Bin2 = binary:part(_Bin, {_Size, ?SIZE(_Bin) - _Size}),
  {Res, Bin2};
decode_list(?DECODE_LIST) ->
  {Size, Bin1} = decode_integer(_Bin),
  BinElem = binary:part(Bin1, {0,Size}),
  Res = decode_list_(BinElem,[],[]),
  Bin2 = binary:part(Bin1, {Size, ?SIZE(Bin1) - Size}),
  {Res, Bin2}.


decode_list_(?DECODE_UNKNOWN = Temp, Res, _) ->
  {El, Rest} = decode_unknown(Temp),
  decode_list_(Rest,Res ++ [El], unknown);
decode_list_(<<?R_Tuple, Bin/binary>>, Res, _) ->
  {Tuple, Bin1} = decode_tuple(Bin),
  decode_list_(Bin1, Res ++ [Tuple], tuple);
decode_list_(?DECODE_INTEGER = Temp, Res, _) ->
  {El, Rest} = decode_integer(Temp),
  decode_list_(Rest, Res ++ [El], number);
decode_list_(?DECODE_LIST = Temp, Res, _) ->
  {El, Rest} = decode_list(Temp),
  decode_list_(Rest, Res ++ [[El]], list);
decode_list_(?DECODE_BINARY = Temp, Res, _) ->
  {El, Rest} = decode_binary(Temp),
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
      {El, Rest} = decode_list(<<?T_List,Bin/binary>>),
      decode_list_(Rest, Res ++ [[El]], list);
    _ ->
      {Size, Bin1} = decode_integer(Bin),
      BinElem = binary:part(Bin1, {0, Size}),
      Res = erlang:binary_to_term(BinElem),
      Bin2 = binary:part(Bin1, {Size, ?SIZE(Bin1) - Size}),
      decode_list_(Bin2, Res ++ [Res], Flag)
  end.


decode_tuple(<<?Tu_sipInfo, Bin/binary>>) ->
  {List, Bin1} = decode_list(Bin),
  {{sipInfo, List}, Bin1};
decode_tuple(<<?Tu_sdp, Bin/binary>>) ->
  {Version, Bin1}    = decode_binary(Bin),
  {Origin, Bin2}     = decode_tuple(Bin1),
  {Title, Bin3}      = decode_binary(Bin2),
  {Arg1, Bin4}       = decode_binary(Bin3),
  {Arg2, Bin5}       = decode_binary(Bin4),
  {Arg3, Bin6}       = decode_binary(Bin5),
  {Arg4, Bin7}       = decode_binary(Bin6),
  {Connection, Bin8} = decode_tuple(Bin7),
  {List1, Bin9}      = decode_list(Bin8),
  {Arg5, Bin10}      = decode_binary(Bin9),
  {List2, Bin11}     = decode_list(Bin10),
  {Arg6, Bin12}      = decode_binary(Bin11),
  {Arg7, Bin13}      = decode_binary(Bin12),
  {List3, Bin14}     = decode_list(Bin13),
  {List4, Bin15}     = decode_list(Bin14),
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
    List4}, Bin15};
decode_tuple(<<?Tu_SSNotification, Bin/binary>>) ->
  {SSFamily, Bin1} = decode_unknown(Bin),
  {CGPN, Bin2} = decode_record(Bin1),
  {CDPN, Bin3} = decode_record(Bin2),
  {ServingSide, Bin4} = decode_enum(Bin3),
  {ServiceTimeStamp, Bin5} = decode_time(Bin4),
  {Participants, Bin6} = decode_list(Bin5),
  {Internal, Bin7} = decode_bool(Bin6),
  {Args, Bin8} = decode_tuple(Bin7),
  {{'SSNotification',
    SSFamily,
    CGPN,
    CDPN,
    ServingSide,
    ServiceTimeStamp,
    Participants,
    Internal,
    Args}, Bin8};
decode_tuple(<<?Tu_origin, Bin/binary>>) ->
  {Symbol, Bin1} = decode_binary(Bin),
  {Num1, Bin2}   = decode_binary(Bin1),
  {Num2, Bin3}   = decode_binary(Bin2),
  {Type, Bin4}   = decode_binary(Bin3),
  {Prot, Bin5}   = decode_binary(Bin4),
  {Ip, Bin6}     = decode_binary(Bin5),
  {{origin,
    Symbol,
    Num1,
    Num2,
    Type,
    Prot,
    Ip}, Bin6};
decode_tuple(<<?Tu_connection, Bin/binary>>) ->
  {Type, Bin1} = decode_binary(Bin),
  {Prot, Bin2} = decode_binary(Bin1),
  {Ip, Bin3}   = decode_binary(Bin2),
  {{connection,
    Type,
    Prot,
    Ip}, Bin3};
decode_tuple(<<?Tu_SSEntity, Bin/binary>>) ->
  {Atom1, Bin1} = decode_binary(Bin),
  {Atom2, Bin2} = decode_binary(Bin1),
  {Number, Bin3} = decode_integer(Bin2),
  {Bool1, Bin4} = decode_bool(Bin3),
  {Bool2, Bin5} = decode_bool(Bin4),
  {Arg1, Bin6} = decode_binary(Bin5),
  {List1, Bin7} = decode_list(Bin6),
  {List2, Bin8} = decode_list(Bin7),
  {List3, Bin9} = decode_list(Bin8),
  {{ss_entity,
    Atom1,
    Atom2,
    Number,
    Bool1,
    Bool2,
    Arg1,
    List1,
    List2,
    List3}, Bin9};
decode_tuple(<<?Tu_media_description, Bin/binary>>) ->
  {Media, Bin1} = decode_tuple(Bin),
  {Arg1, Bin2}  = decode_binary(Bin1),
  {Arg2, Bin3}  = decode_tuple(Bin2),
  {List1, Bin4} = decode_list(Bin3),
  {Arg3, Bin5}  = decode_binary(Bin4),
  {List2, Bin6} = decode_list(Bin5),
  {{media_description,
    Media,
    Arg1,
    Arg2,
    List1,
    Arg3,
    List2}, Bin6};
decode_tuple(<<?Tu_media, Bin/binary>>) ->
  {Type, Bin1}    = decode_binary(Bin),
  {Size, Bin2}    = decode_binary(Bin1),
  {Arg, Bin3}     = decode_binary(Bin2),
  {Prot, Bin4}    = decode_binary(Bin3),
  {Formats, Bin5} = decode_list(Bin4),
  {{media,
    Type,
    Size,
    Arg,
    Prot,
    Formats}, Bin5};
decode_tuple(<<?Tu_attribute, Bin/binary>>) ->
  {Type, Bin1}    = decode_binary(Bin),
  {Version, Bin2} = decode_binary(Bin1),
  {Atom, Bin3}    = decode_unknown(Bin2),
  {{attribute,
    Type,
    Version,
    Atom}, Bin3};
decode_tuple(<<?Tu_AdditionalISUP, Bin/binary>>) ->
  {Msg, Bin1}  = decode_list(Bin),
  {List, Bin2} = decode_list(Bin1),
  {{'AdditionalISUP',
    Msg,
    List}, Bin2};
decode_tuple(<<?Tu_AdditionalISUPParam, Bin/binary>>) ->
  {Type, Bin1}   = decode_integer(Bin),
  {Length, Bin2} = decode_integer(Bin1),
  {Value, Bin3}  = decode_binary(Bin2),
  {{'AdditionalISUPParam',
    Type,
    Length,
    Value}, Bin3};
decode_tuple(<<?Tu_AdditionalSIP, Bin/binary>>) ->
  {Msg, Bin1} = decode_list(Bin),
  {Params, Bin2} = decode_list(Bin1),
  {{'AdditionalSIP',
    Msg,
    Params}, Bin2};
decode_tuple(<<?Tu_AdditionalSIPParam, Bin/binary>>) ->
  {Type, Bin1}  = decode_list(Bin),
  {Value, Bin2} = decode_list(Bin1),
  {{'AdditionalSIPParam',
    Type,
    Value},Bin2};
decode_tuple(<<?Tu_format, Bin/binary>>) ->
  {Bit, Bin1}                       = decode_binary(Bin),
  {Codec, Bin2}                     = decode_binary(Bin1),
  {Kb, <<Status, _/binary>> = Bin3} = decode_binary(Bin2),
  if
    Status == ?T_List ->
      {Version, Bin4} = decode_list(Bin3);
    true ->
      {Version, Bin4} = decode_binary(Bin3)
  end,
  {Info, Bin5} = decode_binary(Bin4),
  {Arg1, Bin6} = decode_bool(Bin5),
  {Arg2, Bin7} = decode_bool(Bin6),
  {{format,
    Bit,
    Codec,
    Kb,
    Version,
    Info,
    Arg1,
    Arg2}, Bin7};
decode_tuple(<<?Tu_MediaId, Bin/binary>>) ->
  {ResBin, Bin1} = decode_binary(Bin),
  {{'MediaId',
    ResBin}, Bin1};
decode_tuple(<<?Tu_keyvalue, SizeKey, BinKey:SizeKey/binary, Bin/binary>>) ->
  Key = erlang:binary_to_term(BinKey),
  {Value, Bin1} = decode_value(Bin),
  {{Key, Value}, Bin1};
decode_tuple(<<?B_Undefined, Bin/binary>>) ->
  {<<>>, Bin};
decode_tuple(<<?UNKNOWN, _/binary>> = Bin) ->
  decode_unknown(Bin).


decode_value(<<?T_String, _/binary>> = Bin) -> decode_list(Bin);
decode_value(<<?T_List, _/binary>> = Bin) -> decode_list(Bin);
decode_value(<<?R_Tuple, Bin/binary>>) -> decode_tuple(Bin);
decode_value(?DECODE_BINARY = Bin) -> decode_binary(Bin);
decode_value(?DECODE_INTEGER = Bin) -> decode_integer(Bin);
decode_value(<<?UNKNOWN, _/binary>> = Bin) -> decode_unknown(Bin).


decode_unknown(?DECODE_UNDEFINED) -> {undefined, _Bin};
decode_unknown(?DECODE_UNKNOWN) ->
  {Size,Bin1} = decode_integer(_Bin),
  BinTerm = binary:part(Bin1, {0,Size}),
  Bin2 = binary:part(Bin1, {Size, ?SIZE(Bin1) - Size}),
  Term = erlang:binary_to_term(BinTerm),
  {Term, Bin2}.


decode_bool(?DECODE_UNKNOWN = BinTerm) -> decode_unknown(BinTerm);
decode_bool(<<?T_Bool:3, 0:5, Bin/binary>>) -> {false, Bin};
decode_bool(<<?T_Bool:3, 1:5, Bin/binary>>) -> {true, Bin}.


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
test() ->
  {ok, List} = file:consult("../../../../wireshark/trace\ ACP/trace\ only\ acpmessage.log"),
%  {ok, List} = file:consult("../../../full1.log"),
  test(List).

test([Pack | T]) ->
  Before = erlang:length(lists:flatten(io_lib:format("~p",[Pack]))),
  Time = erlang:monotonic_time(micro_seconds),
  Binary = encode(Pack),
  io:format("Encode time = ~p~n",[(erlang:monotonic_time(micro_seconds) - Time)]),
  After = ?SIZE(Binary),
  Time1 = erlang:monotonic_time(micro_seconds),
  ResPack = decode(Binary),
  if Pack /= ResPack ->
      io:format("Error Packet~n~p~nResPack~p~n~n",[Pack,ResPack]);
    true ->
      io:format("Decode time = ~p~n",[(erlang:monotonic_time(micro_seconds) - Time1)]),
      io:format("Before size = ~p~nAfter size = ~p~nCompress = ~.2f%~n~n",[Before,After,(1-After/Before)*100])
  end,
  test(T);
test([]) -> ok.


%%%%%% Logs
writeToLogs(TimeStart, Packet, Result) ->
  log:trace([acpCoderTrace],"~n~p~n",[Packet]),
  Calc = fun(Start, Pack, Res) ->
    End = erlang:monotonic_time(micro_seconds),
    Time = End - Start,
    SizeBefore = erlang:length(lists:flatten(io_lib:format("~p",[Pack]))),
    SizeAfter = ?SIZE(Res),
    [Time, SizeBefore, SizeAfter, (1 - SizeAfter / SizeBefore) * 100]
  end,
  log:trace([acpCoder],"~nTime: | Size before,after: | Compress:~n~p~n",[Calc(TimeStart,Packet,Result)]).


writeToError(Packet) ->
  io:format("~p~n",[Packet]),
  log:error([acpCoderError],"~nError packet:~p~n~n",[Packet]).

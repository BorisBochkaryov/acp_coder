%% Generated by the Erlang ASN.1 BER_V2-compiler version, utilizing bit-syntax:4.0.3
%% Purpose: encoder and decoder to the types in mod INCS3BundleArg

-module('INCS3BundleArg').
-compile(nowarn_unused_vars).
-dialyzer(no_improper_lists).
-asn1_info([{vsn,'4.0.3'},
            {module,'INCS3BundleArg'},
            {options,[{i,"src"},{outdir,"src"},noobj,{i,"."},{i,"asn1"}]}]).

-export([encoding_rule/0,bit_string_format/0,
         legacy_erlang_types/0]).
-export(['dialyzer-suppressions'/1]).
-export([
'enc_Empty'/2,
'enc_ArgType'/2,
'enc_ErrorType'/2
]).

-export([
'dec_Empty'/2,
'dec_ArgType'/2,
'dec_ErrorType'/2
]).

-export([info/0]).


-export([encode/2,decode/2]).

encoding_rule() -> ber.

bit_string_format() -> bitstring.

legacy_erlang_types() -> false.

encode(Type, Data) ->
try iolist_to_binary(element(1, encode_disp(Type, Data))) of
  Bytes ->
    {ok,Bytes}
  catch
    Class:Exception when Class =:= error; Class =:= exit ->
      case Exception of
        {error,Reason}=Error ->
          Error;
        Reason ->
         {error,{asn1,Reason}}
      end
end.

decode(Type,Data) ->
try decode_disp(Type, element(1, ber_decode_nif(Data))) of
  Result ->
    {ok,Result}
  catch
    Class:Exception when Class =:= error; Class =:= exit ->
      case Exception of
        {error,Reason}=Error ->
          Error;
        Reason ->
         {error,{asn1,Reason}}
      end
end.

encode_disp('Empty',Data) -> 'enc_Empty'(Data);
encode_disp('ArgType',Data) -> 'enc_ArgType'(Data);
encode_disp('ErrorType',Data) -> 'enc_ErrorType'(Data);
encode_disp(Type,_Data) -> exit({error,{asn1,{undefined_type,Type}}}).


decode_disp('Empty',Data) -> 'dec_Empty'(Data);
decode_disp('ArgType',Data) -> 'dec_ArgType'(Data);
decode_disp('ErrorType',Data) -> 'dec_ErrorType'(Data);
decode_disp(Type,_Data) -> exit({error,{asn1,{undefined_type,Type}}}).




info() ->
   case ?MODULE:module_info(attributes) of
     Attributes when is_list(Attributes) ->
       case lists:keyfind(asn1_info, 1, Attributes) of
         {_,Info} when is_list(Info) ->
           Info;
         _ ->
           []
       end;
     _ ->
       []
   end.


%%================================
%%  Empty
%%================================
'enc_Empty'(Val) ->
    'enc_Empty'(Val, [<<5>>]).

'enc_Empty'(Val, TagIn) ->
encode_null(Val, TagIn).


'dec_Empty'(Tlv) ->
   'dec_Empty'(Tlv, [5]).

'dec_Empty'(Tlv, TagIn) ->
decode_null(Tlv, TagIn).



%%================================
%%  ArgType
%%================================
'enc_ArgType'(Val) ->
    'enc_ArgType'(Val, []).

'enc_ArgType'(Val, TagIn) ->
   {EncBytes,EncLen} = case element(1,Val) of
      aSFArg ->
         'INCS3opsargs':'enc_ActivateServiceFilteringArg'(element(2,Val), [<<160>>]);
      aSFRArg ->
         encode_null(element(2,Val), [<<129>>]);
      aTArg ->
         encode_null(element(2,Val), [<<130>>]);
      aTRArg ->
         encode_null(element(2,Val), [<<131>>]);
      aCArg ->
         'INCS3opsargs':'enc_ApplyChargingArg'(element(2,Val), [<<164>>]);
      aCRArg ->
         encode_restricted_string(element(2,Val), [<<133>>]);
      aRIArg ->
         'INCS3opsargs':'enc_AssistRequestInstructionsArg'(element(2,Val), [<<166>>]);
      cGArg ->
         'INCS3opsargs':'enc_CallGapArg'(element(2,Val), [<<167>>]);
      cIRArg ->
         'INCS3opsargs':'enc_CallInformationReportArg'(element(2,Val), [<<168>>]);
      cIRQArg ->
         'INCS3opsargs':'enc_CallInformationRequestArg'(element(2,Val), [<<169>>]);
      cANArg ->
         'INCS3opsargs':'enc_CancelArg'(element(2,Val), [<<170>>]);
      cIArg ->
         'INCS3opsargs':'enc_CollectInformationArg'(element(2,Val), [<<171>>]);
      cONArg ->
         'INCS3opsargs':'enc_ConnectArg'(element(2,Val), [<<172>>]);
      cTRArg ->
         'INCS3opsargs':'enc_ConnectToResourceArg'(element(2,Val), [<<173>>]);
      cWAArg ->
         'INCS3opsargs':'enc_ContinueWithArgumentArg'(element(2,Val), [<<174>>]);
      cSAArg ->
         'INCS3opsargs':'enc_CreateCallSegmentAssociationArg'(element(2,Val), [<<175>>]);
      cSARArg ->
         'INCS3opsargs':'enc_CreateCallSegmentAssociationResultArg'(element(2,Val), [<<176>>]);
      cUEArg ->
         encode_null(element(2,Val), [<<145>>]);
      dFCArg ->
         encode_null(element(2,Val), [<<159,99>>]);
      dFCWAArg ->
         'INCS3opsargs':'enc_DisconnectForwardConnectionWithArgumentArg'(element(2,Val), [<<178>>]);
      dLArg ->
         'INCS3opsargs':'enc_DisconnectLegArg'(element(2,Val), [<<179>>]);
      dLRArg ->
         encode_null(element(2,Val), [<<148>>]);
      eRArg ->
         'INCS3opsargs':'enc_EntityReleasedArg'(element(2,Val), [<<181>>]);
      eTCArg ->
         'INCS3opsargs':'enc_EstablishTemporaryConnectionArg'(element(2,Val), [<<182>>]);
      eNCArg ->
         'INCS3opsargs':'enc_EventNotificationChargingArg'(element(2,Val), [<<183>>]);
      eRBArg ->
         'INCS3opsargs':'enc_EventReportBCSMArg'(element(2,Val), [<<184>>]);
      fCIArg ->
         encode_restricted_string(element(2,Val), [<<153>>]);
      iDPArg ->
         'INCS3opsargs':'enc_InitialDPArg'(element(2,Val), [<<186>>]);
      iCAArg ->
         'INCS3opsargs':'enc_InitiateCallAttemptArg'(element(2,Val), [<<187>>]);
      mTDArg ->
         'INCS3opsargs':'enc_ManageTriggerDataArg'(element(2,Val), [<<188>>]);
      mTDRArg ->
         'INCS3opsargs':'enc_ManageTriggerDataResultArg'(element(2,Val), [<<189>>]);
      mCArg ->
         'INCS3opsargs':'enc_MergeCallSegmentsArg'(element(2,Val), [<<190>>]);
      mCRArg ->
         encode_null(element(2,Val), [<<159,31>>]);
      mCSArg ->
         'INCS3opsargs':'enc_MoveCallSegmentsArg'(element(2,Val), [<<191,32>>]);
      mCSRArg ->
         encode_null(element(2,Val), [<<159,33>>]);
      mLArg ->
         'INCS3opsargs':'enc_MoveLegArg'(element(2,Val), [<<191,34>>]);
      mLRArg ->
         encode_null(element(2,Val), [<<159,35>>]);
      rCArg ->
         'INCS3opsargs':'enc_ReleaseCallArg'(element(2,Val), [<<191,36>>]);
      rUArg ->
         'INCS3opsargs':'enc_ReportUTSIArg'(element(2,Val), [<<191,37>>]);
      rNCArg ->
         'INCS3opsargs':'enc_RequestNotificationChargingEventArg'(element(2,Val), [<<191,38>>]);
      rRBArg ->
         'INCS3opsargs':'enc_RequestReportBCSMEventArg'(element(2,Val), [<<191,39>>]);
      rRUArg ->
         'INCS3opsargs':'enc_RequestReportUTSIArg'(element(2,Val), [<<191,40>>]);
      rTArg ->
         'INCS3opsargs':'enc_ResetTimerArg'(element(2,Val), [<<191,41>>]);
      sCIArg ->
         'INCS3opsargs':'enc_SendChargingInformationArg'(element(2,Val), [<<191,42>>]);
      sSArg ->
         'INCS3opsargs':'enc_SendSTUIArg'(element(2,Val), [<<191,43>>]);
      sFRArg ->
         'INCS3opsargs':'enc_ServiceFilteringResponseArg'(element(2,Val), [<<191,44>>]);
      sLArg ->
         'INCS3opsargs':'enc_SplitLegArg'(element(2,Val), [<<191,45>>]);
      sLRArg ->
         encode_null(element(2,Val), [<<159,46>>]);
      pAArg ->
         'INCS3opsargs':'enc_PlayAnnouncementArg'(element(2,Val), [<<191,47>>]);
      mRArg ->
         'INCS3opsargs':'enc_MessageReceivedArg'(element(2,Val), [<<191,48>>]);
      pACArg ->
         'INCS3opsargs':'enc_PromptAndCollectUserInformationArg'(element(2,Val), [<<191,49>>]);
      pARArg ->
         'INCS3opsargs':'enc_PromptAndReceiveMessageArg'(element(2,Val), [<<191,50>>]);
      rIArg ->
         'INCS3opsargs':'enc_ReceivedInformationArg'(element(2,Val), [<<191,51>>]);
      sCArg ->
         'INCS3opsargs':'enc_ScriptCloseArg'(element(2,Val), [<<191,52>>]);
      sEArg ->
         'INCS3opsargs':'enc_ScriptEventArg'(element(2,Val), [<<191,53>>]);
      sIArg ->
         'INCS3opsargs':'enc_ScriptInformationArg'(element(2,Val), [<<191,54>>]);
      sCRArg ->
         'INCS3opsargs':'enc_ScriptRunArg'(element(2,Val), [<<191,55>>]);
      sRRArg ->
         encode_null(element(2,Val), [<<159,56>>]);
      aDIArg ->
         'INCS3opsargs':'enc_AnalysedInformationArg'(element(2,Val), [<<191,98>>]);
      aIArg ->
         'INCS3opsargs':'enc_AnalyseInformationArg'(element(2,Val), [<<191,97>>]);
      aUTTArg ->
         'INCS3opsargs':'enc_AuthorizeTerminationArg'(element(2,Val), [<<191,96>>]);
      cDI ->
         'INCS3opsargs':'enc_CollectedInformationArg'(element(2,Val), [<<191,95>>]);
      cTDArg ->
         'INCS3opsargs':'enc_CreateOrRemoveTriggerDataArg'(element(2,Val), [<<191,94>>]);
      cTDRArg ->
         'INCS3opsargs':'enc_CreateOrRemoveTriggerDataResultArg'(element(2,Val), [<<191,93>>]);
      eRFArg ->
         'INCS3opsargs':'enc_EventReportFacilityArg'(element(2,Val), [<<191,92>>]);
      fSAArg ->
         'INCS3opsargs':'enc_FacilitySelectedAndAvailableArg'(element(2,Val), [<<191,91>>]);
      oABNArg ->
         'INCS3opsargs':'enc_OAbandonArg'(element(2,Val), [<<191,90>>]);
      oANSArg ->
         'INCS3opsargs':'enc_OAnswerArg'(element(2,Val), [<<191,89>>]);
      oCPBArg ->
         'INCS3opsargs':'enc_OCalledPartyBusyArg'(element(2,Val), [<<191,88>>]);
      oDISArg ->
         'INCS3opsargs':'enc_ODisconnectArg'(element(2,Val), [<<191,87>>]);
      mIDCLArg ->
         'INCS3opsargs':'enc_MidCallArg'(element(2,Val), [<<191,86>>]);
      oNOANSArg ->
         'INCS3opsargs':'enc_ONoAnswerArg'(element(2,Val), [<<191,85>>]);
      oAArg ->
         'INCS3opsargs':'enc_OriginationAttemptArg'(element(2,Val), [<<191,84>>]);
      oAAArg ->
         'INCS3opsargs':'enc_OriginationAttemptAuthorizedArg'(element(2,Val), [<<191,83>>]);
      oSUSArg ->
         'INCS3opsargs':'enc_OSuspendedArg'(element(2,Val), [<<191,82>>]);
      rECONArg ->
         'INCS3opsargs':'enc_ReconnectArg'(element(2,Val), [<<191,81>>]);
      rCSRArg ->
         'INCS3opsargs':'enc_RequestCurrentStatusReportArg'(element(2,Val), [<<191,80>>]);
      rESCRArg ->
         'INCS3opsargs':'enc_RequestEveryStatusChangeReportArg'(element(2,Val), [<<191,79>>]);
      rFSMRArg ->
         'INCS3opsargs':'enc_RequestFirstStatusMatchReportArg'(element(2,Val), [<<191,78>>]);
      rRFEArg ->
         'INCS3opsargs':'enc_RequestReportFacilityEventArg'(element(2,Val), [<<191,77>>]);
      rSFArg ->
         'INCS3opsargs':'enc_RouteSelectFailureArg'(element(2,Val), [<<191,76>>]);
      sFArg ->
         'INCS3opsargs':'enc_SelectFacilityArg'(element(2,Val), [<<191,75>>]);
      sRArg ->
         'INCS3opsargs':'enc_SelectRouteArg'(element(2,Val), [<<191,74>>]);
      sENDFArg ->
         'INCS3opsargs':'enc_SendFacilityInformationArg'(element(2,Val), [<<191,73>>]);
      sSPArg ->
         'INCS3opsargs':'enc_SetServiceProfileArg'(element(2,Val), [<<191,72>>]);
      sREPArg ->
         'INCS3opsargs':'enc_StatusReportArg'(element(2,Val), [<<191,71>>]);
      tANSArg ->
         'INCS3opsargs':'enc_TAnswerArg'(element(2,Val), [<<191,70>>]);
      tBUSArg ->
         'INCS3opsargs':'enc_TBusyArg'(element(2,Val), [<<191,69>>]);
      tDISArg ->
         'INCS3opsargs':'enc_TDisconnectArg'(element(2,Val), [<<191,68>>]);
      tAAArg ->
         'INCS3opsargs':'enc_TermAttemptAuthorizedArg'(element(2,Val), [<<191,67>>]);
      tAArg ->
         'INCS3opsargs':'enc_TerminationAttemptArg'(element(2,Val), [<<191,66>>]);
      tNOANSArg ->
         'INCS3opsargs':'enc_TNoAnswerArg'(element(2,Val), [<<191,65>>]);
      tSUSArg ->
         'INCS3opsargs':'enc_TSuspendedArg'(element(2,Val), [<<191,64>>]);
      hCINArg ->
         'INCS3opsargs':'enc_HoldCallInNetworkArg'(element(2,Val), [<<191,63>>]);
      Else -> 
         exit({error,{asn1,{invalid_choice_type,Else}}})
   end,

encode_tags(TagIn, EncBytes, EncLen).




'dec_ArgType'(Tlv) ->
   'dec_ArgType'(Tlv, []).

'dec_ArgType'(Tlv, TagIn) ->
Tlv1 = match_tags(Tlv, TagIn),
case (case Tlv1 of [CtempTlv1] -> CtempTlv1; _ -> Tlv1 end) of

%% 'aSFArg'
    {131072, V1} -> 
        {aSFArg, 'INCS3opsargs':'dec_ActivateServiceFilteringArg'(V1, [])};


%% 'aSFRArg'
    {131073, V1} -> 
        {aSFRArg, decode_null(V1, [])};


%% 'aTArg'
    {131074, V1} -> 
        {aTArg, decode_null(V1, [])};


%% 'aTRArg'
    {131075, V1} -> 
        {aTRArg, decode_null(V1, [])};


%% 'aCArg'
    {131076, V1} -> 
        {aCArg, 'INCS3opsargs':'dec_ApplyChargingArg'(V1, [])};


%% 'aCRArg'
    {131077, V1} -> 
        {aCRArg, begin
Val1 = decode_octet_string(V1, []),
C1 = byte_size(Val1),
if 1 =< C1, C1 =< 27 ->
Val1;
true ->
exit({error,{asn1,bad_range}})
end
end};


%% 'aRIArg'
    {131078, V1} -> 
        {aRIArg, 'INCS3opsargs':'dec_AssistRequestInstructionsArg'(V1, [])};


%% 'cGArg'
    {131079, V1} -> 
        {cGArg, 'INCS3opsargs':'dec_CallGapArg'(V1, [])};


%% 'cIRArg'
    {131080, V1} -> 
        {cIRArg, 'INCS3opsargs':'dec_CallInformationReportArg'(V1, [])};


%% 'cIRQArg'
    {131081, V1} -> 
        {cIRQArg, 'INCS3opsargs':'dec_CallInformationRequestArg'(V1, [])};


%% 'cANArg'
    {131082, V1} -> 
        {cANArg, 'INCS3opsargs':'dec_CancelArg'(V1, [])};


%% 'cIArg'
    {131083, V1} -> 
        {cIArg, 'INCS3opsargs':'dec_CollectInformationArg'(V1, [])};


%% 'cONArg'
    {131084, V1} -> 
        {cONArg, 'INCS3opsargs':'dec_ConnectArg'(V1, [])};


%% 'cTRArg'
    {131085, V1} -> 
        {cTRArg, 'INCS3opsargs':'dec_ConnectToResourceArg'(V1, [])};


%% 'cWAArg'
    {131086, V1} -> 
        {cWAArg, 'INCS3opsargs':'dec_ContinueWithArgumentArg'(V1, [])};


%% 'cSAArg'
    {131087, V1} -> 
        {cSAArg, 'INCS3opsargs':'dec_CreateCallSegmentAssociationArg'(V1, [])};


%% 'cSARArg'
    {131088, V1} -> 
        {cSARArg, 'INCS3opsargs':'dec_CreateCallSegmentAssociationResultArg'(V1, [])};


%% 'cUEArg'
    {131089, V1} -> 
        {cUEArg, decode_null(V1, [])};


%% 'dFCArg'
    {131171, V1} -> 
        {dFCArg, decode_null(V1, [])};


%% 'dFCWAArg'
    {131090, V1} -> 
        {dFCWAArg, 'INCS3opsargs':'dec_DisconnectForwardConnectionWithArgumentArg'(V1, [])};


%% 'dLArg'
    {131091, V1} -> 
        {dLArg, 'INCS3opsargs':'dec_DisconnectLegArg'(V1, [])};


%% 'dLRArg'
    {131092, V1} -> 
        {dLRArg, decode_null(V1, [])};


%% 'eRArg'
    {131093, V1} -> 
        {eRArg, 'INCS3opsargs':'dec_EntityReleasedArg'(V1, [])};


%% 'eTCArg'
    {131094, V1} -> 
        {eTCArg, 'INCS3opsargs':'dec_EstablishTemporaryConnectionArg'(V1, [])};


%% 'eNCArg'
    {131095, V1} -> 
        {eNCArg, 'INCS3opsargs':'dec_EventNotificationChargingArg'(V1, [])};


%% 'eRBArg'
    {131096, V1} -> 
        {eRBArg, 'INCS3opsargs':'dec_EventReportBCSMArg'(V1, [])};


%% 'fCIArg'
    {131097, V1} -> 
        {fCIArg, begin
Val2 = decode_octet_string(V1, []),
C2 = byte_size(Val2),
if 1 =< C2, C2 =< 27 ->
Val2;
true ->
exit({error,{asn1,bad_range}})
end
end};


%% 'iDPArg'
    {131098, V1} -> 
        {iDPArg, 'INCS3opsargs':'dec_InitialDPArg'(V1, [])};


%% 'iCAArg'
    {131099, V1} -> 
        {iCAArg, 'INCS3opsargs':'dec_InitiateCallAttemptArg'(V1, [])};


%% 'mTDArg'
    {131100, V1} -> 
        {mTDArg, 'INCS3opsargs':'dec_ManageTriggerDataArg'(V1, [])};


%% 'mTDRArg'
    {131101, V1} -> 
        {mTDRArg, 'INCS3opsargs':'dec_ManageTriggerDataResultArg'(V1, [])};


%% 'mCArg'
    {131102, V1} -> 
        {mCArg, 'INCS3opsargs':'dec_MergeCallSegmentsArg'(V1, [])};


%% 'mCRArg'
    {131103, V1} -> 
        {mCRArg, decode_null(V1, [])};


%% 'mCSArg'
    {131104, V1} -> 
        {mCSArg, 'INCS3opsargs':'dec_MoveCallSegmentsArg'(V1, [])};


%% 'mCSRArg'
    {131105, V1} -> 
        {mCSRArg, decode_null(V1, [])};


%% 'mLArg'
    {131106, V1} -> 
        {mLArg, 'INCS3opsargs':'dec_MoveLegArg'(V1, [])};


%% 'mLRArg'
    {131107, V1} -> 
        {mLRArg, decode_null(V1, [])};


%% 'rCArg'
    {131108, V1} -> 
        {rCArg, 'INCS3opsargs':'dec_ReleaseCallArg'(V1, [])};


%% 'rUArg'
    {131109, V1} -> 
        {rUArg, 'INCS3opsargs':'dec_ReportUTSIArg'(V1, [])};


%% 'rNCArg'
    {131110, V1} -> 
        {rNCArg, 'INCS3opsargs':'dec_RequestNotificationChargingEventArg'(V1, [])};


%% 'rRBArg'
    {131111, V1} -> 
        {rRBArg, 'INCS3opsargs':'dec_RequestReportBCSMEventArg'(V1, [])};


%% 'rRUArg'
    {131112, V1} -> 
        {rRUArg, 'INCS3opsargs':'dec_RequestReportUTSIArg'(V1, [])};


%% 'rTArg'
    {131113, V1} -> 
        {rTArg, 'INCS3opsargs':'dec_ResetTimerArg'(V1, [])};


%% 'sCIArg'
    {131114, V1} -> 
        {sCIArg, 'INCS3opsargs':'dec_SendChargingInformationArg'(V1, [])};


%% 'sSArg'
    {131115, V1} -> 
        {sSArg, 'INCS3opsargs':'dec_SendSTUIArg'(V1, [])};


%% 'sFRArg'
    {131116, V1} -> 
        {sFRArg, 'INCS3opsargs':'dec_ServiceFilteringResponseArg'(V1, [])};


%% 'sLArg'
    {131117, V1} -> 
        {sLArg, 'INCS3opsargs':'dec_SplitLegArg'(V1, [])};


%% 'sLRArg'
    {131118, V1} -> 
        {sLRArg, decode_null(V1, [])};


%% 'pAArg'
    {131119, V1} -> 
        {pAArg, 'INCS3opsargs':'dec_PlayAnnouncementArg'(V1, [])};


%% 'mRArg'
    {131120, V1} -> 
        {mRArg, 'INCS3opsargs':'dec_MessageReceivedArg'(V1, [])};


%% 'pACArg'
    {131121, V1} -> 
        {pACArg, 'INCS3opsargs':'dec_PromptAndCollectUserInformationArg'(V1, [])};


%% 'pARArg'
    {131122, V1} -> 
        {pARArg, 'INCS3opsargs':'dec_PromptAndReceiveMessageArg'(V1, [])};


%% 'rIArg'
    {131123, V1} -> 
        {rIArg, 'INCS3opsargs':'dec_ReceivedInformationArg'(V1, [])};


%% 'sCArg'
    {131124, V1} -> 
        {sCArg, 'INCS3opsargs':'dec_ScriptCloseArg'(V1, [])};


%% 'sEArg'
    {131125, V1} -> 
        {sEArg, 'INCS3opsargs':'dec_ScriptEventArg'(V1, [])};


%% 'sIArg'
    {131126, V1} -> 
        {sIArg, 'INCS3opsargs':'dec_ScriptInformationArg'(V1, [])};


%% 'sCRArg'
    {131127, V1} -> 
        {sCRArg, 'INCS3opsargs':'dec_ScriptRunArg'(V1, [])};


%% 'sRRArg'
    {131128, V1} -> 
        {sRRArg, decode_null(V1, [])};


%% 'aDIArg'
    {131170, V1} -> 
        {aDIArg, 'INCS3opsargs':'dec_AnalysedInformationArg'(V1, [])};


%% 'aIArg'
    {131169, V1} -> 
        {aIArg, 'INCS3opsargs':'dec_AnalyseInformationArg'(V1, [])};


%% 'aUTTArg'
    {131168, V1} -> 
        {aUTTArg, 'INCS3opsargs':'dec_AuthorizeTerminationArg'(V1, [])};


%% 'cDI'
    {131167, V1} -> 
        {cDI, 'INCS3opsargs':'dec_CollectedInformationArg'(V1, [])};


%% 'cTDArg'
    {131166, V1} -> 
        {cTDArg, 'INCS3opsargs':'dec_CreateOrRemoveTriggerDataArg'(V1, [])};


%% 'cTDRArg'
    {131165, V1} -> 
        {cTDRArg, 'INCS3opsargs':'dec_CreateOrRemoveTriggerDataResultArg'(V1, [])};


%% 'eRFArg'
    {131164, V1} -> 
        {eRFArg, 'INCS3opsargs':'dec_EventReportFacilityArg'(V1, [])};


%% 'fSAArg'
    {131163, V1} -> 
        {fSAArg, 'INCS3opsargs':'dec_FacilitySelectedAndAvailableArg'(V1, [])};


%% 'oABNArg'
    {131162, V1} -> 
        {oABNArg, 'INCS3opsargs':'dec_OAbandonArg'(V1, [])};


%% 'oANSArg'
    {131161, V1} -> 
        {oANSArg, 'INCS3opsargs':'dec_OAnswerArg'(V1, [])};


%% 'oCPBArg'
    {131160, V1} -> 
        {oCPBArg, 'INCS3opsargs':'dec_OCalledPartyBusyArg'(V1, [])};


%% 'oDISArg'
    {131159, V1} -> 
        {oDISArg, 'INCS3opsargs':'dec_ODisconnectArg'(V1, [])};


%% 'mIDCLArg'
    {131158, V1} -> 
        {mIDCLArg, 'INCS3opsargs':'dec_MidCallArg'(V1, [])};


%% 'oNOANSArg'
    {131157, V1} -> 
        {oNOANSArg, 'INCS3opsargs':'dec_ONoAnswerArg'(V1, [])};


%% 'oAArg'
    {131156, V1} -> 
        {oAArg, 'INCS3opsargs':'dec_OriginationAttemptArg'(V1, [])};


%% 'oAAArg'
    {131155, V1} -> 
        {oAAArg, 'INCS3opsargs':'dec_OriginationAttemptAuthorizedArg'(V1, [])};


%% 'oSUSArg'
    {131154, V1} -> 
        {oSUSArg, 'INCS3opsargs':'dec_OSuspendedArg'(V1, [])};


%% 'rECONArg'
    {131153, V1} -> 
        {rECONArg, 'INCS3opsargs':'dec_ReconnectArg'(V1, [])};


%% 'rCSRArg'
    {131152, V1} -> 
        {rCSRArg, 'INCS3opsargs':'dec_RequestCurrentStatusReportArg'(V1, [])};


%% 'rESCRArg'
    {131151, V1} -> 
        {rESCRArg, 'INCS3opsargs':'dec_RequestEveryStatusChangeReportArg'(V1, [])};


%% 'rFSMRArg'
    {131150, V1} -> 
        {rFSMRArg, 'INCS3opsargs':'dec_RequestFirstStatusMatchReportArg'(V1, [])};


%% 'rRFEArg'
    {131149, V1} -> 
        {rRFEArg, 'INCS3opsargs':'dec_RequestReportFacilityEventArg'(V1, [])};


%% 'rSFArg'
    {131148, V1} -> 
        {rSFArg, 'INCS3opsargs':'dec_RouteSelectFailureArg'(V1, [])};


%% 'sFArg'
    {131147, V1} -> 
        {sFArg, 'INCS3opsargs':'dec_SelectFacilityArg'(V1, [])};


%% 'sRArg'
    {131146, V1} -> 
        {sRArg, 'INCS3opsargs':'dec_SelectRouteArg'(V1, [])};


%% 'sENDFArg'
    {131145, V1} -> 
        {sENDFArg, 'INCS3opsargs':'dec_SendFacilityInformationArg'(V1, [])};


%% 'sSPArg'
    {131144, V1} -> 
        {sSPArg, 'INCS3opsargs':'dec_SetServiceProfileArg'(V1, [])};


%% 'sREPArg'
    {131143, V1} -> 
        {sREPArg, 'INCS3opsargs':'dec_StatusReportArg'(V1, [])};


%% 'tANSArg'
    {131142, V1} -> 
        {tANSArg, 'INCS3opsargs':'dec_TAnswerArg'(V1, [])};


%% 'tBUSArg'
    {131141, V1} -> 
        {tBUSArg, 'INCS3opsargs':'dec_TBusyArg'(V1, [])};


%% 'tDISArg'
    {131140, V1} -> 
        {tDISArg, 'INCS3opsargs':'dec_TDisconnectArg'(V1, [])};


%% 'tAAArg'
    {131139, V1} -> 
        {tAAArg, 'INCS3opsargs':'dec_TermAttemptAuthorizedArg'(V1, [])};


%% 'tAArg'
    {131138, V1} -> 
        {tAArg, 'INCS3opsargs':'dec_TerminationAttemptArg'(V1, [])};


%% 'tNOANSArg'
    {131137, V1} -> 
        {tNOANSArg, 'INCS3opsargs':'dec_TNoAnswerArg'(V1, [])};


%% 'tSUSArg'
    {131136, V1} -> 
        {tSUSArg, 'INCS3opsargs':'dec_TSuspendedArg'(V1, [])};


%% 'hCINArg'
    {131135, V1} -> 
        {hCINArg, 'INCS3opsargs':'dec_HoldCallInNetworkArg'(V1, [])};

      Else -> 
         exit({error,{asn1,{invalid_choice_tag,Else}}})
   end
.


%%================================
%%  ErrorType
%%================================
'enc_ErrorType'(Val) ->
    'enc_ErrorType'(Val, []).

'enc_ErrorType'(Val, TagIn) ->
   {EncBytes,EncLen} = case element(1,Val) of
      cancelPar ->
         encode_null(element(2,Val), [<<128>>]);
      cancelFailedPar ->
         'INCS3opsargs':'enc_CancelFailed'(element(2,Val), [<<161>>]);
      chainingRefusedPar ->
         encode_null(element(2,Val), [<<130>>]);
      eTCFailedPar ->
         encode_null(element(2,Val), [<<131>>]);
      improperCallerResponsePar ->
         encode_null(element(2,Val), [<<132>>]);
      missingCustomerRecordPar ->
         encode_null(element(2,Val), [<<133>>]);
      missingParameterPar ->
         encode_null(element(2,Val), [<<134>>]);
      parameterOutOfRangePar ->
         encode_null(element(2,Val), [<<135>>]);
      requestedInfoError ->
         case element(2,Val) of
unknownRequestedInfo -> encode_tags([<<136>>], [1], 1);
requestedInfoNotAvailable -> encode_tags([<<136>>], [2], 1);
Enumval8 -> exit({error,{asn1, {enumerated_not_in_range,Enumval8}}})
end;
      systemFailurePar ->
         case element(2,Val) of
unavailableResources -> encode_tags([<<137>>], [0], 1);
componentFailure -> encode_tags([<<137>>], [1], 1);
basicCallProcessingException -> encode_tags([<<137>>], [2], 1);
resourceStatusFailure -> encode_tags([<<137>>], [3], 1);
endUserFailure -> encode_tags([<<137>>], [4], 1);
screening -> encode_tags([<<137>>], [5], 1);
Enumval9 -> exit({error,{asn1, {enumerated_not_in_range,Enumval9}}})
end;
      taskRefusedPar ->
         case element(2,Val) of
generic -> encode_tags([<<138>>], [0], 1);
unobtainable -> encode_tags([<<138>>], [1], 1);
congestion -> encode_tags([<<138>>], [2], 1);
Enumval10 -> exit({error,{asn1, {enumerated_not_in_range,Enumval10}}})
end;
      unavailableResourcePar ->
         encode_null(element(2,Val), [<<139>>]);
      unexpectedComponentSequencePar ->
         encode_null(element(2,Val), [<<140>>]);
      unexpectedDataValuePar ->
         encode_null(element(2,Val), [<<141>>]);
      unexpectedParameterPar ->
         encode_null(element(2,Val), [<<142>>]);
      unknownLegIDPar ->
         encode_null(element(2,Val), [<<143>>]);
      unknownRecordedMessageID ->
         encode_null(element(2,Val), [<<144>>]);
      unknownResource ->
         encode_null(element(2,Val), [<<145>>]);
      unknownSubscriber ->
         encode_null(element(2,Val), [<<146>>]);
      Else -> 
         exit({error,{asn1,{invalid_choice_type,Else}}})
   end,

encode_tags(TagIn, EncBytes, EncLen).




'dec_ErrorType'(Tlv) ->
   'dec_ErrorType'(Tlv, []).

'dec_ErrorType'(Tlv, TagIn) ->
Tlv1 = match_tags(Tlv, TagIn),
case (case Tlv1 of [CtempTlv1] -> CtempTlv1; _ -> Tlv1 end) of

%% 'cancelPar'
    {131072, V1} -> 
        {cancelPar, decode_null(V1, [])};


%% 'cancelFailedPar'
    {131073, V1} -> 
        {cancelFailedPar, 'INCS3opsargs':'dec_CancelFailed'(V1, [])};


%% 'chainingRefusedPar'
    {131074, V1} -> 
        {chainingRefusedPar, decode_null(V1, [])};


%% 'eTCFailedPar'
    {131075, V1} -> 
        {eTCFailedPar, decode_null(V1, [])};


%% 'improperCallerResponsePar'
    {131076, V1} -> 
        {improperCallerResponsePar, decode_null(V1, [])};


%% 'missingCustomerRecordPar'
    {131077, V1} -> 
        {missingCustomerRecordPar, decode_null(V1, [])};


%% 'missingParameterPar'
    {131078, V1} -> 
        {missingParameterPar, decode_null(V1, [])};


%% 'parameterOutOfRangePar'
    {131079, V1} -> 
        {parameterOutOfRangePar, decode_null(V1, [])};


%% 'requestedInfoError'
    {131080, V1} -> 
        {requestedInfoError, case decode_integer(V1, []) of
1 -> unknownRequestedInfo;
2 -> requestedInfoNotAvailable;
Default1 -> exit({error,{asn1,{illegal_enumerated,Default1}}})
end};


%% 'systemFailurePar'
    {131081, V1} -> 
        {systemFailurePar, case decode_integer(V1, []) of
0 -> unavailableResources;
1 -> componentFailure;
2 -> basicCallProcessingException;
3 -> resourceStatusFailure;
4 -> endUserFailure;
5 -> screening;
Default2 -> exit({error,{asn1,{illegal_enumerated,Default2}}})
end};


%% 'taskRefusedPar'
    {131082, V1} -> 
        {taskRefusedPar, case decode_integer(V1, []) of
0 -> generic;
1 -> unobtainable;
2 -> congestion;
Default3 -> exit({error,{asn1,{illegal_enumerated,Default3}}})
end};


%% 'unavailableResourcePar'
    {131083, V1} -> 
        {unavailableResourcePar, decode_null(V1, [])};


%% 'unexpectedComponentSequencePar'
    {131084, V1} -> 
        {unexpectedComponentSequencePar, decode_null(V1, [])};


%% 'unexpectedDataValuePar'
    {131085, V1} -> 
        {unexpectedDataValuePar, decode_null(V1, [])};


%% 'unexpectedParameterPar'
    {131086, V1} -> 
        {unexpectedParameterPar, decode_null(V1, [])};


%% 'unknownLegIDPar'
    {131087, V1} -> 
        {unknownLegIDPar, decode_null(V1, [])};


%% 'unknownRecordedMessageID'
    {131088, V1} -> 
        {unknownRecordedMessageID, decode_null(V1, [])};


%% 'unknownResource'
    {131089, V1} -> 
        {unknownResource, decode_null(V1, [])};


%% 'unknownSubscriber'
    {131090, V1} -> 
        {unknownSubscriber, decode_null(V1, [])};

      Else -> 
         exit({error,{asn1,{invalid_choice_tag,Else}}})
   end
.

%%%
%%% Run-time functions.
%%%

'dialyzer-suppressions'(Arg) ->
    ok.

ber_decode_nif(B) ->
    asn1rt_nif:decode_ber_tlv(B).

collect_parts(TlvList) ->
    collect_parts(TlvList, []).

collect_parts([{_,L}|Rest], Acc) when is_list(L) ->
    collect_parts(Rest, [collect_parts(L)|Acc]);
collect_parts([{3,<<Unused,Bits/binary>>}|Rest], _Acc) ->
    collect_parts_bit(Rest, [Bits], Unused);
collect_parts([{_T,V}|Rest], Acc) ->
    collect_parts(Rest, [V|Acc]);
collect_parts([], Acc) ->
    list_to_binary(lists:reverse(Acc)).

collect_parts_bit([{3,<<Unused,Bits/binary>>}|Rest], Acc, Uacc) ->
    collect_parts_bit(Rest, [Bits|Acc], Unused + Uacc);
collect_parts_bit([], Acc, Uacc) ->
    list_to_binary([Uacc|lists:reverse(Acc)]).

decode_integer(Tlv, TagIn) ->
    Bin = match_tags(Tlv, TagIn),
    Len = byte_size(Bin),
    <<Int:Len/signed-unit:8>> = Bin,
    Int.

decode_null(Tlv, Tags) ->
    Val = match_tags(Tlv, Tags),
    case Val of
        <<>> ->
            'NULL';
        _ ->
            exit({error,{asn1,{decode_null,Val}}})
    end.

decode_octet_string(Tlv, TagsIn) ->
    Bin = match_and_collect(Tlv, TagsIn),
    binary:copy(Bin).

encode_length(L) when L =< 127 ->
    {[L],1};
encode_length(L) ->
    Oct = minimum_octets(L),
    Len = length(Oct),
    if
        Len =< 126 ->
            {[128 bor Len|Oct],Len + 1};
        true ->
            exit({error,{asn1,too_long_length_oct,Len}})
    end.

encode_null(_Val, TagIn) ->
    encode_tags(TagIn, [], 0).

encode_restricted_string(OctetList, TagIn) when is_binary(OctetList) ->
    encode_tags(TagIn, OctetList, byte_size(OctetList));
encode_restricted_string(OctetList, TagIn) when is_list(OctetList) ->
    encode_tags(TagIn, OctetList, length(OctetList)).

encode_tags([Tag|Trest], BytesSoFar, LenSoFar) ->
    {Bytes2,L2} = encode_length(LenSoFar),
    encode_tags(Trest,
                [Tag,Bytes2|BytesSoFar],
                LenSoFar + byte_size(Tag) + L2);
encode_tags([], BytesSoFar, LenSoFar) ->
    {BytesSoFar,LenSoFar}.

match_and_collect(Tlv, TagsIn) ->
    Val = match_tags(Tlv, TagsIn),
    case Val of
        [_|_] = PartList ->
            collect_parts(PartList);
        Bin when is_binary(Bin) ->
            Bin
    end.

match_tags({T,V}, [T]) ->
    V;
match_tags({T,V}, [T|Tt]) ->
    match_tags(V, Tt);
match_tags([{T,V}], [T|Tt]) ->
    match_tags(V, Tt);
match_tags([{T,_V}|_] = Vlist, [T]) ->
    Vlist;
match_tags(Tlv, []) ->
    Tlv;
match_tags({Tag,_V} = Tlv, [T|_Tt]) ->
    exit({error,{asn1,{wrong_tag,{{expected,T},{got,Tag,Tlv}}}}}).

minimum_octets(0, Acc) ->
    Acc;
minimum_octets(Val, Acc) ->
    minimum_octets(Val bsr 8, [Val band 255|Acc]).

minimum_octets(Val) ->
    minimum_octets(Val, []).

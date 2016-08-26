%%%-------------------------------------------------------------------
%%% @author Eugeny Bachar <eugeny.bachar@eltex.nsk.ru>
%%% @copyright (C) 2010, Eugeny Bachar
%%% @doc
%%%  Adapter Core Protocol, INCS3Internals  - headers
%%% @end
%%% Created :  5 Oct 2010 by Eugeny Bachar <eugeny.bachar@eltex.nsk.ru>
%%%-------------------------------------------------------------------


-record('CallProgressType',
        {
          cause                   :: 'Cause'(),
          causeInitiator = system :: 'CauseInitiator'(),
          causeDescription = []   :: string(),
          causeIsup               :: undefined | 'CauseIsup'(),
          eventInformation        :: undefined | 'EventInformation'(),
          additionalInfo          :: undefined | 'ListOfAdditionalInformation'(),
%%          collectInfo    :: undefined | 'CollectInformation'(),
          oBCI                    :: undefined | 'OptionalBackwardCallIndicators'(),
          gNotification           :: undefined | 'GenericNotificationIndicatorList'(),
          redirectionNumber       :: undefined | 'RedirectionNumber'(),
          redirectionRestInd      :: undefined | 'RedirectionRestrictionIndicator'(),
          callDiversionInfo       :: undefined | 'CallDiversionInformation'(),
          facility                :: undefined | 'FacilityType'(),
          sdp                     :: undefined | 'SDPType'(),
          mediaPoint              :: undefined | 'MediaPointType'(),
          trunkGroupId            :: undefined | 'TrunkGroupId'(),
          callTransferNumber      :: undefined | 'CallTransferNumber'(),
          refer                   :: undefined | 'ReferType'(),
          eventTime               :: 'EventTime'()
        }).
-type 'CallProgressType'() :: #'CallProgressType'{}.

-record('FailureType',
        {
          domain                  :: nonempty_string(),
          cause                   :: 'Cause'(),
          causeInitiator = system :: 'CauseInitiator'(),
          causeIsup               :: undefined | 'CauseIsup'()
        }).
-type 'FailureType'() :: #'FailureType'{}.

-record('NetworkSRType',
        {
        }).
-type 'NetworkSRType'() :: #'NetworkSRType'{}.

-record('ReleaseType',
        {
          cause                   :: 'Cause'(),
          causeInitiator = system :: 'CauseInitiator'(),
          causeDescription = []   :: string(),
          dialledDigits           :: undefined | 'Digits'(),
          causeIsup               :: undefined | 'CauseIsup'(),
          additionalInfo          :: undefined | 'ListOfAdditionalInformation'(),
          trunkGroupId            :: undefined | 'TrunkGroupId'(),
          sid                     :: undefined | 'SID'(),
          refer                   :: undefined | 'ReferType'(),
          need_ack = true         :: boolean(), %%false in case release_ack is not expected
          eventTime               :: 'EventTime'()
        }).
-type 'ReleaseType'() :: #'ReleaseType'{}.

-record('ReleaseAckType',
        {
          dialledDigits           :: undefined | 'Digits'()
        }).
-type 'ReleaseAckType'() :: #'ReleaseAckType'{}.

-record('ServiceFeatureType',
        {
          cause = flash           :: flash | refer, %% undefined сейчас будет интерпретироваться так же как и flash
          additionalInfo          :: undefined | 'ListOfAdditionalInformation'()
        }).
-type 'ServiceFeatureType'() :: #'ServiceFeatureType'{}.

-record('SetupCRType',
        {
          connectedNumber :: undefined | 'CallingPartyNumber'(),
          additionalInfo  :: undefined | 'ListOfAdditionalInformation'(),
          redirectionNumber  :: undefined | 'RedirectionNumber'(),
          redirectionRestInd :: undefined | 'RedirectionRestrictionIndicator'(),
          sdp             :: 'SDPType'(),
          obsoleteMediaPoint      :: undefined | 'MediaPointType'(),
          refer           :: undefined | 'ReferType'(),
          eventTime       :: 'EventTime'()
        }).
-type 'SetupCRType'() :: #'SetupCRType'{}.

-type 'SetupModeType'() :: normal | dummy | internal | callback | parking | supervise | acd.
%% acd - вызов, распределенный очередью контакт-центра

-record('SetupIRType',
        {
          domain                     :: undefined | nonempty_string(),
          calledPartyNumber          :: undefined | 'CalledPartyNumber'(),
          calledPartysCategory       :: undefined | 'CalledPartysCategory'(),
          callingPartyNumber         :: undefined | 'CallingPartyNumber'(),
          callingPartysCategory      :: undefined | 'CallingPartysCategory'(),
          locationNumber             :: undefined | list(),
          originalCalledNumber       :: undefined | 'OriginalCalledNumber'(),
          userTeleserviceInformation :: undefined | list(),
          genericNumber              :: undefined | 'GenericNumbers'(),
          forwardCallIndicators      :: undefined | 'ForwardCallIndicators'(),
          redirectingNumber          :: undefined | 'RedirectingNumber'(),
          redirectingInformation     :: undefined | 'RedirectionInformation'(),
          uSIServiceIndicator        :: undefined | 'USIServiceIndicator'(),
          uSIInformation             :: undefined | 'USIInformation'(),
          isupCallRef                :: undefined | 'ISUPCallReference'(),
          sdp                        :: undefined | 'SDPType'(),
          mediaPoint                 :: undefined | 'MediaPointType'(),
%          serviceData                :: undefined | 'ServiceDataContainer'(),
%          serviceTriggers            :: undefined | 'ServiceTriggersContainer'(),
          additionalInfo = []        :: [] | 'ListOfAdditionalInformation'(),
          trunkGroupId               :: undefined | 'TrunkGroupId'(),
          callingPartyInfo           :: undefined | list(), %% property list of A alias info
          calledPartyInfo            :: undefined | list(), %% property list of B alias info for internal Core usage
          callingIfaceInfo = []      :: list(), %% property list of A iface info
          calledIfaceInfo = []       :: list(), %% property list of B iface info for internal Core usage
          refer                      :: undefined | 'ReferType'(),
          mode = normal              :: 'SetupModeType'(),
          priority = '5_routine'     :: 'PrecedenceLevel'() | 'MLPPPrecedence'(),
          eventTime                  :: 'EventTime'()}).
-type 'SetupIRType'() :: #'SetupIRType'{}.

-record('SetupAckType',
        {
          refer :: undefined | 'ReferType'(),
          trunkGroupId :: undefined | 'TrunkGroupId'()
         }).
-type 'SetupAckType'() :: #'SetupAckType'{}.

-record('SubsequentAddressType',
        {
          digits  :: 'Digits'(),
          sdp            :: undefined | 'SDPType'()
        }).
-type 'SubsequentAddressType'() :: #'SubsequentAddressType'{}.

-type 'InfoDirection'() ::
        utsi | %0
        stui.  %1

-record('UserDataType',
        {
          uSIServiceIndicator :: 'USIServiceIndicator'(),
          uSIInformation      :: 'USIInformation'(),
          infoDirection       :: 'InfoDirection'()
        }).
-type 'UserDataType'() :: #'UserDataType'{}.

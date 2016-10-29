%%%-------------------------------------------------------------------
%%% @author Eugeny Bachar <eugeny.bachar@eltex.nsk.ru>
%%% @copyright (C) 2010, Eugeny Bachar
%%% @doc
%%%  Adapter Core Protocol, INCS3datatypes  - headers
%%% @end
%%% Created :  5 Oct 2010 by Eugeny Bachar <eugeny.bachar@eltex.nsk.ru>
%%%-------------------------------------------------------------------

-type 'OBJECT IDENTIFIER'() :: list().
-type 'TriggerID'() :: integer().
-type 'TriggerData'() :: list('TriggerID'()).
-type 'Extensions'() :: list('ExtensionField'()).
-type 'ExtensionField'() :: list().

-type 'ExportLegsType'() :: list({SourceLeg::'DTLegID'(), NewLeg::'DTLegID'()}).

-type 'Integer4'() :: integer().
-type 'InvokeId'() :: integer().

%%% From Remote-Operations-Information-Objects
-type 'Code'() :: {local, integer()} | {global, 'OBJECT IDENTIFIER'()}.

%%% Types which are defined in  Q.1238.3
-record('CollectedDigits',
        {
          minimumNbOfDigits = 1      :: integer(), %INTEGER (1..127)	DEFAULT 1,
          maximumNbOfDigits          :: integer(), %INTEGER (1..127),
          endOfReplyDigit            :: undefined | list(), %OCTET STRING (SIZE (1..2))	OPTIONAL,
          cancelDigit                :: undefined | list(), %OCTET STRING (SIZE (1..2))	OPTIONAL,
          startDigit                 :: undefined | list(), %OCTET STRING (SIZE (1..2))	OPTIONAL,
          firstDigitTimeOut          :: undefined | integer(), %INTEGER (1..127)	OPTIONAL,
          interDigitTimeOut          :: undefined | integer(), %INTEGER (1..127)	OPTIONAL,
          errorTreatment             :: 'ErrorTreatment'(), %--	DEFAULT reportErrorToScf,
          interruptableAnnInd = true :: boolean(),
          voiceInformation = false   :: boolean(),
          voiceBack = false          :: boolean(),
          detectModem = false        :: boolean()
        }).
-type 'CollectedDigits'() :: #'CollectedDigits'{}.

%%% The use of voiceBack is network operator specific.
%%% The endOfReplyDigit, cancelDigit, and startDigit parameters have been designated as OCTET STRING,
%%% and are to be encoded as  BCD, one digit per octet only, contained
%%% in the four least significant bits of each OCTET. The usage is service dependent.

-type 'CollectedInfo'() ::
        {collectedDigits, 'CollectedDigits'()} |
        {iA5Information, boolean()} |
        {detectModem, boolean()}.

-type 'ElementaryMessageID'() :: 'Integer4'().

-type 'ErrorTreatment'() :: reportErrorToScf | %(0),
                            help |             %(1),
                            repeatPrompt.      %(2)
%%% reportErrorToScf means returning the "ImproperCallerResponse" error in the event of an error
%%% condition during collection of user info.

-type 'GapOnResource'() :: 'Code'().

-record('InbandInfo',
        {
          messageID           :: 'MessageID'() ,
          numberOfRepetitions :: undefined | integer(), %INTEGER (1..127)	OPTIONAL,
          duration            :: undefined | integer(), %INTEGER (0..32767)	OPTIONAL,
          interval            :: undefined | integer(), %INTEGER (0.. 32767)	OPTIONAL,
          preferredLanguage   :: undefined | 'Language'()
        }).
-type 'InbandInfo'() :: #'InbandInfo'{}.

%%% Interval is the time in seconds between each repeated announcement. Duration is the total
%%% amount of time in seconds, including repetitions and intervals.
%%% The end of announcement is either the end of duration or numberOfRepetitions, whatever comes first.
%%% duration with value 0 indicates infinite duration

-record('InformationToRecord',
        {
          messageID              :: undefined | 'ElementaryMessageID'(),
          messageDeletionTimeOut :: undefined | integer(), %INTEGER (1..3600)	OPTIONAL,--Time units = hours
          timeToRecord           :: undefined | integer(), %INTEGER (0..maxRecordingTime)	OPTIONAL,--Time units = seconds
          controlDigits	         :: 'ControlDigits'()
		}).
-type 'InformationToRecord'() :: #'InformationToRecord'{}.

-record('ControlDigits',
        {
          endOfRecordingDigit    :: undefined | list(), %OCTET STRING (SIZE(1..2))	OPTIONAL,
          cancelDigit            :: undefined | list(), %OCTET STRING (SIZE(1..2))	OPTIONAL,
          replayDigit            :: undefined | list(), %OCTET STRING (SIZE(1..2))	OPTIONAL,
          restartRecordingDigit  :: undefined | list(), %OCTET STRING (SIZE(1..2))	OPTIONAL,
          restartAllowed = false :: boolean(),
          replayAllowed = false  :: boolean()
        }).
-type 'ControlDigits'() :: #'ControlDigits'{}.

-type 'InformationToSend'()  ::
        {inbandInfo, 'InbandInfo'()} |
        {tone, 'Tone'()} |
        {displayInformation, 'DisplayInformation'()} |
        {sDSSinformation, 'SDSSinformation'()}.

-type 'Language'() :: list(). % PrintableString (SIZE (3) ) -- ISO 639 codes only;


-type 'MailBoxID'() :: list(). %OCTET STRING (SIZE(minMailBoxIDLength..maxMailBoxIDLength))

-type 'Media'() :: voiceMail | % (0),
                   faxGroup3 | % (1),
                   faxGroup4.  % (2)

-type 'MessageID'() ::
        {elementaryMessageID, 'Integer4'()} |
        {text,
         MessageContent::list(), %IA5String (SIZE (minMessageContentLength..maxMessageContentLength)),
         Attributes::list()      %OCTET STRING (SIZE (minAttributesLength..maxAttributesLength)) OPTIONAL
                     } |
        {elementaryMessageIDs, ['Integer4'()]} |
        {variableMessage,
         ElementaryMessageID::'Integer4'(),
         VariableParts::['VariablePart'()]
                        }.
%%% OPTIONAL denotes network operator specific use.


-type 'ReceivedStatus'() :: messageComplete |    % (0),
                            messageInterrupted | % (1),
                            messageTimeOut.      % (2)

-type 'RecordedMessageID'() :: 'Integer4'().

-type 'SRFGapCriteria'() ::
        {iPAddressValue, 'Digits'()} |
        {gapOnResource, 'GapOnResource'()} |
        {iPAddressAndresource, IPAddressValue::'Digits'() , GapOnResource::'GapOnResource'()}.

-record('Tone',
        {
          toneID   :: 'Integer4'(),
          duration :: undefined | 'Integer4'()
        }).
-type 'Tone'() :: #'Tone'{}.
%%% The duration specifies the length of the tone in seconds, value 0 indicates infinite duration.

-type 'VariablePart'() ::
	{integer, 'Integer4'()} |
	{number, 'Digits'()} | % Generic digits
	{time, list()} |       %OCTET STRING (SIZE(2)),	-- HH:MM, BCD coded
	{date, list()} |       %OCTET STRING (SIZE(3)),	-- YYMMDD, BCD coded
	{price, list()}.       %OCTET STRING (SIZE(4))

%%% Indicates the variable part of the message.
%%% BCD coded variable parts are encoded as described in the examples below.
%%% For example, time = 12:15 would be encoded as:
%%% 	Bits					HGFE 		DCBA
%%% 	leading octet	2	1
%%% 			5	1
%%% date = 1993 September 30th would be encoded as:
%%% 	Bits					HGFE		DCBA
%%% 	leading octet 	3	9
%%% 			9	0
%%% 			0	3
%%% For a system operating when or after this Recommendation is released, the 2 digit value
%%% representing a Year shall be interpreted as follows :
%%%        - If the two-digits value is 00 through 49 inclusive, it shall be interpreted as representing
%%%          year  2000  through 2049.
%%%       -  If the two-digits value is 50 through 99 inclusive, it shall be interpreted as representing
%%%         year 1950 through 1999.

%%% End of added definitions.
%%% *****


%%% The following three definitions are local short-hand notation for convenience.
%%% B1 ::= COMMON-BOUNDS		 defined in Q.1238.1 (Part 1 of Recommendation Q.1238)
%%% B2 ::= SCF-SSF-BOUNDS		 defined in this Recommendation (Q.1238.2)
%%% B3 ::= SCF-SRF-BOUNDS		 defined in Q.1238.3 (Part 3 of Recommendation Q.1238)

-type 'AccessCode'() :: list(). %OCTET STRING %% 'LocationNumber'().

%%% An access code from a business group dialling plan attendant access codes, access codes to escape
%%% to the public network, access code to access a private facility/network, and feature access codes.
%%% Uses the LocationNumber format which is based on the Q.763 Location Number format.
%%% The Nature of Address indicator field shall be set to "Spare" (value 00000000).
%%% The Numbering Plan Indicator field shall be set to "Spare" (value 000).
%%% Of local significance.


-type 'AChBillingChargingCharacteristics'() :: list(). % OCTET STRING (SIZE
                                                %(minAChBillingChargingLength..maxAChBillingChargingLength))
%%% The AChBillingChargingCharacteristics parameter specifies the charging related information
%%% to be provided by the SSF and the conditions on which this information has to be reported
%%% back to the SCF with the ApplyChargingReport operation.
%%% Different set of criteria may be provided in case more than one report is expected.
%%% Its content is network operator specific.
%%% Examples of charging related information to be provided by the SSF may be: bulk counter
%%% values, costs, tariff change and time of charge, time stamps, durations, etc.
%%% Examples of conditions on which the charging related information are to be reported may be:
%%% threshold value reached, timer expiration, tariff change, end of connection configuration, etc

-type 'ActionIndicator'() :: activate |  % (1) ,
                             deactivate |% (2) ,
                             retrieve.   % (3)
%%% indicates the action to be performed by the ManageTriggerData operation (activate, deactivate
%%% or retrieve the status of a TDP.


-type 'ActionOnProfile'() :: activate |  % (0),
                             deactivate. % (1)

-type 'ActionPerformed'() :: activated |       % (1) ,
                             deactivated |     % (2) ,
                             alreadyActive |   % (3) ,
                             alreadyInactive | % (4) ,
                             isActive |        % (5) ,
                             isInactive |      % (6),
                             tDPunknown.       % (7)

%%% indicates the result of the operation ManageTriggerData
%%% activated: response of activate TDP
%%% deactivated: response of deactivate TDP
%%% alreadyActive: response of activate TDP
%%% alreadyInactive: response of deactivate TDP
%%% isActive: response of retrieve status of TDP
%%% isInactive: response of retrieve status of TDP

-record('ActivableServices',
        {
          callingLineIdentificationPresentation = false   :: boolean(),
          callingLineIdentificationRestriction = false    :: boolean(),
          connectedLineIdentificationPresentation = false :: boolean(),
          connectedLineIdentificationRestriction = false  :: boolean(),
          callForwardingOnNoReply = false                 :: boolean(),
          callForwardingUnconditional = false             :: boolean(),
          callForwardingOnBusy = false                    :: boolean(),
          callForwardingOnNotReachable = false            :: boolean(),
          reverseCharging = false                         :: boolean(),
          adviceOfChargeOnStart = false                   :: boolean(),
          adviceOfChargeAtEnd = false                     :: boolean(),
          adviceOfChargeDuringCall = false                :: boolean(),
          timeDependentRouting = false                    :: boolean(),
          callingPartingDependentRouting = false          :: boolean(),
          outgoingCallBarring = false                     :: boolean(),
          incomingCallBarring = false                     :: boolean()
        }).
-type 'ActivableServices'() :: #'ActivableServices'{}.

-type 'AdditionalCallingPartyNumber'()  :: 'Digits'().
%%% Indicates the Additional Calling Party Number.  Refer to Q.763 for encoding.

-record('ReferTo', {refer_number :: 'Digits'(), args = [] :: list({Key :: atom(), Value :: term()})}).
-type 'ReferTo'() :: #'ReferTo'{}.

-type 'ReplacesCP'() :: term().

-type 'AdditionalInformation'() :: {forISUP, 'AdditionalISUP'()} |
                                   {forMegaco, 'AdditionalMegaco'()} |
                                   {forSIP, 'AdditionalSIP'() | list('AdditionalSIP'())} |
                                   {applicationInfo, [{Key :: atom(), Value :: term()}]} |
                                   {ssNotification, 'SSNotification'()} |
                                   {recordingFile, File :: nonempty_string()} | %% for notification recording file by voice mail from ivr
                                   {mediaInfo, {CallIdA :: nonempty_string(), CallIdB :: nonempty_string()}} |
                                   {causeDescription, 'AdditionalCauseDescription'()} | %% depricated from 3.7.0
                                   {callingInfo, 'AdditionalCallingInfo'()} | %% depricated from 3.7.0
                                   {ivrInfo, ['AdditionalIvrInfo'()]} |
                                   {msInfo, 'MSInfo'()} |
                                   {dtmf, DtmfCode :: integer()} | {dtmf, DtmfCode :: integer(), DtmfDuration :: none|term()} | %% refs #11522. Peer to peer DTMF transit.
                                   {forOther, integer()} |
                                   {pickup_number, _} |
                                   {autoAnswer, boolean()} | %% only for SetupIRType
                                   {ringSignalType, term()} |
                                   {isFocus, boolean()} |
                                   {replaces, 'CallId'()} |
                                   {replaces_cp, 'ReplacesCP'()} | %% only for internal core usage
                                   {refer_to, 'ReferTo'()} |
                                   {text, {Message :: string(), Args :: list()}}.

-type 'ListOfAdditionalInformation'() :: list('AdditionalInformation'()).
%%% Additional information
%%% The unspecified info that is depends on context.

-record('AdditionalISUP',
        {
          message :: list(), %IA5String (SIZE (minMessageContentLength..maxMessageContentLength)),
          params  :: undefined | ['AdditionalISUPParam'()] %SET OF AdditionalISUPParam OPTIONAL
        }).
-type 'AdditionalISUP'() :: #'AdditionalISUP'{}.
%%% Additional info for ISUP (e.g. optional params for IAM)

-record('AdditionalISUPParam',
        {
          type   :: integer(), %INTEGER (0..maxISUPParameterType),
          length :: integer(), %INTEGER (minISUPParameterLength..maxISUPParameterLength),
          value  :: list()|binary()     %OCTET STRING (SIZE(minISUPParameterLength..maxISUPParameterLength))
        }).
-type 'AdditionalISUPParam'() :: #'AdditionalISUPParam'{}.
%%% Additional parameter for ISUP (e.g. optional params for IAM).
%%% It has next structure: {Type, Length, Value}.

-type 'AdditionalIvrInfo'() :: {Key :: binary() | atom(), Value :: term()}. %% {<<"widget_id">>, binary()}
                                                                            %% {call_count, list({nonempty_string(), pos_integer()})}

-type 'AdditionalMegaco'() :: {transfer, 'AdditionalMegacoTransfer'()} |
                              {conference, 'AdditionalMegacoConference'()}.

-record('AdditionalMegacoTransfer',
        {
          callRef   :: 'CallReference'(),
          legRef    :: 'CallReference'()%,
%%%          remoteSDP :: 'SDPType'
        }).
-type 'AdditionalMegacoTransfer'() :: #'AdditionalMegacoTransfer'{}.

-record('AdditionalMegacoConference',
        {
          callRef   :: 'CallReference'(),
          legRef    :: 'CallReference'()%,
%%%          remoteSDP :: 'SDPType'
        }).
-type 'AdditionalMegacoConference'() :: #'AdditionalMegacoConference'{}.

-record('AdditionalSIP',
        {
          message :: list(),
          params  :: undefined | ['AdditionalSIPParam'()]
        }).
-type 'AdditionalSIP'() :: #'AdditionalSIP'{}.
%%% Additional info for ISUP (e.g. optional params for IAM)

-record('AdditionalSIPParam',
        {
          type   :: list(),    %OCTET STRING (SIZE(minISUPParameterLength..maxISUPParameterLength))
          value  :: list()     %OCTET STRING (SIZE(minISUPParameterLength..maxISUPParameterLength))
        }).
-type 'AdditionalSIPParam'() :: #'AdditionalSIPParam'{}.

-record('AdditionalCauseDescription',
        {
          value  :: term(),
          text   :: string()
        }).
-type 'AdditionalCauseDescription'() :: #'AdditionalCauseDescription'{}.


%%% Service code definitions for SORM usage
-define(CFU,         16#21).
-define(CFB,         16#29).
-define(CFNRY,       16#2A).
-define(CFNRC,       16#2B).
-define(ALL_CF,      16#20).
-define(ALL_COND_CF, 16#28).
-define(CW,          16#41).
-define(HOLD,        16#42).
-define(CCBS,        16#43).
-define(HOLE,        16#40).
-define('3WAY',      16#52).
-define(MPTY,        16#51).
-define(CONF,        16#50).
-define(CT,          16#31).
-define(CP,          16#32).
-define(CC,          16#33).

-type 'SSFamily'() :: '3WAY' |
                      'ACB' |
                      'CFB' |
                      'CFSip' |
                      'CFNR' |
                      'CFU' |
                      'CGG' |
                      'CHOLD' |
                      'CHUNT' |
                      'CIDB' |
                      'CTR' |
                      'DND' |
                      'MGM' |
                      'PICKUP'.

-type 'EventTime'() :: {MegaSec::integer(), Sec::integer(), MicSec::integer()}.

-type 'Participants'() :: 'CalledPartyNumber'() | 'CallingPartyNumber'().

-record('SSNotification',
        {family     :: 'SSFamily'(),
         cgpn       :: 'CallingPartyNumber'(),
         cdpn       :: 'CalledPartyNumber'(),
         servingSide :: calling | called,
         serviceTimeStamp :: 'EventTime'(),
         participants = [] :: list('Participants'()),
         internal = false :: boolean(),
         args = []   :: list({Key :: atom(), Value :: term()})
        }).
-type 'SSNotification'() :: #'SSNotification'{}.

-type 'AdditionalCallingInfo'() :: list({Key :: atom(), Value :: term()}).

-type 'MSKey'() :: correlationId | toneId.

-type 'MSInfo'() :: list({Key :: 'MSKey'(), Value :: term()}). %% Information for MediaService usage

-type 'AlertingPattern'() :: list(). % OCTET STRING (SIZE(3))
%%% Indicates a specific pattern that is used to alert a subscriber (e.g. distinctive ringing, tones, etc.).
%%% Only the trailing OCTET is used, the remaining OCTETS should be sent as NULL (zero)
%%% The receiving side ignores the leading two OCTETS.
%%% Only applies if SSF is the terminating local exchange for the subscriber.
%%% Refer to the Q.931  Signal parameter for encoding.

-type 'ApplicationTimer'() :: integer(). %INTEGER (0..2047)
%%% Used by the SCF to set a timer in the SSF. The timer is in seconds.

-type 'AssistingSSPIPRoutingAddress'() :: 'Digits'().
%%% Indicates the destination address of the SRF for the assist procedure.

-type 'ATMTrafficDescriptor'() :: list(). % OCTET STRING(SIZE(
%minATMTrafficDescriptorLength..maxATMTrafficDescriptorLength))

%%% Indicates the ATM Traffic Descriptors. Refer to Q.2961  for encoding.


-type 'BackwardGVNS'() :: list(). %OCTET STRING (SIZE(
%minBackwardGVNSLength..maxBackwardGVNSLength))
%%% Indicates the GVNS Backward information. Refer to Q.735, §6 for encoding.

-record('BackwardServiceInteractionInd',
        {
          conferenceTreatmentIndicator     :: undefined | list(),%OCTET STRING (SIZE(1))	OPTIONAL,
          %% acceptConferenceRequest	'xxxx xx01'B
          %% rejectConferenceRequest	'xxxx xx10'B
          %% network default is accept conference request,

          callCompletionTreatmentIndicator :: undefined | list() %OCTET STRING (SIZE(1))	OPTIONAL
          %% acceptCallCompletionServiceRequest	'xxxx xx01'B,
          %% rejectCallCompletionServiceRequest	'xxxx xx10'B
          %% network default is accept call completion service request
	    }).
-type 'BackwardServiceInteractionInd'() :: #'BackwardServiceInteractionInd'{}.

-record('BCSMEvent',
        {
          eventTypeBCSM      :: 'EventTypeBCSM'(),
          monitorMode        :: 'MonitorMode'(),
          legID              :: undefined | 'DTLegID'(),
          dpSpecificCriteria :: undefined | 'DpSpecificCriteria'()
        }).
-type 'BCSMEvent'() :: #'BCSMEvent'{}.
%%% Indicates the BCSM Event information for monitoring.

-type 'BearerCapability'() :: {bearerCap, list()} | %OCTET STRING (SIZE(2..maxBearerCapabilityLength)),
                              {tmr, list()} |       %OCTET STRING (SIZE(1)),
                              {broadbandBearerCap, list()}. %OCTET STRING
                                                            %(SIZE  	(minBroadbandBearerCapabilityLength..
                                                            %				maxBroadbandBearerCapabilityLength))
%%% Indicates the type of bearer capability connection to the user. For narrowband bearerCapability, either
%%% DSS 1 (Q.931) or the ISUP User Service Information (Q.763) encoding can be used. Refer
%%% to the Q.763 Transmission Medium Requirement parameter for tmr encoding.
%%% For broadband ISDN:  Indicates the Broadband Bearer Capability. Refer to Q.2961 for encoding.

-type 'BothwayThroughConnectionInd'() :: bothwayPathRequired |   % (0),
                                         bothwayPathNotRequired. % (1)

-type 'CalledDirectoryNumber'() :: list().      %    ::= OCTET STRING (SIZE
                                                %						(minCalledDirectoryNumberLength..
                                                %					maxCalledDirectoryNumberLength))
%%% Indicates the Called Directory Number. Refer to Q.769.1  'Called Directory Number' for encoding.


-type 'CalledPartyBusinessGroupID'() :: list().
%%% Indicates the business group of the called party. The value of this octet string is network
%%% operator specific.

-record('CalledPartyNumber',
        {
          nai         :: undefined | 'NAIType'(),  % Nature of address indicator
          ni          :: undefined | 'NIType'(),   % Number indicator
          incomplete  :: undefined | boolean(),    % number incomplete indicator (flase - complete, true - incomplete) - not by specs
          inni        :: undefined | 'INNIType'(), % Internal network number indicator
          npi         :: undefined | 'NPIType'(),  % Numbering plan indicator
          category    :: undefined | 'CallPartyCategory'(),
          digits      :: undefined | 'Digits'(),    % Number digits
          displayName :: undefined | string(),
          sipUri      :: undefined | string(),
          vdn         :: undefined | 'Digits'() % Virtual direct number
        }).
-type 'CalledPartyNumber'() :: #'CalledPartyNumber'{}.


-type 'CalledPartySubaddress'() :: list(). %OCTET STRING
                                                %	(SIZE (	minCalledPartySubaddressLength..
                                                %			maxCalledPartySubaddressLength))
%%% Indicates the Called Party Subaddress. Both N-ISDN and B-ISDN E.164 addresses are supported. Refer to Q.2763 for encoding.

-record('CallingGeodeticLocation',
        {
          lrpi :: 'LRPI'(),               %Location presentation restricted indicator
          screening :: 'ScreeningType'(), %Screening indicator
          shape :: 'Shape'()
        }).
-type 'CallingGeodeticLocation'() :: #'CallingGeodeticLocation'{}.

%%% The coding of this parameter is based on the appropriate mapping with the ISUP parameter Calling Geodetic Location.
%%% Refer to Q.763 for encoding.
%%% This parameter  indicates the geograhic coordinate of a calling party. The excessive amount of data possible
%%% within this parameter may require segmentation of the INAP operation to be-sent to the SCF.
%%% The amount of data possible to be conveyed within this  parameter  from
%%% the SSF -to the SCF could  be limitted, for example it may be considered to only support a relevant subset of all the
%%% shape descriptions. This is to be considered in the next Capability Set.

-type 'LRPI'() :: presentation_allowed |
                  presentation_restricted |
                  location_not_available |
                  spare.

-type 'Shape'() :: ellipsoid_point |
                   ellipsoind_point_with_uncertainty |
                   point_with_altitude_and_uncertatinty |
                   ellipse_on_the_ellipsoid |
                   ellipsoid_circle_sector |
                   polygon |
                   integer.

-type 'CalledPartysCategory'() ::
        unknownAtThisTime |     %0 - Calling party's category unknow at this time (national use)
        operatorFrench |        %1 - Operator, French language
        operatorEngish |        %2 - Operator, English language
        operatorGerman |        %3 - Operator, German language
        operatorRussian |       %4 - Operator, Russian language
        operatorSpanish |       %5 - Operator, Spanish language
        reserved |              %9 - Reserved (see ITU-T Q.104) (national use) (may be used as natinal operator)
        ordinarySubscriber |    %10 - Ordinary calling subscriber
        subscriberWithPriority| %11 - Calling subsciber with priority
        dataCall |              %12 - Data call (voice band data)
        testCall |              %13 - Test call
        spare    |              %14 - Spare
        payphone |              %15 - Payphone
        category0 |             %224 - Reserved. Category of subscriber's terminal is 0.
        hotelsSubscriber |      %225 - Hotel's subscriber. Category of subscribers termina'l is 2.
        freeSubscriber |        %226 - Calls for subscriber are free. Category of subscriber's terminal is 5.
        paidSubscriber |        %227 - Subscriber has access to paid services. Category of subscriber's terminal is 7.
        localSubscriber |       %228 - Subscriber has access to local network only. Category of subscriber's terminal is 3.
        localTaksofon |         %229 - Local taksofon. Category of subscriber's terminal is 9.
        autoCallI |             %240 - Automatic call. Category I.
        semiautoCallI |         %241 - Semi-automatic call. Category I.
        autoCallII |            %242 - Automatic call. Category II.
        semiautoCallII |        %243 - Semi-automatic call. Category II.
        autoCallIII |           %244 - Automatic call. Category III.
        semiautoCallIII |       %245 - Semi-automatic call. Category III.
        autoCallIV |            %246 - Automatic call. Category IV.
        semiautoCallIV.         %247 - Semi-automatic call. Category IV.

-type 'CallingPartyBusinessGroupID'() :: list(). % ::= OCTET STRING
%%% Indicates the business group of the calling party. The value of this octet string is network
%%% operator specific.
-record('CallerDisplayInformation',
        {
          showDisplayName = false     :: boolean(),
          displayName                 :: undefined | string(),
          showCallerId = false        :: boolean(),
          callerId                    :: undefined | 'Digits'()
        }).
-type 'CallerDisplayInformation'() :: #'CallerDisplayInformation'{}.

-record('CallingPartyNumber',
        {
          nai = subscriberNumber      :: 'NAIType'(),       % Nature of address indicator
          ni = private                :: 'NIType'() | undefined,  % Number Indicator
          incomplete = false          :: boolean(),         % number incomplete indicator (flase - complete, true - incomplete)
                                                            % - not by specs
          npi = isdnTelephony         :: 'NPIType'(),       % Numbering plan indicator
          apri = presentationAllowed  :: 'APRIType'(),      % Address presentation restricted inficator
          screening = networkProvided :: 'ScreeningType'(), % Screening indicator
          category                    :: undefined | 'CallingPartysCategory'(),
          digits                      :: 'Digits'(),        % Number digits
          callerDisplayInformation = #'CallerDisplayInformation'{} :: 'CallerDisplayInformation'(),
          sipUri                      :: undefined | string(),
          vdn                         :: undefined | 'Digits'() % Virtual direct number
        }).
-type 'CallingPartyNumber'() :: #'CallingPartyNumber'{}.
%%% Indicates the Calling Party Number. Both N-ISDN and B-ISDN E.164 addresses are supported. Refer to Q.2763 for encoding.

-type 'CallingPartySubaddress'() :: list().%OCTET STRING
                                                %	(SIZE (	minCallingPartySubaddressLength..
                                                %			maxCallingPartySubaddressLength))
%%% Indicates the Calling Party Subaddress. Both N-ISDN and B-ISDN E.164 addresses are supported. Refer to Q.2763  for encoding.

-type 'CallingPartysCategory'() ::
        unknownAtThisTime |     %0 - Calling party's category unknow at this time (national use)
        operatorFrench |        %1 - Operator, French language
        operatorEngish |        %2 - Operator, English language
        operatorGerman |        %3 - Operator, German language
        operatorRussian |       %4 - Operator, Russian language
        operatorSpanish |       %5 - Operator, Spanish language
        reserved |              %9 - Reserved (see ITU-T Q.104) (national use) (may be used as natinal operator)
        ordinarySubscriber |    %10 - Ordinary calling subscriber
        subscriberWithPriority| %11 - Calling subsciber with priority
        dataCall |              %12 - Data call (voice band data)
        testCall |              %13 - Test call
        spare    |              %14 - Spare
        payphone |              %15 - Payphone
        category0 |             %224 - Reserved. Category of subscriber's terminal is 0.
        hotelsSubscriber |      %225 - Hotel's subscriber. Category of subscribers termina'l is 2.
        freeSubscriber |        %226 - Calls for subscriber are free. Category of subscriber's terminal is 5.
        paidSubscriber |        %227 - Subscriber has access to paid services. Category of subscriber's terminal is 7.
        localSubscriber |       %228 - Subscriber has access to local network only. Category of subscriber's terminal is 3.
        localTaksofon |         %229 - Local taksofon. Category of subscriber's terminal is 9.
        autoCallI |             %240 - Automatic call. Category I.
        semiautoCallI |         %241 - Semi-automatic call. Category I.
        autoCallII |            %242 - Automatic call. Category II.
        semiautoCallII |        %243 - Semi-automatic call. Category II.
        autoCallIII |           %244 - Automatic call. Category III.
        semiautoCallIII |       %245 - Semi-automatic call. Category III.
        autoCallIV |            %246 - Automatic call. Category IV.
        semiautoCallIV.         %247 - Semi-automatic call. Category IV.

-type 'CallPartyCategory'() ::
        unknownAtThisTime |     %0 - Calling party's category unknow at this time (national use)
        operatorFrench |        %1 - Operator, French language
        operatorEngish |        %2 - Operator, English language
        operatorGerman |        %3 - Operator, German language
        operatorRussian |       %4 - Operator, Russian language
        operatorSpanish |       %5 - Operator, Spanish language
        reserved |              %9 - Reserved (see ITU-T Q.104) (national use) (may be used as natinal operator)
        ordinarySubscriber |    %10 - Ordinary calling subscriber
        subscriberWithPriority| %11 - Calling subsciber with priority
        dataCall |              %12 - Data call (voice band data)
        testCall |              %13 - Test call
        spare    |              %14 - Spare
        payphone |              %15 - Payphone
        category0 |             %224 - Reserved. Category of subscriber's terminal is 0.
        hotelsSubscriber |      %225 - Hotel's subscriber. Category of subscribers termina'l is 2.
        freeSubscriber |        %226 - Calls for subscriber are free. Category of subscriber's terminal is 5.
        paidSubscriber |        %227 - Subscriber has access to paid services. Category of subscriber's terminal is 7.
        localSubscriber |       %228 - Subscriber has access to local network only. Category of subscriber's terminal is 3.
        localTaksofon |         %229 - Local taksofon. Category of subscriber's terminal is 9.
        autoCallI |             %240 - Automatic call. Category I.
        semiautoCallI |         %241 - Semi-automatic call. Category I.
        autoCallII |            %242 - Automatic call. Category II.
        semiautoCallII |        %243 - Semi-automatic call. Category II.
        autoCallIII |           %244 - Automatic call. Category III.
        semiautoCallIII |       %245 - Semi-automatic call. Category III.
        autoCallIV |            %246 - Automatic call. Category IV.
        semiautoCallIV.         %247 - Semi-automatic call. Category IV.

-type 'CallProcessingOperationCorrelationID'() :: aLERTing |   % (1),
                                                  sETUP |      % (5),
                                                  cONNNect |   % (7),
                                                  dISConnect | % (69),
                                                  rELease |    % (77),
                                                  rELeaseCOMPlete | % (90),
                                                  fACility.    % (98)

-type 'CallReference'() :: integer(). %OCTET STRING (SIZE(1..maxCallReferenceLength))
-type 'ISUPCallReference'() :: list(integer()). %% list of bytes
%%% The coding of this parameter is based on the appropriate mapping with the ISUP parameter.
%%% Refer to Q.763 for encoding
%%% The Call Reference value is unique within one network. When transit through a private
%%% network the uniqueness of the call reference parameter is not maintained


%%% [editor note: new call reference parameter NOT included in  latest version of Q.763 ???]


-type 'CallResult'() :: list().% OCTET STRING (SIZE (minCallResultLength..maxCallResultLength))

%%% This parameter provides the SCF with the charging related information previously requested
%%% using the ApplyCharging operation. This shall include the partyToCharge parameter as
%%% received in the related ApplyCharging operation to correlate the result to the request
%%% The remaining content is network operator specific.

%%% Examples of charging related information to be provided by the SSF may be: bulk counter values,
%%% costs, tariff change and time of change, time stamps, durations, etc.
%%% Examples of conditions on which the charging related information are to be reported may be:
%%% threshold value reached, timer expiration, tariff change, end of connection configuration, etc.

-type 'DTCallSegmentID'() :: integer(). %INTEGER (1..numOfCSs)

-type 'Carrier'() :: no_indication | % 00000000		No indication
                     carrier_1 |     % 00000001		Selected carrier code pre subscribed and not input by calling party
                     carrier_2 |     % Selected carrier identification code pre subscribed and input by calling party
                     carrier_3 |     % 00000011		Selected carrier identification code pre subscribed,
                                     %              no indication of whether input by calling party
                     carrier_4 |      % 00000100 	Selected carrier identification code not pre subscribed and
                                     %              input by calling party
                     integer().

%%% Contains the carrier selection field  followed by either  Carrier ID information (option 1) , or the Transit
%%% Network selection information (option 2), depending on the network.
%%% In both cases, the Carrier selection is one octet and is encoded as:
%%% 00000000		No indication
%%% 00000001		Selected carrier code pre subscribed and not input by calling party
%%% 00000010		Selected carrier identification code pre subscribed and input by calling party
%%% 00000011		Selected carrier identification code pre subscribed, no indication of whether 				input by calling party
%%% 00000100 		Selected carrier identification code not pre subscribed and input by calling 				party
%%% 00000101
%%% 	to		Spare
%%% 11111110
%%% 11111111		Reserved

%%% For the first option, Carrier ID has a one octet field indicating the number of digits followed by the digits encoded
%%% using BCD. Detailed coding is outside the scope of this capability set. It is of local significance and carrying
%%% it through the ISUP is outside the scope of this capability set
%%% For the second option, refer to Q.763  for the TNS encoding.
%%% Note that this ASN.1 encoding of this parameter includes 2 possible encodings, referred to as option 1 and option 2.
%%% The encoding that should be used is dependent on the network. It is a hard-coded decision based
%%% on the region  in which the switch is located.


%%% Cause    ::= OCTET STRING (SIZE (minCauseLength..
%%% 							 maxCauseLength))

-type 'Cause'() ::
        undefined |
        normal |                      % (0),
        originationDenied |           % (1),
        collectDigits |               % (2),
        authorisationFailure |        % (3),
        bPtyAlerted |                 % (4),
        noIndication |                %	(5),
        aPtyAbandon |                 % (6),
        invalidCollectedInformation | % (7),
        collectInformationFailure |   % (8),
        aPtyDisc |                    % (9),
        bPtyDisc |                    % (10),
        routeSelectFailure |          % (11),
        oNoAnswer |                   % (12),
        terminationDenied |           % (13),
        notReachable |                % (14),
        bPtyNoAnswer |                % (15),
        bPtyBusyUDUB |                % (16),
        bPtyBusyNDUB |                % (17),
        ss7Failure |                  % (18),
        calledPartyRejected |         % (19),
        tException |                  % (20),
        routeFailure1 |               % (21),
        routeFailure2 |               % (22)
        ssActivating |
        conversationTimeout |
        noCircuitAvailable |
        coreNotification |
        unsupportedMedia |
        numberIncomplete |
        invalidNumber |
        unassignedNumber |
        doNotDisturb |
        externalControlled |
%        heartbeat_timeout |
%        killed |
%        session_timeout |
        systemFailure |                % our specific, Alpha's added
        applicationNotification |
        refer_ok |
        refer_failure |
        destinationOutOfOrder |        %% refs #65586
        uaPreemption |                 % RFC 4411 Reason: preemption ;cause=1 ;text="UA Preemption"
        reservedResourcesPreemted |    % RFC 4411 Reason: Preemption :cause=2 ;text="Reserved Resources Preempted"
        genericPreemption |            % RFC 4411 Reason: preemption ;cause=3 ;text="Generic Preemption"
        nonIpPreemption.               % RFC 4411 Reason: preemption ;cause=4 ;text="Non-IP Preemption"


-type 'CauseInitiator'() :: user | isup_network | non_isup_network | system | undefined. % cause initiation point - internal system or external network

-type 'CauseIsup'() :: term(). %ISUP Cause code

-type 'CauseLocation'() ::
        user | % user (U)
        lpn  | % private network serving the local user (LPN)
        ln   | % public network serving the local user (LN)
        tn   | % transit network (TN)
        rln  | % public network serving the remote user (RLN)
        rpn  | % private nerwork serving the remote user (RPN)
        intl | % international network
        bi   | % network beyond interworking point
        ssw  | % softswitch internal initiated
        undefined.

-type 'ExtCause'() ::
        isup_1_unallocated_number |      % unallocated (unassigned) number
        isup_2_no_route |                % no route to specified transit network
        isup_3_no_route |                % no route to destination
        isup_4_send_tone |               % send special information tone
        isup_5_misdialled_trunk_prefix | % misdialled trunk prefix
%        isup_6_channel_unacceptable |    % DSS1 channel unacceptable
%        isup_7_call_awarded |            % DSS1 call awarded and being delivered in an established channelNumber
        isup_8_preemption |              % preemption
        isup_9_preemption_reserved |     % preemption circuit reserved for reuse
        isup_16_normal |                 % normal call clearing
        isup_17_busy |                   % user busy
        isup_18_no_response |            % no user responding
        isup_19_no_answer |              % no answer from user (user alerted)
        isup_20_subscriber_absent |      % subscriber absent
        isup_21_call_rejected |          % call rejected
        isup_22_number_changed |         % number changed
        isup_23_redirection |            % redirection to new destination
        isup_25_routing_error |          % exchange routing error
%        isup_26_user_clearing |          % DSS1 non selected user clearing
        isup_27_dst_out_of_order |       % destination out of order
        isup_28_invalid_number_format |  % invalid number format (address incomplete)
        isup_29_facility_rejected |      % facility rejected
%        isup_30_resp_status_enquiry |    % DSS1 response to STATUS ENQUIRY
        isup_31_normal_unspecified |     % normal, unspecified
        isup_34_no_circuit |             % no circuit/channel available
        isup_38_network_out_of_order |   % network out of order
%        isup_39_frame_mode_out_of_serv | % DSS1 permanent frame mode connection out of service
%        isup_40_frame_mode_operational | % DSS1 permanent frame mode connection operational
        isup_41_temporary_failure |      % temporary failure
        isup_42_equipment_congestion |   % switching equipment congestion
        isup_43_access_info_discarded |  % access information discarded
        isup_44_circuit_not_available |  % requested circuit/channel not available
        isup_46_precedence_call_blocked | % precedence call blocked
        isup_47_resource_unavailable |   % resource available, unspecified
        isup_49_qos_not_available |      % quality of service not available
        isup_50_facility_not_supported | % requested facility not supported
        isup_53_ocalls_barred_cug |      % outgoing calls barred within CUG
        isup_55_icalls_barred_cug |      % incoming calls barred within CUG
        isup_57_bc_not_authorized |      % bearer capability not authorized
        isup_58_bc_not_available |       % bearer capability not presently available
        isup_62_inconsitency |           % inconsistency in designated outgoing access information and subscriber class
        isup_63_service_not_available |  % service or option not available, unspecified
        isup_65_bc_not_implemented |     % bearer capability not implemented
%        isup_66_channel_type_not_implemented | % DSS1 channel type not implemented
        isup_69_facility_not_implemented | % requested facility not implemented
        isup_70_only_restricted_bc |     % only restricted digital information bearer capability is available
        isup_79_service_not_implemented | % service or option not implemented, unspecified
%        isup_81_invalid_call_reference | % DSS1 invalid call reference
%        isup_82_channel_not_exist |      % DSS1 identified channel does not exist
%        isup_83_suspended_call_exist |   % DSS1 a suspended call exist, but this call identity does not
%        isup_84_call_identity_in_use |   % DSS1 call identity in use
%        isup_85_no_call_suspended |      % DSS1 no call suspended
%        isup_86_call_cleared |           % DSS1 call having the requested call identity has been cleared
        isup_87_not_memeber_cug |        % user not member of CUG
        isup_88_incompatible_destination | % incompatible destination
        isup_90_non_existent_cug |        % non-existent CUG
        isup_91_invalid_transit_network | % invalid transit network selection
        isup_95_invalid_message |         % invalid message, unspecified
%        isup_96_mandatory_missing |       % DSS1 mandatory message element is missing
        isup_97_mt_not_implemented |      % message type non-existent or not implemented
%        isup_98_message_not_compatible |  % DSS1 message not compatible with call state or message type non-existent or not implemented
        isup_99_element_non_existent |    % information element/parameter non-existent or not implemented
%        isup_100_invalid_contents |       % DSS1 invalid information element contents
%        isup_101_message_not_compatible | % DSS1 message not compatible with call state
        isup_102_recovery_on_timer |      % recovery on timer expiry
        isup_103_parameter_non_existent | % parameter non-existent or not implemented, passed on
        isup_110_message_discarded |      % message with unrecognized parameter, discarded
        isup_111_protocol_error |         % protocol error, unspecified
        isup_127_interworking |           % interworking, unspecified
        undefined.

-record('ISUPCause',
        {
          value :: term()
        }).

-type 'ISUPCause'() :: #'ISUPCause'{}.

-record('SIPCause',
        {
          value :: term()
        }).

-type 'SIPCause'() :: #'SIPCause'{}.

-record('MegacoCause',
        {
          value :: term()
        }).

-type 'MegacoCause'() :: #'MegacoCause'{}.

-type 'OriginalCause'() :: 'ISUPCause'() |
                           'SIPCause'() |
                           'MegacoCause'().

-record('ExtendedCause',
        {
          location   :: 'CauseLocation'(),
          cause      :: 'ExtCause'(),
          diagnostic :: term(),
          originalCause :: undefined | 'OriginalCause'()
        }).

-type 'ExtendedCause'() :: #'ExtendedCause'{}.

%%% Indicates the cause for interface related information. Refer to the Q.763 Cause  parameter for encoding
%%% For the use of cause and location values refer to Q.850.

-type 'CCSS'() :: boolean().
%%% Used by the SSF to indicate CCSS (Call Completion on Service Set-up) if set to "True"  to the SCF,
%%% i.e. that the current call is due a special procedure  (CCBS or CCNR).

-type 'CGEncountered'() :: noCGencountered |     % (0),
                           manualCGencountered | % (1),
                           sCPOverload.          % (2)
%%% Indicates the type of automatic call gapping encountered, if any.

-type 'ChargeNumber'() :: list(). %OCTET STRING %% 'LocationNumber'().
%%% Information sent in either direction indicating the chargeable number for the call and consisting
%%% of the odd/even indicator, nature of address indicator, numbering plan indicator, and address signals.
%%% Uses the LocationNumber format which is based on the Q.763 Location Number format
%%% For example, the ChargeNumber may be a third party number to which a call is billed for the 3rd party billing
%%% service. In this case, the calling party may request operator assistance to charge the call to,
%%% for example, their home number.

-record('ChargingEvent',
        {
          eventTypeCharging :: undefined | 'EventTypeCharging'() ,
          monitorMode       :: undefined | 'MonitorMode'(),
          legID             :: undefined | 'DTLegID'()
        }).
-type 'ChargingEvent'() :: #'ChargingEvent'{}.
%%% This parameter indicates the charging event  type and corresponding monitor mode and LedID

%% CollectInformation   ::= SEQUENCE {
%%   numberingPlan            [0] CollectNumberingPlan OPTIONAL,
%%   ...
%% }

%% CollectNumberingPlan  ::= SEQUENCE {
%%   name            [0] OCTET STRING OPTIONAL,
%%   body            [1] OCTET STRING OPTIONAL,
%%   ...											  -- Timers etc...
%% }

-type 'Component'() :: {componentInfo, list()} | %OCTET STRING (SIZE(1..118)),
%%% Contains the operation value (object identifier), error value, etc. within the UNI APDU, in addition also contain
%%% the parameter set/sequence for the operation invocation/return result ot return error/reject on UNI. See Q.932
%%% for encoding
                       {relayedComponent, list()}.%OCTET STRING
%%% If componentInfo is chosen, then it is necessary to use this parameter in sequence with ComponentType and
%%% ComponentCorrelationID
%%% If relayedComponent is chosen, then ComponentType and ComponentCorrelationID may not be used in the
%%% sequence

-type 'ComponentCorrelationID'() :: integer().

-type 'ComponentType'() :: ann |     % (0),
                           invoke |  % (1),
                           rResult | % (2),
                           rError |  % (3),
                           rReject.  % (4)

-type 'ConnectedNumberTreatmentInd'() :: noINImpact |                     %	(0),
                                         presentationRestricted |         % (1),
                                         presentCalledINNumber |          % (2),
                                         presentCalledINNumberRestricted. %	(3)

-type 'ControlType'() :: sCPOverloaded |      % (0),
                         manuallyInitiated |  % (1),
                         destinationOverload. % (2)
%%% other values are outside the scope of this capability set.

-type 'CorrelationID'() :: 'Digits'().
%%% used by SCF for correlation with a previous operation. Refer to clause 4 for a description of the procedures
%%% associated with this parameter.
-record('CounterAndValue',
        {
          counterID	   :: undefined | 'CounterID'(),
          counterValue :: undefined | 'Integer4'()
		}).
-type 'CounterAndValue'() :: #'CounterAndValue'{}.

-type 'CounterID'() :: integer(). %INTEGER (0..9)
%%% Indicates the counters to be incremented The counterIDs can be addressed by using the last digits of the dialed number.

-type 'CountersValue'() :: list('CounterAndValue'()). %SEQUENCE SIZE(0..numOfCounters) OF CounterAndValue

-type 'CreateOrRemoveIndicator'() :: crreate | % (0),
                                     remove.   % (1)

-type 'CSAID'() :: integer(). %INTEGER (1..numOfCSAs)
%%% Indicates the SSF CSA identifier

-type 'CutAndPaste'() :: integer(). % INTEGER (0..2)
%%% Indicates the number of leading digits to be deleted (cut) and to paste remaining dialed digits.

-type 'DateAndTime'() :: list(). %OCTET STRING (SIZE(6))
%%% Indicates, amongst others, the start time for activate service filtering. Coded as YYMMDDHHMMSS
%%% with each digit coded BCD
%%% The first octet contains YY and the remaining items are sequenced following
%%% For example, 1993 September 30th, 12:15:01 would be encoded as:
%%% Bits 			HGFE		DCBA
%%% leading octet	3	9
%%%                 9	0
%%%                 0	3
%%%                 2	1
%%%                 5	1
%%%                 1	0
%%% For a system operating when or after this Recommendation is released, the 2 digit value
%%% representing a Year shall be interpreted as follows
%%% If the two-digits value is 00 through 49 inclusive, it shall be interpreted as representing
%%% year 2000  through 2004.
%%% If the two-digits value is 50 through 99 inclusive, it shall be interpreted as representng
%%% year 1950 through 1999.

-record('DefaultFaultHandling',
        {
          action    :: resumeCallProcessing | % (0),
                       releaseCall          | % (1),
                       undefined,

          treatment	:: undefined | 'GapTreatment'()
        }).
-type 'DefaultFaultHandling'() :: #'DefaultFaultHandling'{}.

-type 'DestinationRoutingAddress'() :: list('CalledPartyNumber'()).
                                                %	SEQUENCE SIZE(1..numOfAddresses) OF CalledPartyNumber
%%% Indicates the list of Called Party Numbers (primary and alternates).

-type 'Digits'() :: list(integer()). %OCTET STRING (SIZE (minDigitsLength..maxDigitsLength))
%%% Indicates the address signalling digits. Refer to the Q.763 Generic Number and Generic Digits parameter
%%% for encoding. The coding of the subfield's 'NumberQualifier' in Generic Number and 'TypeOfDigits' in
%%% Generic Digits are irrelevant to the INAP, the ASN.1 tags are sufficient to identify the parameter.
%%% The ISUP format does not allow to exclude these subfields, therefore the value is network operator specific.
%%% The following parameters should use Generic Number
%%% Additional Calling Number, CorrelationID for AssistRequestInstructions, AssistingSSPIPRoutingAddress
%%% for EstablishTemporaryConnection
%%% calledAddressValue for all occurrences,callingAddressValue for all occurrences
%%% The following parameters should use Generic Digits: prefix, all
%%% other CorrelationID occurrences, dialledNumber filtering criteria, callingLineID filtering criteria, lineID for
%%% ResourceID type, digitResponse for ReceivedInformationArg, iNServiceControlLow / iNServiceControlHigh for
%%% MidCallInfoType,, iNServiceControlCode for MidCallInfo.

-type 'DisplayInformation'() :: list().% IA5String (SIZE (minDisplayInformationLength..	maxDisplayInformationLength))
%%% Indicates the display information
%%% Delivery of DisplayInformation parameter to Private Networks cannot be guaranteed due to signalling
%%% interworking problems, solutions are outside the scope of this capability set.

-record('DpSpecificCommonParameters',
        {
          serviceAddressInformation         :: undefined | 'ServiceAddressInformation'(),
          bearerCapability                  :: undefined | 'BearerCapability'(),
          calledPartyNumber                 :: undefined | 'CalledPartyNumber'(),
          callingPartyNumber                :: undefined | 'CallingPartyNumber'(),
          callingPartysCategory             :: undefined | 'CallingPartysCategory'(),
          iPSSPCapabilities                 :: undefined | 'IPSSPCapabilities'(),
          iPAvailable                       :: undefined | 'IPAvailable'(),

          iSDNAccessRelatedInformation      :: undefined | 'ISDNAccessRelatedInformation'(),
          cGEncountered                     :: undefined | 'CGEncountered'(),
          locationNumber                    :: undefined | 'LocationNumber'(),
          serviceProfileIdentifier          :: undefined | 'ServiceProfileIdentifier'(),
          terminalType                      :: undefined | 'TerminalType'(),
          extensions                        :: undefined | 'Extensions'(),
          chargeNumber                      :: undefined | 'ChargeNumber'(),
          servingAreaID                     :: undefined | 'ServingAreaID'(),
          serviceInteractionIndicators      :: undefined | 'ServiceInteractionIndicators'(),
          iNServiceCompatibilityIndication  :: undefined | 'INServiceCompatibilityIndication'(),
          serviceInteractionIndicatorsTwo   :: undefined | 'ServiceInteractionIndicatorsTwo'(),
          uSIServiceIndicator               :: undefined | 'USIServiceIndicator'(),
          uSIInformation                    :: undefined | 'USIInformation'(),
          forwardGVNS                       :: undefined | 'ForwardGVNS'(),
          createdCallSegmentAssociation     :: undefined | 'CSAID'()
        }).
-type 'DpSpecificCommonParameters'() :: #'DpSpecificCommonParameters'{}.

%%% OPTIONAL for iPSSPCapabilities, iPAvailable, and cGEncountered denotes network operator specific use
%%% OPTIONAL for callingPartyNumber, and callingPartysCategory
%%% . bearerCapability should be appropriately coded as speech.

-type 'DpSpecificCriteria'() ::
        {numberOfDigits, 'NumberOfDigits'()} |
        {applicationTimer, 'ApplicationTimer'()} |
		{midCallControlInfo, 'MidCallControlInfo'()} |
		{numberOfDigitsTwo, RequestedNumberOfDigits::'NumberOfDigits'(), MinNumberOfDigits::undefined | 'NumberOfDigits'()}.

%%% The SCF may specify the number of digits to be collected by the SSF for the CollectedInfo event
%%% When all digits are collected, the SSF reports the event to the SCF
%%% The SCF may set a timer in the SSF for the No Answer event. If the user does not answer the call
%%% within the allotted time, the SSF reports the event to the SCF
%%% The SCF may specify the number of digits to be collected by the SSF for the
%%% CollecteInfo event and hereby specify a minimum number of digits to be collected in case
%%% the exact number of digits is unknown to the SCF, but a  report is desired in case of complete number
%%% is determined before the requested number of digits has been collected.

-type 'XDuration'() :: integer(). % INTEGER (-2..86400)
%%% Values are seconds

-type 'Entry'() ::
        {agreements, 'OBJECT IDENTIFIER'()} |
        {networkSpecific, 'Integer4'()}.

-type 'EventSpecificInformationBCSM'() ::
        {collectedInfoSpecificInfo, CalledPartynumber :: 'CalledPartyNumber'()} |
        {analysedInfoSpecificInfo, CalledPartynumber :: 'CalledPartyNumber'()} |
        {routeSelectFailureSpecificInfo, FailureCause :: undefined | 'Cause'()} |
        {oCalledPartyBusySpecificInfo, BusyCause :: undefined | 'Cause'()} |
        oNoAnswerSpecificInfo |
        {oAnswerSpecificInfo, BackwardGVNS :: undefined | 'BackwardGVNS'()} |
        {oMidCallSpecificInfo, ConnectTime :: undefined | 'Integer4'(),  OMidCallInfo :: undefined | 'MidCallInfo'()} |
        {oDisconnectSpecificInfo, ReleaseCause :: undefined | 'Cause'(),   ConnectTime :: undefined | 'Integer4'()} |
        {tBusySpecificInfo, BusyCause :: undefined | 'Cause'()} |
        tNoAnswerSpecificInfo |
        tAnswerSpecificInfo |
        {tMidCallSpecificInfo, ConnectTime :: undefined | 'Integer4'(), TMidCallInfo :: undefined | 'MidCallInfo'()} |
        {tDisconnectSpecificInfo, ReleaseCause :: undefined | 'Cause'(), ConnectTime :: undefined | 'Integer4'()} |
        oTermSeizedSpecificInfo |
        oSuspend |
        tSuspend |
        origAttemptAuthorized |
        oReAnswer |
        tReAnswer |
        facilitySelectedAndAvailable |
        callAccepted |
        {oAbandon, AbandonCause :: undefined | 'Cause'()} |
        {tAbandon, AbandonCause :: undefined | 'Cause'()} |
        {authorizeRouteFailure, AuthoriseRouteFailureCause :: undefined | 'Cause'()} |
        terminationAttemptAuthorized |
        {orininationDenied, OriginationDeniedCause :: undefined | 'Cause'()} |
        {terminationDenied, TerminationDeniedCause :: undefined | 'Cause'()}.
%%% Indicates the call related information specific to the event.
%%% The unit for the connectTime is 100 milliseconds

-type 'EventSpecificInformationCharging'() :: list().%   ::= OCTET STRING (SIZE
                                                %							(minEventSpecificInformationChargingLength..
                                                %			  				maxEventSpecificInformationChargingLength))
%%% defined by network operator.
%%% Its content is network signalling/operator specific.


%%% Indicates the charging related information specific to the event.
%%% An example data EmbeddedType definition for this parameter is given below:
%%% 	chargePulses		[0] Integer4,
%%% 	chargeMessages		[1] OCTET STRING (SIZE (min..max))

-type 'EventTypeBCSM'() :: origAttemptAuthorized |        % (1),
                           collectedInfo |                % (2),
                           analysedInformation |          % (3),
                           routeSelectFailure |           % (4),
                           oCalledPartyBusy |             % (5),
                           oNoAnswer |                    % (6),
                           oAnswer |                      % (7),
                           oMidCall |                     % (8),
                           oDisconnect |                  % (9),
                           oAbandon |                     % (10),
                           termAttemptAuthorized |        % (12),
                           tBusy |                        % (13),
                           tNoAnswer |                    % (14),
                           tAnswer |                      % (15),
                           tMidCall |                     % (16),
                           tDisconnect |                  % (17),
                           tAbandon |                     % (18),
                           oTermSeized |                  % (19),
                           oSuspend |                     % (20),
                           tSuspend |                     % (21),
                           origAttempt |                  % (22),
                           termAttempt |                  % (23),
                           oReAnswer |                    % (24),
                           tReAnswer |                    % (25),
                           facilitySelectedAndAvailable | % (26),
                           callAccepted |                 % (27),
                           authorizeRouteFailure |        % (28),
                           originationDenied |            % (29),
                           terminationDenied.             % (30)

%%% Indicates the name of the BCSM detection point event.
%%% Notice: Values origAttempt and termAttempt can only be used for TDPs

-type 'EventTypeCharging'() :: list().%   ::= OCTET STRING (SIZE
                                                %(minEventTypeChargingLength..
                                                % maxEventTypeChargingLength))
%%% This parameter indicates the charging event type. .
%%% Its content is network signalling / operator specific.

%%% An example data type definition for this parameter is given below:
%%% EventTypeCharging EmbeddedType ::= ENUMERATED {
%%% 			chargePulses (0),
%%% 			chargeMessages (1)
%%% 			}

-type 'FacilityGroup'() :: {trunkGroupID, integer()} |
                           {privateFacilityID, integer()} |
                           {huntGroup, list()} |	        %OCTET STRING,
                           {routeIndex, list()}.            %OCTET STRING
%%% Indicates the particular group of facilities to route the call. huntGroup and routeIndex are encoded as
%%% network operator specific.



-type 'FacilityGroupMember'() :: integer().
%%% Indicates the specific member of a trunk group or multi-line hunt group.

-type 'FacilityType'() :: list(integer()).
%%% Used to passthrough Q.763 facility message by CallProgress ACP message

-type 'FCIBillingChargingCharacteristics'() :: list(). %OCTET STRING (SIZE
                                                %(minFCIBillingChargingLength..
                                                %maxFCIBillingChargingLength))
%%% This parameter indicates the billing and/or charging characteristics.
%%% Its content is network operator specific

%%% An example datatype definition for this parameter is given below:
%%% FCIBillingChargingCharacteristicsEmbeddedType ::= CHOICE {
%%% completeChargingrecord			[0] OCTET STRING (SIZE (min..max)),
%%% correlationID				[1] CorrelationID,
%%% scenario2Dot3				[2] SEQUENCE {
%%% 					chargeParty	[0] LegID 	OPTIONAL,
%%% 					chargeLevel	[1] OCTET STRING (SIZE (min..max)) OPTIONAL,
%%% 					chargeItems	[2] SET OF Attribute	OPTIONAL
%%% 						}
%%% }
%%% Depending on the applied charging scenario the following information elements can be included
%%% (refer to Q.1214 Appendix II):
%%% complete charging record (scenario 2.2)
%%% charge party (scenario 2.3)
%%% charge level (scenario 2.3)
%%% charge items (scenario 2.3)
%%% correlationID (scenario 2.4)

-type 'FeatureCode'() :: list(). %OCTET STRING %% 'LocationNumber'().
%%% The two-digit feature code preceded by "*" or "11".
%%% Uses the LocationNumber format which is based on the Q.763 Location Number format.
%%% The Nature of Address indicator field shall be set to "Spare" (value 00000000).
%%% The Numbering Plan Indicator field shall be set to "Spare" (value 000)
%%% Used for stimulus signalling (Q.932).

-type 'FeatureRequestIndicator'() :: hold |              % (0),
                                     retrieve |          % (1),
                                     featureActivation | % (2),
                                     spare1 |            % (3),
                                     sparen.             % (127)
%%% Indicates the feature activated (e.g. a switch-hook flash, feature activation). Spare values reserved
%%% for future use.

-record('FilteredCallTreatment',
        {
          sFBillingChargingCharacteristics :: undefined | 'SFBillingChargingCharacteristics'() ,
          informationToSend                :: undefined | 'InformationToSend'(),
          maximumNumberOfCounters          :: undefined | 'MaximumNumberOfCounters'(),
          releaseCause                     :: undefined | 'Cause'()
        }).
-type 'FilteredCallTreatment'() :: #'FilteredCallTreatment'{}.
%%% If releaseCause is not present, the default value is the same as the ISUP cause value decimal 31.
%%% If informationToSend is present, the call will be released after the end of the announcement
%%% with the indicated or default releaseCause.
%%% If maximumNumberOfCounters is not present, ServiceFilteringResponse will be sent with
%%% CountersValue::= SEQUENCE SIZE (0) OF CountersAndValue

-type 'FilteringCharacteristics'() :: {interval, integer()} | %INTEGER (-1..32000),
                                      {numberOfCalls, 'Integer4'()}.
%%% Indicates the severity of the filtering and the point in time when the ServiceFilteringResponse is to be sent.
%%% If = interval, every interval of time the next call leads to an InitialDP and a ServiceFilteringResponse is sent to
%%% the SCF.
%%% An interval of 0 indicates that all calls matching the filtering criteria will result in sending of an "InitialDP" or a DP-specific
%%% operation and no filtering will be applied (i.e. no "ServiceFilteringResponse will be sent).
%%% An interval of -1 indicates that none of the calls matching the filtering criteria will either result in sending of
%%% an "InitialDP"  or a DP-specific operation or a "ServiceFilteringResponse" operation.
%%% Other values of Interval indicate duration in seconds.

%%% If = NumberOfCalls, every N calls the Nth call leads to an InitialDP and a ServiceFilteringResponse
%%% is sent to the SCF.
%%% If ActivateServiceFiltering implies several counters - filtering on several dialled numbers -,
%%% the numberOfCalls would include calls to all the dialled numbers.

-type 'FilteringCriteria'() ::
        {dialledNumber, 'Digits'()} |
        {callingLineID, 'Digits'()} |
        {serviceKey, 'ServiceKey'()} |
        {addressAndService,
         CalledAddressValue :: 'Digits'(),
         ServiceKey :: 'ServiceKey'(),
         CallingAddressValue :: undefined | 'Digits'(),
         LocationNumber :: undefined | 'LocationNumber'()}.
%%% In case calledAddressValue is specified, the numbers to be filtered are from calledAddressValue
%%% up to and including calledAddressValue + maximumNumberOfCounters-1.
%%% The last two digits of calledAddressvalue can not exceed 100-maximumNumberOfCounters.

-type 'FilteringTimeOut'() :: {duration, 'XDuration'()} |
                              {stopTime, 'DateAndTime'()}.
%%% Indicates the maximum duration of the filtering. When the timer expires, a ServiceFilteringResponse
%%% is sent to the SCF.

-type 'ForwardCallIndicators'() :: list(). %OCTET STRING (SIZE(2))
%%% Indicates the Forward Call Indicators. Refer to Q.763 for encoding

-type 'ForwardGVNS'() :: list(). %OCTET STRING (SIZE(
                                                %minForwardGVNSLength..
                                                %maxForwardGVNSLength))
%%% Indicates the GVNS Forward information. Refer to Q.735, §6 for encoding.

-type 'ForwardingCondition'() :: busy |     % (0),
                                 noanswer | % (1),
                                 any.       % (2)

%%% Indicates the condition that must be met to complete the connect.

-record('ForwardServiceInteractionInd',
        {
          conferenceTreatmentIndicator = acceptConferenceRequest ::
            undefined |
            acceptConferenceRequest |
            rejectConferenceRequest,
                                                %-- [1] OCTET STRING (SIZE(1))	OPTIONAL,
                                                %-- acceptConferenceRequest	'xxxx xx01',B
                                                %-- rejectConferenceRequest	'xxxx xx10'B
                                                %-- network default is accept conference request
          callDiversionTreatmentIndicator = callDiversionAllowed ::
            undefined |
            callDiversionAllowed |
            callDiversionNotAllowed,
                                                %-- [2] OCTET STRING (SIZE(1))	OPTIONAL,
                                                %-- callDiversionAllowed	'xxxx xx01'B
                                                %-- callDiversionNotAllowed	'xxxx xx10'B
                                                %-- network default is Call Diversion allowed
          callOfferingTreatmentIndicator = callOfferingNotAllowed ::
            callOfferingNotAllowed |
            callOfferingAllowed |
            callOfferingNoINImpact,
%%% indicates if call offering is "allowed"," not allowed" or  "no impact by IN"
%%% the value 'no impact by IN," has only local significance in SSF as a request to SSF
%%% not to modify the value of the call offering treatment indicator conveyed in signaling.
                                                %-- [3] OCTET STRING (SIZE(1))	OPTIONAL ,
                                                %-- callOfferingNotAllowed		'xxxx xx01'B,
                                                %-- callOfferingAllowed		'xxxx xx10'B
                                                %-- callOfferingNoINImpact		'xxxx x100'B
                                                %--indicates if call offering is "allowed"," not allowed" or  "no impact by IN".
                                                %-- network default is Call Offering not allowed
          callWaitingTreatmentIndicator = callWaitingAllowed ::
            callWaitingAllowed |
            callWaitingNotAllowed
                                                %-- [5] OCTET STRING (SIZE(1))	OPTIONAL
                                                %-- callWaitingAllowed		'xxxx xx01'B,
                                                %-- callWaitingNotAllowed	'xxxx xx10'B
                                                %-- network default is Call Waiting allowed
        }).
-type 'ForwardServiceInteractionInd'() :: #'ForwardServiceInteractionInd'{}.

%%% Notice  Tag value 4 is otherwise reserved ,

%%% The forwardServiceInteractionInd parameter is applicable to IDP, CON, CWA and ICA operations.
%%% This parameter can be received either in the O_BCSM or in the T_BCSM.

-type 'GapCriteria'() :: {calledAddressValue, 'Digits'()} |
                         {gapOnService, 'GapOnService'()} |
                         gapAllInTraffic |
                         {calledAddressAndService, CalledAddressValue :: 'Digits'(), ServiceKey :: 'ServiceKey'()} |
                         {callingAddressAndService,
                          CallingAddressValue :: 'Digits'(),
                          ServiceKey :: 'ServiceKey'(),
                          LocationNumber :: undefined | 'LocationNumber'()}.
%%% Both calledAddressValue and callingAddressValue can be
%%% incomplete numbers, in the sense that a limited amount of digits can be given.

%%% For the handling of numbers starting with the same digit string refer to the detailed procedure
%%% of the CallGap operation in 3.3

-record('GapOnService',
        {
          serviceKey :: undefined | 'ServiceKey'(),
          dpCriteria :: undefined | 'EventTypeBCSM'()
        }).
-type 'GapOnService'() :: #'GapOnService'{}.

-record('GapIndicators',
        {
          duration    :: undefined | 'XDuration'(),
          gapInterval :: undefined | 'Interval'()
        }).
-type 'GapIndicators'() :: #'GapIndicators'{}.

%%% Indicates the gapping characteristics.
%%% No gapping when gapInterval equals 0, and
%%% gap all calls when gapInterval equals -1.

-type 'GapTreatment'() :: {informationToSend, 'InformationToSend'()} |
                          {releaseCause, 'Cause'()} |
                          {both, InformationToSend :: 'InformationToSend'(), ReleaseCause :: 'Cause'()}.
%%% The default value for Cause is the same as in ISUP.

-type 'GenericName'() :: list(). %OCTET STRING (SIZE(
                                                %minGenericNameLength..
                                                %maxGenericNameLength))
%%% Refer to Q.931 Display Information for encoding.

-type 'GenericNumber-ISUP'() :: {isup, GenericNumberRecord :: term()}. %% GenericNumberRecord defined in isup_encoder application. Real format isn't actual here, usefull for SIP, SIGTRAN etc. adapters only.
-type 'GenericNumber'() :: list(integer()). % OCTET STRING (SIZE(
                                                %minGenericNumberLength..
                                                %maxGenericNumberLength))
%%% Refer to Q.763 Generic Number for encoding.

-type 'GenericNumbers'() :: list('GenericNumber'()) | 'GenericNumber-ISUP'(). % SET SIZE(1..numOfGenericNumbers) OF GenericNumber

-type 'HighLayerCompatibility'() :: list(). % OCTET STRING (SIZE (highLayerCompatibilityLength))
%%% Indicates the teleservice. For encoding, DSS1 (Q.931) is used.

-type 'HoldCause'() :: list(). % OCTET STRING
%%% defined by network operator.
%%% Indicates the cause for holding the call.
%%% Its content is network operator specific

-define(initialCallSegment, 1). % INTEGER ::= 1
%%% the initial call segment represents the call segment that was there when the CSA was created, ie. the CS where
%%% the trigger took place or the CS that was created by an InitateCallAttempt within a TC-BEGIN message.

-record('INprofile',
        {
          actionOnProfile :: undefined | 'ActionOnProfile'(),
          tDPIdentifier   :: undefined | 'TDPIdentifier'(),
          dpName          :: undefined | 'EventTypeBCSM'(),
          extensions      :: undefined | 'Extensions'()
        }).
-type 'INprofile'() :: #'INprofile'{}.

-type 'INServiceCompatibilityIndication'() :: list('Entry'()). %SEQUENCE SIZE (1..numOfInServiceCompatibilityIndLength) OF Entry

-type 'INServiceCompatibilityResponse'() :: 'Entry'().


-type 'Interval'() :: integer(). % INTEGER (-1..60000)
%%% Units are milliseconds. A -1 value denotes infinite.


-type 'IPAvailable'() :: list(). % OCTET STRING (SIZE (
                                                %minIPAvailableLength..
                                                %maxIPAvailableLength))
%%% defined by network operator.
%%% Indicates that the resource is available.
%%% Its content is network operator specific


-type 'IPRoutingAddress'() :: 'CalledPartyNumber'().
%%% Indicates the routing address for the IP.

-type 'IPSSPCapabilities'() :: list(). %OCTET STRING (SIZE (
                                                %minIPSSPCapabilitiesLength..
                                                %maxIPSSPCapabilitiesLength))
%%% defined by network operator.
%%% Indicates the SRF resources available at the SSP.
%%% Its content is network operator specific

-type 'ISDNAccessRelatedInformation'() :: list(). % OCTET STRING (SIZE
                                                %(minISDNAccessRelatedInfoLength..
                                                %maxISDNAccessRelatedInfoLength))
%%% Indicates the destination user network interface related information. Refer to the Q.763 Access
%%% Transport parameter for encoding.

-type 'DTLegID'() :: {sendingSideID, 'DTLegType'()} |
                     {receivingSideID, 'DTLegType'()}.
%%% Indicates a reference to a specific party in a call. OPTIONAL denotes network operator specific use
%%% with a choice of unilateral ID assignment or bilateral ID assignment.
%%% OPTIONAL for LegID also denotes the following:
%%% when only one party exists in the call, this parameter is not needed (as no ambiguity exists);
%%% when more than one party exists in the call, one of the following alternatives applies:
%%%  1. LegID is present and indicates which party is concerned.
%%%  2. LegID is not present and a default value is assumed (e.g. calling party in the case of the
%%%     ApplyCharging operation).
%%% Choice between these two alternatives is kept a network operator option.
%%% sendingSideID is used where legID is sent from the SCF  to the SSF and
%%% receivingSideID is used  where SCF receives legID from the SSF.

-type 'DTLegType'() :: integer(). %OCTET STRING (SIZE(1))

-define(dtleg1,  1).
-define(dtleg2, 2).

-record('LocationNumber',
        {
          nai = subscriberNumber      :: 'NAIType'(),       % Nature of address indicator
          ni = private                :: 'NIType'(),        % Number Indicator
          incomplete = false          :: boolean(),         % number incomplete indicator (flase - complete, true - incomplete)
                                                % - not by specs
          npi = isdnTelephony         :: 'NPIType'(),       % Numbering plan indicator
          apri = presentationAllowed  :: 'APRIType'(),      % Address presentation restricted inficator
          screening = networkProvided :: 'ScreeningType'(), % Screening indicator
          category                    :: undefined | 'CallingPartysCategory'(),
          digits                      :: 'Digits'(),        % Number digits
          displayName                 :: undefined | string(),
          callerId                    :: undefined | 'Digits'(),
          sipUri                      :: undefined | string(),
          vdn                         :: undefined | 'Digits'() % Virtual direct number
         }).
-type 'LocationNumber'() :: #'LocationNumber'{}. %% list(). %OCTET STRING (SIZE (%minLocationNumberLength.. %maxLocationNumberLength))

%%% Indicates the Location Number for the calling party. Refer to Q.763 (White book) for encoding.

-type 'MaximumNumberOfCounters'() :: integer(). %INTEGER (1..numOfCounters)

-record('MidCallControlInfo',
        {
          midCallInfoType :: undefined | 'MidCallInfoType'(),
          midCallReportType = inMonitoringState :: inMonitoringState |
                                                   inAnyState
		}).
-type 'MidCallControlInfo'() :: list(#'MidCallControlInfo'{}).

-record('MidCallInfo',
        {
          iNServiceControlCode :: undefined | 'Digits'()
        }).
-type 'MidCallInfo'() :: #'MidCallInfo'{}.
-record('MidCallInfoType',
        {
          iNServiceControlCodeLow  :: undefined | 'Digits'(),
          iNServiceControlCodeHigh :: undefined | 'Digits'()
        }).
-type 'MidCallInfoType'() :: #'MidCallInfoType'{}.

-record('DTMiscCallInfo',
        {
          messageType :: undefined |
                         request |     %(0),
                         notification, %(1)
          dpAssignment :: undefined |
                          individualLine | % (0),
                          groupBased |     % (1),
                          switchBased      % (2)
        }).
-type 'DTMiscCallInfo'() :: #'DTMiscCallInfo'{}.
%%% Indicates detection point related information.
%%% Note: "Switch based "is used with the same meaning as the previous used term  "office based"

-type 'MonitorMode'() :: interrupted |       % (0),
                         notifyAndContinue | % (1),
                         transparent.        % (2)
%%% Indicates the event is relayed and/or processed by the SSP.


-type 'NumberingPlan'() :: 'NPIType'(). %OCTET STRING (SIZE(1))
%%% Indicates the numbering plan for collecting the user information. Refer to the Q.763 Numbering Plan
%%% Indicator field for encoding.

-type 'NumberOfDigits'() :: integer(). %INTEGER (1..255)
%%% Indicates the number of digits to be collected


-type 'OriginalCalledPartyID'() :: 'OriginalCalledNumber'().% OCTET STRING (SIZE
                                                %(minOriginalCalledPartyIDLength..
                                                %maxOriginalCalledPartyIDLength))
%%% Indicates the original called number. Both N-ISDN and B-ISDN E.164 addresses are supported Refer to the Q.2763 Original Called Number for encoding.

-record('OriginalCalledNumber',
        {
          nai         :: 'NAIType'(),       % Nature of address indicator
          ni          :: 'NIType'(),        % Number indicator
          incomplete  :: boolean(),         % number incomplete indicator (flase - complete, true - incomplete)
                                            % - not by specs
          npi         :: 'NPIType'(),       % Numbering plan indicator
          apri        :: 'APRIType'(),      % Address presentation restricted inficator
          category    :: undefined | 'CallPartyCategory'(),
          digits      :: 'Digits'(),        % Number digits
          displayName :: undefined | string(),
          sipUri      :: undefined | string(),
          vdn         :: undefined | 'Digits'() % Virtual direct number
        }).
-type 'OriginalCalledNumber'() :: #'OriginalCalledNumber'{}.

-type 'ProfileIdentifier'() :: {access, 'CalledPartyNumber'()} |
                               {group, 'FacilityGroup'()}.
%%% Please note that 'CalledPartyNumber' is used to address a subscriber access line.
%%% The data type was reused from the existing types to avoid the definition of a new one.

-type 'QoSParameter'() :: list(). %OCTET STRING (SIZE (
                                                %minQoSParameterLength..
                                                %maxQoSParameterLength))

%%% Indicates the QoS. Refer to Q.2961 for encoding.


-type 'Reason'() :: list(). %OCTET STRING (SIZE(
                                                %minReasonLength..
                                                %maxReasonLength))
%%% Its content is network operator specific


-type 'RedirectingPartyID'() :: 'RedirectingNumber'(). % OCTET STRING (SIZE (
                                                %minRedirectingPartyIDLength..
                                                %maxRedirectingPartyIDLength))
%%% Indicates redirecting number. Refer to the Q.763 Redirecting number for encoding.

-record('RedirectingNumber',
        {
          nai         :: 'NAIType'(),       % Nature of address indicator
          ni          :: 'NIType'(),        % Number indicator
          incomplete  :: boolean(),         % number incomplete indicator (flase - complete, true - incomplete)
                                            % - not by specs
          npi         :: 'NPIType'(),       % Numbering plan indicator
          apri        :: 'APRIType'(),      % Address presentation restricted inficator
          category    :: undefined | 'CallPartyCategory'(),
          digits      :: 'Digits'(),        % Number digits
          displayName :: undefined | string(),
          sipUri      :: undefined | string(),
          vdn         :: undefined | 'Digits'() % Virtual direct number
        }).
-type 'RedirectingNumber'() :: local_redirecting | #'RedirectingNumber'{}.

-record('RedirectionInformation',
        {
          originalRedirectionReason :: undefined | 'OriginalRedirectionReason',
          redirectingIndicator :: undefined | 'RedirectingIndicator' | diversion,
          redirectingReason :: undefined | 'RedirectingReason',
          redirectionCounter :: undefined | 0 | 1 | 2 | 3 | 4 | 5
        }).
-type 'RedirectionInformation'() :: #'RedirectionInformation'{}.
%%% Indicates redirection information. Refer to the Q.763 Redirection Information for encoding.

-type 'OriginalRedirectionReason'() ::
        unknown |      %0 - Unknown/not available
        busy |         %1 - user busy (national use)
        noReply |      %2 - no reply (national use)
        unconditional. %3 - unconditional (national use)

-type 'RedirectingIndicator'() ::
        noRedirection |          %0 - no redirection (national use)
        reRouted |               %1 - call re-routed (national use)
        reRoutedAllRestricted |  %2 - call re-routed, all redirection information presentation restricted (national use)
        diversion |              %3 - call diversion
        diversionAllRestricted | %4 - call diversion, all redirection information presentation restricted
        reRoutedRestricted |     %5 - call re-routed, redirection information presentation restricted (national use)
        diversionRestricted |    %6 - call diversion, redirection information presentation restricted
        spare.                   %7

-type 'RedirectingReason'() ::
        unknown |        %0 - unknown/not available
        busy |           %1 - user busy
        noReply |        %2 - no reply
        unconditional |  %3 - unconditional
        deflectAlert |   %4 - deflection during alerting
        deflectResp |    %5 - deflection immediate response
        notReachable |   %6 - mobile subscriber not reachable
        timeOfDay |      %  - time-of-day
        doNotDisturb |   %  - do-not-disturb
        followMe |       %  - follow-me
        outOfService |   %  - out-of-service
        away |           %  - away
        diversion.

-type 'RedirectReason'() :: list(). %OCTET STRING (SIZE(1))
%%% Indicates redirection reason information. Refer to the "Invoking Pivot Reason" parameter of  Q.763 for encoding.


-record('RedirectionNumber',
        {
          nai         :: 'NAIType'(),       % Nature of address indicator
          ni          :: 'NIType'(),        % Number indicator
          incomplete  :: boolean(),         % number incomplete indicator (flase - complete, true - incomplete)
                                            % - not by specs
          inni        :: 'INNIType'(),      % Internal network number indicator
          npi         :: 'NPIType'(),       % Numbering plan indicator
          category    :: undefined | 'CallPartyCategory'(),
          digits      :: 'Digits'(),        % Number digits
          displayName :: undefined | string(),
          sipUri      :: undefined | string(),
          vdn         :: undefined | 'Digits'() % Virtual direct number
        }).
-type 'RedirectionNumber'() :: #'RedirectionNumber'{}.

-type 'RedirectionRestrictionIndicator'() :: presentation_allowed | presentation_restricted.


-record('CallDiversionInformation',
        {
          notificationSubsOpts :: undefined | 'NotificationSubscriptionOptions'(),
          redirectingReason    :: undefined | 'RedirectingReason'()
        }).
-type 'CallDiversionInformation'() :: #'CallDiversionInformation'{}.

-type 'NotificationSubscriptionOptions'() :: unknown |
                                             presentation_not_allowed |
                                             presentation_allowed_with_redirecting_number |
                                             presentation_allowed_without_redirecting_number.

-record('EventInformation',
        {
          eventIndicator :: undefined | 'EventIndicator'(),
          eventPresentationIndicator :: undefined | 'EventPresentationRestrictedIndicator'()
        }).
-type 'EventInformation'() :: #'EventInformation'{}.

-type 'EventIndicator'() :: alerting |
                            progress |
                            in_band_info |
                            cfb |
                            cfnr |
                            cfu.

-type 'EventPresentationRestrictedIndicator'() :: no_indication | restricted.

-record('CallTransferNumber',
        {
          nai         :: 'NAIType'(),       % Nature of address indicator
          ni          :: 'NIType'(),        % Number indicator
          incomplete  :: boolean(),         % number incomplete indicator (flase - complete, true - incomplete)
                                            % - not by specs
          npi         :: 'NPIType'(),        % Numbering plan indicator
          apri        :: 'APRIType'(),      % Address presentation restricted inficator
          screening   :: 'ScreeningType'(), % Screening indicator
          category    :: undefined | 'CallPartyCategory'(),
          digits      :: 'Digits'(),         % Number digits
          displayName :: undefined | string(),
          sipUri      :: undefined | string(),
          vdn         :: undefined | 'Digits'() % Virtual direct number
        }).

-type 'CallTransferNumber'() :: #'CallTransferNumber'{}.

-type 'RegistratorIdentifier'() :: list(). % OCTET STRING
%%% Its content is network operator specific

-type 'ReportCondition'() :: statusReport | % (0),
                             timerExpired | % (1),
                             canceled.      % (2)
%%% ReportCondition specifies the cause of sending "StatusReport" operation to the SCF

-type 'RequestedInformationList'() :: list('RequestedInformation'()). %SEQUENCE SIZE (1..numOfInfoItems) OF RequestedInformation

-type 'RequestedInformationTypeList'() :: list('RequestedInformationType'()). %SEQUENCE SIZE (1..numOfInfoItems) OF RequestedInformationType

-record('RequestedInformation',
        {
          requestedInformationType  :: undefined | 'RequestedInformationType'(),
          requestedInformationValue :: undefined | 'RequestedInformationValue'()
        }).
-type 'RequestedInformation'() :: #'RequestedInformation'{}.

-type 'RequestedInformationType'() :: callAttemptElapsedTime |   % (0),
                                      callStopTime |             % (1),
                                      callConnectedElapsedTime | % (2),
                                      calledAddress |            % (3),
                                      releaseCause.              % (30)

-type 'RequestedInformationValue'() :: {callAttemptElapsedTimeValue, integer()} | %INTEGER (0..255),
                                       {callStopTimeValue, 'DateAndTime'()} |
                                       {callConnectedElapsedTimeValue, 'Integer4'()} |
                                       {calledAddressValue, 'Digits'()} |
                                       {releaseCauseValue, 'Cause'()}.

%%% The callAttemptElapsedTimeValue is specified in seconds.
%%% The unit for the callConnectedElapsedTimeValue is 100 milliseconds

-record('RequestedUTSI',
        {
          uSIServiceIndicator :: undefined | 'USIServiceIndicator'(),
          uSImonitorMode      :: undefined | 'USIMonitorMode'()
        }).
-type 'RequestedUTSI'() :: #'RequestedUTSI'{}.


-type 'RequestedUTSIList'() :: list('RequestedUTSI'()).% SEQUENCE SIZE
                                                %(minRequestedUTSINum..
                                                %maxRequestedUTSINum) OF  RequestedUTSI

-type 'ResourceID'() :: {lineID, 'Digits'()} |
                        {facilityGroupID, 'FacilityGroup'()} |
                        {facilityGroupMemberID, integer()} |
                        {trunkGroupID, integer()}.
%%% Indicates a logical identifier for the physical termination resource.

-type 'ResourceStatus'() :: busy | % (0),
                            idle.  % (1)

-type 'ResponseCondition'() :: intermediateResponse | % (0),
                               lastResponse.          % (1)
%%% additional values are outside the scope of this capability set
%%% ResponseCondition is used to identify the reason why ServiceFilteringResponse operation is sent.

-type 'RouteList'() :: list(). %SEQUENCE SIZE(1..3) OF OCTET STRING (SIZE
                                                %(minRouteListLength..
                                                %maxRouteListLength))
%%% Indicates a list of trunk groups or a route index. .
%%% Its content is network operator specific


-type 'RoutingNumber'() :: list(). %OCTET STRING (SIZE
                                                %(minRoutingNumberLength..
                                                %maxRoutingNumberLength))
%%% Indicates the Routing Number. Refer to Q.769.1 parameter Network Routing Number for encoding.


-type 'ScfID'() :: list(). %OCTET STRING (SIZE
                                                %(minScfIDLength..
                                                %maxScfIDLength))
%%% defined by network operator.
%%% Indicates the SCF identity.
%%% Used to derive the INAP address of the SCF to establish a connection between a requesting FE
%%% and the specified SCF.
%%% When ScfID is used in an operation which may cross an internetwork boundary, its encoding must
%%% be understood in both networks; this requires bilateral agreement on the encoding.
%%% Refer to  3.5/Q.713 "calling party address" parameter  for encoding.
%%% It indicates  the SCCP address  e.g. Global Title of the SCF,.
%%% Other encoding schemes are also possible as a operator specific option..

-type 'SCIBillingChargingCharacteristics'() :: list(). %OCTET STRING (SIZE
                                                %(minSCIBillingChargingLength..
                                                %maxSCIBillingChargingLength))
%%% This parameter indicates the billing and/or charging characteristics.
%%% Its content is network signalling / operator specific

%%% An example datatype definition for this parameter is given below:
%%% SCIBillingChargingCharacteristicsEmbeddedType  ::= CHOICE {
%%% chargeLevel			[0] OCTET STRING (SIZE (min..max),
%%% chargePulses			[1] Integer4,
%%% chargeMessages		[2] OCTET STRING (SIZE (min..max)
%%% }
%%% Depending on the applied charging scenario the following information elements
%%% can be included (refer to Q.1214 Appendix II):
%%% chargeLevel (scenario 3.2)
%%% chargePulses (scenario 3.2)
%%% chargeMessages (scenario 3.2)

-type 'SDSSinformation'() :: list(). %OCTET STRING (SIZE
                                                %(minSDSSinformationLength..
                                                %maxSDSSinformationLength))

%%% Its content is network operator specific


-record('ServiceAddressInformation',
        {
          serviceKey   :: undefined | 'ServiceKey'(),
          miscCallInfo :: undefined | 'DTMiscCallInfo'(),
          triggerType  :: undefined | 'TriggerType'()
        }).
-type 'ServiceAddressInformation'() :: #'ServiceAddressInformation'{}.

%%% Information that represents the result of trigger analysis and allows the SCF to choose the appropriate service logic

-type 'ServiceInteractionIndicators'() :: list(). %OCTET STRING (SIZE (
                                                %minServiceInteractionIndicatorsLength..
                                                %maxServiceInteractionIndicatorsLength))

%%% Indicators which are exchanged between SSP and SCP to resolve interactions between IN based services
%%% and network based services, respectively between different IN based services.

%%% Its content is network signalling/operator specific

%%% Note this parameter is kept in this Recommendation for backward compatibility to CS-1R,
%%% for this Recommendation see parameter ServiceInteractionIndicatorsTwo

-record('ServiceInteractionIndicatorsTwo',
        {
          forwardServiceInteractionInd  :: undefined | 'ForwardServiceInteractionInd'(),
          %% applicable to operations IDP, CON, ICA, CWA.

          backwardServiceInteractionInd :: undefined | 'BackwardServiceInteractionInd'(),
          %% applicable to operations IDP, CON, CTR, ETC, CWA.

          bothwayThroughConnectionInd   :: undefined | 'BothwayThroughConnectionInd'(),
          %% applicable to operations CTR, ETC.

          suspendTimer                  :: undefined | 'SuspendTimer'(),
          %% applicable to operations CON, ICA CWA.

          connectedNumberTreatmentInd   :: undefined | 'ConnectedNumberTreatmentInd'(),
          %% applicable to operations CON, CTR, ETC, CWA .

          suppressCallDiversionNotification :: undefined | boolean(),
          %% applicable to CON, ICA, CWA

          suppressCallTransferNotification  :: undefined | boolean(),
          %% applicable to CON, ICA, CWA

          allowCdINNoPresentationInd        :: undefined | boolean(),
          %% applicable to CON, ICA CWA
          %% indicates whether the Address Presentation restricted indicator of the ISUP
          %% "called IN number" shall be set to presentation allowed (TRUE)
          %%  or presentation restricted (FALSE)

          userDialogueDurationInd = true :: boolean(),
          %% applicable to operations CTR, ETC.
          %% applicable when interaction with the user is required during call set-up
          %% The interaction TRUE means the user interaction may last longer than 90 seconds.
          %% Otherwise the indicator should be set to FALSE. Used  for delaying ISUP T9 timer.

          overrideLineRestrictions = false :: boolean(),
          %% only applicable to operations  (e.g. Connect) which lead to a transition to a PIC before
          %% the AuthorizeCallSetup PIC.
          %% When set to TRUE, this parameter indicates that some facility restrictions
          %% should not be checked when the authority to place a call is verified in the
          %% Authorize_Call_Setup PIC.
          %% Which restrictions are actually overwriden is network specific.

          suppressVPNAPP = false :: boolean(),
          %% applicable to CWA, CON, ICA.
          %% indicates whether to allow or stop (suppress) the forward transmission of the VPN PSS1 capability.
          %% When set to TRUE, the  exchange, on receipt of this parameter, will not transmit for this call
          %% any ISUP Application transport parameter with Application Context Identifier set to « PSS1 ASE (VPN) »
          %% This indicator  is populated by the SCF, where the SCF and SSF in conjunction have provided the
          %% outgoing gateway PINX functionality as required by PSS1.

          calledINNumberOverriding :: undefined | boolean(),
          %% applicable to CON and CWA
          %% indicates whether the generation/override of  the ISUP
          %% "called IN number"  is allowed (TRUE)   or  not allowed (FALSE)
          %% If set to FALSE, the ISUP shall not generate a "called IN number"  or override
          %% an already existing "called IN number".
          %% if absent , the default will be "generation/overriding allowed" (TRUE).

          redirectServiceTreatmentInd :: undefined | {redirectReason, undefined | 'RedirectReason'()}
          %% applicable to operation CON .
          %% if absent , call redirection service is not allowed
          %% Existence of this parameter requests Pivot Routing or Redirection supplementary service to be
          %% allowed for-the new routing address specified in the Connect operation.
          %% If absent, neither Pivot Routing nor Redirection service is allowed.
          %% The redirectReason Parameter indicates the reason for invoking Pivot Routing / Redirection service
          %% Whether the service is actually invoked depends only on SSP conditions.
          %% In this capability set the SCP will not know all the SSP conditions.
          %% To send that kind of conditions is out of scope of this capability set.
          %% If the service is allowed, the parameter must be sent in the ISUP-FAC message (Pivot Routing case)
          %% or ISUP-REL message (Redirection case) as the service is invoked,
        }).
-type 'ServiceInteractionIndicatorsTwo'() :: #'ServiceInteractionIndicatorsTwo'{}.

%%% ServiceInteractionIndicatorsTwo contains Indicators which are exchanged between SSP and SCP to resolve interactions
%%% between IN based services  and network based services, respectively between different IN based services.

-type 'ServiceKey'() :: 'Integer4'().
%%% Information that allows the SCF to choose the appropriate service logic.

-type 'ServiceProfileIdentifier'() :: list(). % OCTET STRING
%%% Indicates a particular ISDN terminal. Refer to Q.932 for encoding.

-type 'ServingAreaID'() :: list(). %OCTET STRING %% 'LocationNumber'().
%%% Identifies the local serving area where a network provider operates. Uses the LocationNumber
%%% format which is based on the Q.763 Location Number format.
%%% The Nature of Address indicator field shall be set to "Spare" (value 00000000).
%%% The Numbering Plan Indicator field shall be set to "Spare" (value 000).
%%% Defined by the network operator.

-type 'SFBillingChargingCharacteristics'() :: list(). % OCTET STRING (SIZE
                                                %(minSFBillingChargingLength..
                                                %maxSFBillingChargingLength))
%%% This parameter indicates the billing and/or charging characteristics for filtered calls.
%%% Its content is network operator specific
%%% Its content is network signalling/operator specific.


%%% SupportedTriggers  TRIGGER ::= {}

-type 'SuspendTimer'() :: integer(). % INTEGER (0..120)
%%% value in seconds

-type 'TDPIdentifier'() :: {oneTrigger, integer()} |
                           {triggers, 'Triggers'()}.

-type 'TerminalType'() :: unknown |    % (0),
                          dialPulse |  % (1),
                          dtmf |       % (2),
                          isdn |       % (3),
                          isdnNoDtmf | % (4),
                          spare.       % (16)

%%% Identifies the terminal type so that the SCF can specify, to the SRF, the appropriate type of capability
%%% (voice recognition, DTMF, display capability, etc.). Since present signalling systems do not convey
%%% terminal type, this parameter applies only at originating or terminating local exchanges.

-type 'XTimerID'() :: tssf. % (0)
%%% Indicates the timer to be reset.

-type 'TimerValue'() :: 'Integer4'().
%%% Indicates the timer value (in seconds).


-type 'TraceInformation'() :: list('TraceItem'()). %SEQUENCE OF TraceItem

-type 'TraceItem'() :: {scf, 'ScfID'()}.

-type 'TravellingClassMark'() :: list(). %OCTET STRING %% 'LocationNumber'().
%%% Indicates travelling class mark information.
%%% Uses the LocationNumber format which is based on the Q.763 Location Number format.
%%% The Nature of Address indicator field shall be set to "Spare" (value 00000000).
%%% The Numbering Plan Indicator field shall be set to "Spare" (value 000).
%%% Maximum 2 digits.

%%% TriggerData ::= SEQUENCE {
%%% triggerId	 	[0] 	TRIGGER.&id		({SupportedTriggers}),
%%% triggerPar 	[1]TRIGGER.&Parameter ({SupportedTriggers}{@triggerId})
%%%
%%% }

-record('TriggerDataIdentifier',
        {
          triggerID         :: undefined | 'EventTypeBCSM'(),
          profileIdentifier :: undefined | 'ProfileIdentifier'(),
          extensions        :: undefined | 'Extensions'()
        }).
-type 'TriggerDataIdentifier'() :: #'TriggerDataIdentifier'{}.
%%% It is outside the scope of this capability set  whether all TDP types really apply

-type 'TriggerDPType'() :: 'tdp-r' | % (0),
                           'tdp-n'.  % (1)



-type 'TriggerResults'() :: list('TriggerResult'()). %SEQUENCE SIZE (1..numOfTriggers) OF TriggerResult

-record('TriggerResult',
        {
          tDPIdentifer    :: undefined | integer(),
          actionPerformed :: undefined | 'ActionPerformed'(),
          dpName          :: undefined | 'EventTypeBCSM'()
        }).
-type 'TriggerResult'() :: #'TriggerResult'{}.


-type 'Triggers'() :: list('Trigger'()). % SEQUENCE SIZE(1..numOfTriggers) OF Trigger

-record('Trigger',
        {
          tDPIdentifier :: undefined | integer(),
          dpName        :: undefined | 'EventTypeBCSM'()
        }).
-type 'Trigger'() :: #'Trigger'{}.

-type 'TriggerStatus'() :: created |       % (0),
                           alreadyExist |  % (1),
                           deleted |       % (2),
                           unknownTrigger. % (3)


-type 'TriggerType'() :: featureActivation |   % (0),
                         verticalServiceCode | % (1),
                         customizedAccess |    % (2),
                         customizedIntercom |  % (3),
                         emergencyService |    % (12),
                         aFR |                 % (13),
                         sharedIOTrunk |       % (14),
                         offHookDelay |        % (17),
                         channelSetupPRI |     % (18),
                         tNoAnswer |           % (25),
                         tBusy |               % (26),
                         oCalledPartyBusy |    % (27),
                         oNoAnswer |           % (29),
                         originationAttemptAuthorized | %(30),
                         oAnswer |             % (31),
                         oDisconnect |         % (32),
                         termAttemptAuthorized | %(33),
                         tAnswer |             % (34),
                         tDisconnect.          %(35)
%%% The type of trigger which caused call suspension
%%% 4-11: Reserved; 15,16: Reserved; 19-24: Reserved


-type 'USIInformation'() :: list(term()). % OCTET STRING (SIZE
                                                %(minUSIInformationLength..
                                                %maxUSIInformationLength))
%%% Its content is network signalling/operator specific


-type 'USIMonitorMode'() :: monitoringActive |  % (0),
                            monitoringInactive. % (1)

%%% Indicates if the monitoring relationship for the specified UTSI IE should be activated or deactivated.


-type 'USIServiceIndicator'() :: {global, 'OBJECT IDENTIFIER'()} |
                                 {local, atom() | list()}. %OCTET STRING (SIZE (
                                                %minUSIServiceIndicatorLength..
                                                %maxUSIServiceIndicatorLength))
%%% In case of local its content is network signalling/operator specific




-type 'VPNIndicator'() :: boolean().
%%% This parameter is set to TRUE if the originating call part supports VPN with PSS1 information flows


%%% The Definition of range of constants Follows
%% highLayerCompatibilityLength	INTEGER ::= 2
%% minCauseLength	INTEGER ::= 1
%% numOfCounters	INTEGER ::= 100
%% numOfInfoItems	INTEGER ::= 5


%% 	minAChBillingChargingLength		INTEGER ::= 1	-- example value
%% 	maxAChBillingChargingLength	INTEGER ::=27	-- example value
%% 	minATMTrafficDescriptorLength	INTEGER ::= 1	-- example value
%% 	maxATMTrafficDescriptorLength	INTEGER ::=27	-- example value
%% 	minBackwardGVNSLength			INTEGER ::= 1	-- example value
%% 	maxBackwardGVNSLength			INTEGER ::=27	-- example value
%% 	maxBearerCapabilityLength		INTEGER ::=27	-- example value
%% 	minBroadbandBearerCapabilityLength  INTEGER ::= 1  	 -- example value
%% 	maxBroadbandBearerCapabilityLength  INTEGER ::=27  	 -- example value
%% 	minCalledDirectoryNumberLength  INTEGER ::= 1	-- example value
%% 	maxCalledDirectoryNumberLength  INTEGER ::=27	-- example value
%% 	minCalledPartyNumberLength		INTEGER ::= 0	-- example value
%% 	maxCalledPartyNumberLength	INTEGER ::= 15
%% 	minCalledPartySubaddressLength INTEGER ::= 1          	                  -- example value
%% 	maxCalledPartySubaddressLength INTEGER ::= 27
%% 	minCallingGeodeticLocationLength INTEGER ::= 1	 -- example value
%% 	maxCallingGeodeticLocationLength INTEGER ::= 10	-- example value
%% 	minCallingPartyNumberLength	INTEGER ::= 1	-- example value
%% 	maxCallingPartyNumberLength	INTEGER ::= 27
%% 	minCallingPartySubaddressLength INTEGER ::= 1         	                  -- example value
%% 	maxCallingPartySubaddressLength INTEGER ::= 27

%% 	minCallResultLength				INTEGER ::= 1	-- example value
%% 	maxCallResultLength				INTEGER ::=27	-- example value
%% 	minCarrierLength						INTEGER ::= 3     		 -- example value
%% 	maxCarrierLength						INTEGER ::= 10     	-- example value
%% 	maxCauseLength					INTEGER ::= 16	-- example value
%% 	minDigitsLength						INTEGER ::= 1	-- example value
%% 	maxDigitsLength						INTEGER ::=27	-- example value
%% 	minDisplayInformationLength					INTEGER ::= 1	-- example value
%% 	maxDisplayInformationLength					INTEGER ::=27	-- example value
%% 	minEventSpecificInformationChargingLength	INTEGER ::= 1	-- example value
%% 	maxEventSpecificInformationChargingLength	INTEGER ::=27	-- example value
%% 	minEventTypeChargingLength		INTEGER ::= 1	-- example value
%% 	maxEventTypeChargingLength		INTEGER ::=27	-- example value
%% 	minFCIBillingChargingLength		INTEGER ::= 1	-- example value
%% 	maxFCIBillingChargingLength		INTEGER ::=27	-- example value
%% 	minForwardGVNSLength			INTEGER ::= 1	-- example value
%% 	maxForwardGVNSLength			INTEGER ::=27	-- example value
%% 	minGenericNameLength				INTEGER ::= 1	-- example value
%% 	maxGenericNameLength			INTEGER ::=27	-- example value
%% 	minGenericNumberLength			INTEGER ::= 1	-- example value
%% 	maxGenericNumberLength			INTEGER ::=27	-- example value
%% 	maxInitialTimeInterval		INTEGER ::=27	-- example value
%% 	maxINServiceCompatibilityIndLength	INTEGER ::=27	 -- example value
%% 	minIPAvailableLength				INTEGER ::= 1	-- example value
%% 	maxIPAvailableLength				INTEGER ::=27	-- example value
%% 	minIPSSPCapabilitiesLength		INTEGER ::= 1	-- example value
%% 	maxIPSSPCapabilitiesLength		INTEGER ::=27	-- example value
%% 	minISDNAccessRelatedInfoLength	INTEGER ::=  1  	-- example value
%% 	maxISDNAccessRelatedInfoLength INTEGER ::= 10	  -- example value
%% 	minISUPParameterLength         INTEGER ::= 1
%% 	maxISUPParameterLength         INTEGER ::= 255
%% 	maxISUPParameterType    	   INTEGER ::= 255
%% 	minLocationNumberLength			INTEGER ::= 1	-- example value
%% 	maxLocationNumberLength		INTEGER ::=27	-- example value


%% 	minMidCallControlInfoNum	INTEGER ::= 1	-- example value
%% 	maxMidCallControlInfoNum	INTEGER ::=27	-- example value
%% 	minOriginalCalledPartyIDLength INTEGER ::=  1	-- example value
%% 	maxOriginalCalledPartyIDLength INTEGER ::= 15	-- example value
%% 	minQoSParameterLength		INTEGER ::=	1	-- example value
%% 	maxQoSParameterLength		INTEGER ::=	5	-- example value

%% 	minReasonLength				INTEGER ::=	1	-- example value
%% 	maxReasonLength				INTEGER ::=	5	-- example value
%% 	minRedirectingPartyIDLength			INTEGER ::= 1	-- example value
%% 	maxRedirectingPartyIDLength			INTEGER ::=27	-- example value
%% 	minRequestedUTSINum		INTEGER ::= 1	-- example value
%% 	maxRequestedUTSINum		INTEGER ::=27	-- example value
%% 	minRouteListLength			INTEGER ::=	1	-- example value
%% 	maxRouteListLength			INTEGER ::=	27	-- example value
%% 	minRoutingNumberLength			INTEGER ::= 1	-- example value
%% 	maxRoutingNumberLength			INTEGER ::=27	-- example value
%% 	minScfIDLength						INTEGER ::= 1	-- example value
%% 	maxScfIDLength						INTEGER ::=27	-- example value

%% 	minSCIBillingChargingLength		INTEGER ::= 1	-- example value
%% 	maxSCIBillingChargingLength		INTEGER ::=27	-- example value
%% 	minSDSSinformationLength		INTEGER ::=	1   	-- example value
%% 	maxSDSSinformationLength		INTEGER ::=27   	-- example value
%% 	minServiceInteractionIndicatorsLength					INTEGER ::=	1	-- example value
%% 	maxServiceInteractionIndicatorsLength					INTEGER ::=	5	-- example value
%% 	minSFBillingChargingLength		INTEGER ::= 1	-- example value
%% 	maxSFBillingChargingLength		INTEGER ::=27	-- example value
%% 	minUSIInformationLength			INTEGER ::= 1	-- example value
%% 	maxUSIInformationLength		INTEGER ::=	5	-- example value
%% 	minUSIServiceIndicatorLength		INTEGER ::= 1	-- example value
%% 	maxUSIServiceIndicatorLength	INTEGER ::=27	-- example value
%% 	numOfBCSMEvents					INTEGER ::=	4	-- example value
%% 	numOfBCUSMEvents				INTEGER ::=	4	-- example value
%% 	numOfChargingEvents				INTEGER ::= 4	-- example value
%% 	numOfCSAs					INTEGER ::=	10	-- example value
%% 	numOfCSs						INTEGER ::=	10	-- example value
%% 	numOfGenericNumbers				INTEGER ::= 2	-- example value
%% 	numOfINProfile					INTEGER ::=	1 	-- example value
%% 	numOfTriggers				INTEGER ::=27 	-- example value
%% 	numOfInServiceCompatibilityIndLength		INTEGER ::= 2	-- example value
%% 	numOfLegs					INTEGER ::=	10	-- example value
%% 	maxAmount				INTEGER ::= 10000	-- example value
%% 	maxCallReferenceLength		INTEGER ::=	27	-- example value
%% 	maxInitialUnitIncrement	INTEGER ::= 2	-- example value
%% 	maxScalingFactor			INTEGER ::= 2	-- example value
%% 	maxSegmentsPerDataInterval INTEGER ::=27	-- example value

%% 	ub-nbCall				INTEGER ::=27	-- example value
%% 	numOfAddresses						INTEGER ::=27 	-- example value
%% 	numOfServiceKeys				INTEGER ::= 3


%% 	minAttributesLength	INTEGER	::= 1
%% 	maxAttributesLength	INTEGER ::=27
%% 	minMailBoxIDLength	INTEGER	::= 1
%% 	maxMailBoxIDLength	INTEGER	::=27
%% 	minMessageContentLength	INTEGER	::= 1
%% 	maxMessageContentLength	INTEGER	::=27
%% 	minReceivedInformationLength	INTEGER	::= 1
%% 	maxReceivedInformationLength 	INTEGER	::=27
%% 	maxRecordingTime	INTEGER	::=27
%% 	numOfMessageIDs	INTEGER	::= 2

%% 	numOfExtensions					INTEGER ::= 1
%% 	maxRecordedMessageUnits		INTEGER  ::=27

%% -- Note: Definition of the cause values (informative)
%% -- noIndication Cause ::= '00'H
%% -- originationDenied Cause ::= '01'H
%% -- collectInformationFailure Cause ::=  '02'H
%% -- invalidCollectedInformation Cause ::=  '03'H
%% -- authorisationFailure Cause ::=  '04'H
%% -- routeSelectFailure Cause ::= '05'H
%% -- calledPartyBusy Cause ::=  '06'H
%% -- oNoAnswer Cause ::= '07'H
%% -- ss7Failure Cause ::= '08'H
%% -- bptyNoAnswer Cause ::= '09'H
%% -- tException Cause ::= '0A'H
%% -- routeFailure1 Cause ::= '0B'H
%% -- routeFailure2 Cause ::= '0C'H
%% -- bPtyBusyUDUB Cause ::= '0D'H
%% -- bPtyBusyNDUB Cause ::= '0E'H
%% -- aPtyAbandon Cause ::= '0F'H
%% -- aPtyDisc Cause ::= '10'H
%% -- bPtyDisc Cause ::= '11'H
%% -- terminationDenied Cause ::= '12'H
%% -- presentationFailure Cause ::= '13'H
%% -- calledPartyCallRejected Cause ::= '14'H
%% -- failureOfConnection Cause ::= '15'H
%% -- releaseCall Cause ::= '16'H
%% -- bPtyAlerted Cause ::= '17'H
%% -- accessOutOfOrder Cause ::= '18'H
%% -- accessMaintenanceBusy Cause ::= '19'H
%% -- accessCustomerBusy Cause ::= '1A'H
%% -- pstnLineBusy Cause ::= '1B'H
%% -- isdnLineBusy Cause ::= '1C'H
%% -- trunkGroupBusy Cause ::= '1D'H
%% -- presentCallFailure Cause ::= '1E'H
%% -- collectDigits Cause ::= '1F'H
%% -- notReachable Cause ::= '20'H


-type 'NAIType'() ::
        spare |            %0
        subscriberNumber | %1 - national use
        unknown |          %2 - national use
        nationalNumber |   %3 - national use
        internationNumber. %4

-type 'NIType'() :: % number indicator - for sorm
        private |
        local |
        zone |
        intercity |
        international |
        emergency.

-type 'INNIType'() ::
        routingToInternalNumberAllowed |   %0
        routingToInternalNumberNotAllowed. %1

-type 'NPIType'() ::
        spare |
        isdnTelephony | %E.164
        dataNumberingPlan | %X.121 (national use)
        telexNumberingPlan | %F.69 (national use)
        reserved1 |     %reserved for national use
        reserved2 |     %reserved for national use
        reserved3.      %reserved for national use

-type 'APRIType'() ::
        presentationAllowed |    %0
        presentationRestricted | %1
        addressNotAvailable |    %2 - national use, subfields NAI, incomplete, NP must be coded as 0's
        spare.                   %3

-type 'ScreeningType'() ::
        userProvidedNotVerified |       %0
        userProvidedVerifiedAndPassed | %1
        userProvidedVerifiedAndFailed | %2
        networkProvided.                %3

-record('OptionalBackwardCallIndicators',
        {
          inbInfoInd = noIndication :: 'InBandInfoIndicator'(),
          callDiversionInd = noIndication :: 'CallDiversionIndicator'(),
          simpleSegmentationInf = noAdditionalInformation :: 'SimpleSegmentationIndicator'(),
          mlppUserInd = noIndication :: 'MLPPUserIndicator'()
        }).
-type 'OptionalBackwardCallIndicators'() :: #'OptionalBackwardCallIndicators'{}.

-type 'InBandInfoIndicator'() ::
        noIndication |                %0
        inBandInfoOrPatternAvailable. %1

-type 'CallDiversionIndicator'() ::
        noIndication |         %0
        callDiversionMayOccur. %1

-type 'SimpleSegmentationIndicator'() ::
        noAdditionalInformation |        %0
        additionalInformationWillBeSent. %1

-type 'MLPPUserIndicator'() ::
        noIndication | %0
        mlppUser.      %1

-type 'GenericNotificationIndicatorList'() :: list('GenericNotificationIndicator'() | {release, 'Cause'()}). %% #8851

-type 'GenericNotificationIndicator'() ::
        userSuspended |          %0
        userResumed   |          %1
        bearerServiceChange |    %2
        callCompletionDelay |    %4
        conferenceEstablished |  %66
        conferenceDisconnected | %67
        otherPartyAdded |        %68
        isolated |               %69
        reattached |             %70
        otherPartyIsolated |     %71
        otherPartyReattached |   %72
        otherPartySplit |        %73
        otherPartyDisconnected | %74
        conferenceFloating |     %75
        callIsAWaitingCall |     %96
        callTransferAlerting |   %113
        callTransferActive |     %114
        remoteHold |             %121
        remoteRetrieval |        %122
        callIsDiverting |        %123
        localHold |              % our specific
        localRetrieval |         % our specific
        release |                % our specific - leg released
        sessionPing |            % our specific - ping by timer
        sessionPong |            % our specific - pong response on ping
        mediaChanged |
        mediaConnected |
        pickupActivating.


-record('TrunkGroupId',
        {
          trunkGroupId :: undefined | list(),
          trunkId :: undefined | list(),
          pCMId :: undefined | list(),
          channelNumber :: undefined | integer()}).
-type 'TrunkGroupId'() :: #'TrunkGroupId'{}.

-type 'SDPRecordType'() :: term().

-record('SDPType',
        {
          type=unknown :: offer | answer | unknown,
          body :: undefined | ['SDPRecordType'()] | term() %% use record(sdp) from sdp_lib to transfer sdp
        }).

-type 'SDPType'() :: #'SDPType'{}.

-record('CallId',
        {
            id       :: binary(),
            fromTag  :: binary(),
            toTag    :: binary()
        }).

-type 'CallId'() :: #'CallId'{}.

-type 'MediaPointType'() :: any().

-type 'SID'() :: binary().

%-type 'ServiceDataContainer'() :: term().
%% container used to transmit service dependent data (user data for services) from PA to Core

%-type 'ServiceTriggersContainer'() :: term().
%% container used to transmit service triggers from PA to Core

-type 'SGroupType'() :: atom(). % Name of subscriber group in atom format

-record('ReferType',
        {
         exchange   :: undefined | list(),
         routingKey :: undefined | list(),
         sid        :: undefined | 'SID'(),
         callRef    :: undefined | 'CallReference'(),
         conf_id :: undefined | list() %% ID (номер) конференции
        }).

-type 'ReferType'() :: #'ReferType'{}.

-record('MLPPPrecedence',
        {
          lfb = 2 :: integer(), %% half byte, default value 2 - look ahead for busy (LFB) not allowed
          precedenceLevel = 4 :: integer(), %% 0 - flash override, 1 - flash, 2 - immediate, 3 - priority, 4 - routine
          network_identity = 0 :: integer(), %% 16 bit - 4 bcd digit country code accoring E.164
          mlpp_service_domain = 0 :: integer() %% 24bit integer
        }).

-type 'MLPPPrecedence'() :: #'MLPPPrecedence'{}.

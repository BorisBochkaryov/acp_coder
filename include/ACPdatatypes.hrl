%% Generated by the Erlang ASN.1 compiler version:4.0.3
%% Purpose: Erlang record definitions for each named and unnamed
%% SEQUENCE and SET, and macro definitions for each value
%% definition,in module ACPdatatypes



-ifndef(_ACPDATATYPES_HRL_).
-define(_ACPDATATYPES_HRL_, true).

-record('CalledPartyNumber',{
nai = asn1_NOVALUE, ni = asn1_NOVALUE, incomplete = asn1_NOVALUE, inni = asn1_NOVALUE, npi = asn1_NOVALUE, digits}).

-record('RedirectionNumber',{
nai = asn1_NOVALUE, ni = asn1_NOVALUE, incomplete = asn1_NOVALUE, inni = asn1_NOVALUE, npi = asn1_NOVALUE, digits = asn1_NOVALUE}).

-record('CallTransferNumber',{
nai = asn1_NOVALUE, ni = asn1_NOVALUE, incomplete = asn1_NOVALUE, inni = asn1_NOVALUE, npi = asn1_NOVALUE, digits = asn1_NOVALUE}).

-record('CallingPartyNumber',{
nai, ni, incomplete, npi, apri, screening, digits}).

-record('OriginalCalledNumber',{
nai, ni, incomplete, npi, apri, digits}).

-record('RedirectingNumber',{
nai, ni, incomplete, npi, apri, digits}).

-record('ACPRedirectionInformation',{
originalRedirectionReason, redirectingIndicator, redirectingReason, redirectionCounter}).

-record('OptionalBackwardCallInidicators',{
inbInfoInd = asn1_DEFAULT, callDiversionInd = asn1_DEFAULT, simpleSegmentationInf = asn1_DEFAULT, mlppUserInd = asn1_DEFAULT}).

-record('TrunkGroupId',{
trunkGroupId, trunkId = asn1_NOVALUE, pCMId = asn1_NOVALUE, channelNumber = asn1_NOVALUE}).

-define('minACPCalledPartyNumberLength', 0).
-define('maxACPCalledPartyNumberLength', 27).
-define('minACPCallingPartyNumberLength', 0).
-define('maxACPCallingPartyNumberLength', 27).
-define('numOfACPAddresses', 27).
-define('numOfGenericNotificatioIndicators', 10).
-endif. %% _ACPDATATYPES_HRL_

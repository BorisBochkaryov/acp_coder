%%%-------------------------------------------------------------------
%%% @author Eugeny Bachar <eugeny.bachar@eltex.nsk.ru>
%%% @copyright (C) 2010, Eugeny Bachar
%%% @doc
%%%  Adapter Core Protocol - headers
%%% @end
%%% Created :  5 Oct 2010 by Eugeny Bachar <eugeny.bachar@eltex.nsk.ru>
%%%-------------------------------------------------------------------


-record('SetupInd', {arg :: #'SetupIRType'{}}).
-record('SetupIndAck', {arg :: #'SetupAckType'{}}).
-record('SetupReq', {arg ::  #'SetupIRType'{}}).
-record('SetupReqAck', {arg :: #'SetupAckType'{}}).
-record('SetupConf', {arg :: #'SetupCRType'{}}).
-record('SetupResp', {arg :: #'SetupCRType'{}}).
-record('CallProgressInd', {arg :: #'CallProgressType'{}}).
-record('CallProgressReq', {arg :: #'CallProgressType'{}}).
-record('SubsequentAddressInd', {arg :: #'SubsequentAddressType'{}}).
-record('SubsequentAddressReq', {arg :: #'SubsequentAddressType'{}}).
-record('AddressEndInd', {arg :: #'SubsequentAddressType'{}}).
-record('ServiceFeatureInd', {arg :: #'ServiceFeatureType'{}}).
-record('ServiceFeatureReq', {arg :: #'ServiceFeatureType'{}}).
-record('DataInd', {arg :: #'UserDataType'{}}). % Not used
-record('DataReq', {arg :: #'UserDataType'{}}). % Not used
-record('ReleaseInd', {arg :: #'ReleaseType'{}}).
-record('ReleaseIndAck', {arg :: #'ReleaseAckType'{}}).
-record('ReleaseReq', {arg :: #'ReleaseType'{}}).
-record('ReleaseReqAck', {arg :: #'ReleaseAckType'{}}).
-record('FailureInd', {arg :: #'FailureType'{}}). % Not used
-record('NetworkSuspendInd', {arg :: #'NetworkSRType'{}}). % Not used
-record('NetworkSuspendReq', {arg :: #'NetworkSRType'{}}). % Not used
-record('NetworkResumeInd', {arg :: #'NetworkSRType'{}}). % Not used
-record('NetworkResumeReq', {arg :: #'NetworkSRType'{}}). % Not used


-type 'AcpBody'() :: #'SetupInd'{} |
                     #'SetupIndAck'{} |
                     #'SetupReq'{} |
                     #'SetupReqAck'{} |
                     #'SetupConf'{} |
                     #'SetupResp'{} |
                     #'CallProgressInd'{} |
                     #'CallProgressReq'{} |
                     #'SubsequentAddressInd'{} |
                     #'SubsequentAddressReq'{} |
                     #'AddressEndInd'{} |
                     #'ServiceFeatureInd'{} |
                     #'ServiceFeatureReq'{} |
                     #'DataInd'{} |
                     #'DataReq'{} |
                     #'ReleaseInd'{} |
                     #'ReleaseIndAck'{} |
                     #'ReleaseReq'{} |
                     #'ReleaseReqAck'{} |
                     #'FailureInd'{} |
                     #'NetworkSuspendInd'{} |
                     #'NetworkSuspendReq'{} |
                     #'NetworkResumeInd'{} |
                     #'NetworkResumeReq'{}.

-record('AcpMessage',
        {
          uri  :: binary(),
          callRef :: 'CallReference'(),
          body :: 'AcpBody'()
        }).

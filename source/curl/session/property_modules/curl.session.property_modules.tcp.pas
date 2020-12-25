(******************************************************************************)
(*                                 libPasCURL                                 *)
(*            delphi and object pascal wrapper around cURL library            *)
(*                        https://github.com/curl/curl                        *)
(*                                                                            *)
(* Copyright (c) 2020                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpascurl                  ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* This source  is free software;  you can redistribute  it and/or modify  it *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation; either version 3 of the License.                      *)
(*                                                                            *)
(* This code is distributed in the  hope that it will  be useful, but WITHOUT *)
(* ANY  WARRANTY;  without even  the implied  warranty of MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License for *)
(* more details.                                                              *)
(*                                                                            *)
(* A copy  of the  GNU General Public License is available  on the World Wide *)
(* Web at <http://www.gnu.org/copyleft/gpl.html>. You  can also obtain  it by *)
(* writing to the Free Software Foundation, Inc., 51  Franklin Street - Fifth *)
(* Floor, Boston, MA 02110-1335, USA.                                         *)
(*                                                                            *)
(******************************************************************************)

unit curl.session.property_modules.tcp;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, utils.timeinterval,
  curl.session.property_module;

type
  TModuleTCP = class(TPropertyModule)
  protected
    { Enable TCP Fast Open. }
    procedure SetFastOpen (AEnable : Boolean);

    { Set the TCP_NODELAY option. }
    procedure SetNoDelay (AEnable : Boolean);

    { Enable TCP keep-alive probing. }
    procedure SetKeepAlive (AEnable : Boolean);

    { Set TCP keep-alive idle time wait. }
    procedure SetKeepAliveIdle (ATimeout : TTimeInterval);

    { Set TCP keep-alive interval. }
    procedure SetKeepAliveInterval (ATimeout : TTimeInterval);
  protected
    { Enable TCP Fast Open. 
      TCP Fast Open (RFC7413) is a mechanism that allows data to be carried in 
      the SYN and SYN-ACK packets and consumed by the receiving end during the 
      initial connection handshake, saving up to one full round-trip time 
      (RTT). }
    property FastOpen : Boolean write SetFastOpen;

    { Set the TCP_NODELAY option. 
      Setting this option will disable TCP's Nagle algorithm on this connection. 
      The purpose of this algorithm is to try to minimize the number of small 
      packets on the network (where "small packets" means TCP segments less than 
      the Maximum Segment Size (MSS) for the network).
      Maximizing the amount of data sent per TCP segment is good because it 
      amortizes the overhead of the send. However, in some cases small segments 
      may need to be sent without delay. This is less efficient than sending 
      larger amounts of data at a time, and can contribute to congestion on the 
      network if overdone. }
    property NoDelay : Boolean write SetNoDelay;

    { Enable TCP keep-alive probing. 
      If set, TCP keepalive probes will be sent. }
    property KeepAlive : Boolean write SetKeepAlive;

    { Set TCP keep-alive idle time wait. 
      Sets the delay, that the operating system will wait while the connection 
      is idle before sending keepalive probes. }
    property KeepAliveIdle : TTimeInterval write SetKeepAliveIdle;

    { Set TCP keep-alive interval. 
      Sets the interval, that the operating system will wait between sending 
      keepalive probes. }
    property KeepAliveInterval : TTimeInterval write SetKeepAliveInterval;
  end;

implementation

{ TModuleTCP }

procedure TModuleTCP.SetFastOpen (AEnable : Boolean);
begin
  Option(CURLOPT_TCP_FASTOPEN, AEnable);
end;

procedure TModuleTCP.SetNoDelay (AEnable : Boolean);
begin
  Option(CURLOPT_TCP_NODELAY, AEnable);
end;

procedure TModuleTCP.SetKeepAlive (AEnable : Boolean);
begin
  Option(CURLOPT_TCP_KEEPALIVE, AEnable);
end;

procedure TModuleTCP.SetKeepAliveIdle (ATimeout : TTimeInterval);
begin
  Option(CURLOPT_TCP_KEEPIDLE, Longint(ATimeout.Seconds));
end;

procedure TModuleTCP.SetKeepAliveInterval (ATimeout : TTimeInterval);
begin
  Option(CURLOPT_TCP_KEEPINTVL, Longint(ATimeout.Seconds));
end;

end.

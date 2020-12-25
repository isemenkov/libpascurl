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

unit curl.response.property_modules.timeout;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, utils.timeinterval, curl.response.property_module;

type
  TModuleTimeout = class(TPropertyModule)
  protected
    { Get transfer total time. }
    function GetTotal : TTimeInterval;

    { Get the name lookup time. }
    function GetNameLookup : TTimeInterval;

    { Get the time until connect. }
    function GetConnect : TTimeInterval;

    { Get the time until the SSL/SSH handshake is completed. }
    function GetAppConnect : TTimeInterval;

    { Get the time until the file transfer start. }
    function GetPreTransfer : TTimeInterval;

    { Get time until the first byte is received. }
    function GetStartTransfer : TTimeInterval;
  protected
    { Get transfer total time. }
    property Total : TTimeInterval read GetTotal;

    { Get the name lookup time. }
    property NameLookup : TTimeInterval read GetNameLookup;

    { Get the time until connect. }
    property Connect : TTimeInterval read GetConnect;

    { Get the time until the SSL/SSH handshake is completed. }
    property AppConnect : TTimeInterval read GetAppConnect;

    { Get the time until the file transfer start. }
    property PreTransfer : TTimeInterval read GetPreTransfer;

    { Get time until the first byte is received. }
    property StartTransfer : TTimeInterval read GetStartTransfer;
  end;

implementation

{ TModuleTimeout }

function TModuleTimeout.GetTotal : TTimeInterval;
begin
  Result := TTimeInterval.Create;
  Result.Milliseconds := GetInt64Value(CURLINFO_TOTAL_TIME, 
    CURLINFO_TOTAL_TIME_T);
end;

function TModuleTimeout.GetNameLookup : TTimeInterval;
begin
  Result := TTimeInterval.Create;
  Result.Milliseconds := GetInt64Value(CURLINFO_NAMELOOKUP_TIME,
    CURLINFO_NAMELOOKUP_TIME_T);
end;

function TModuleTimeout.GetConnect : TTimeInterval;
begin
  Result := TTimeInterval.Create;
  Result.Milliseconds := GetInt64Value(CURLINFO_CONNECT_TIME,
    CURLINFO_CONNECT_TIME_T);
end;

function TModuleTimeout.GetAppConnect : TTimeInterval;
begin
  Result := TTimeInterval.Create;
  Result.Milliseconds := GetInt64Value(CURLINFO_APPCONNECT_TIME,
    CURLINFO_APPCONNECT_TIME_T);
end;

function TModuleTimeout.GetPreTransfer : TTimeInterval;
begin
  Result := TTimeInterval.Create;
  Result.Milliseconds := GetInt64Value(CURLINFO_PRETRANSFER_TIME,
    CURLINFO_PRETRANSFER_TIME_T);
end;

function TModuleTimeout.GetStartTransfer : TTimeInterval;
begin
  Result := TTimeInterval.Create;
  Result.Milliseconds := GetInt64Value(CURLINFO_STARTTRANSFER_TIME,
    CURLINFO_STARTTRANSFER_TIME_T);
end;

end.
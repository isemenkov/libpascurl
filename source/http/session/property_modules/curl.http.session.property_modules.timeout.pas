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

unit curl.http.session.property_modules.timeout;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.session.property_module, utils.timeinterval;

type
  TModuleTimeout = class(curl.session.property_module.TPropertyModule)
  protected
    { Timeout for Expect: 100-continue response. }
    procedure SetContinueResponse (ATimeout : TTimeInterval);
  public
    { Timeout for Expect: 100-continue response. 
      Tell libcurl the time to wait for a server response with the HTTP status 
      100 (Continue), 417 (Expectation Failed) or similar after sending an HTTP 
      request containing an Expect: 100-continue header. If this times out 
      before a response is received, the request body is sent anyway. }
    property ContinueResponse : TTimeInterval write SetContinueResponse;
  end;

implementation

{ TModuleTimeout }

procedure TModuleTimeout.SetContinueResponse (ATimeout : TTimeInterval);
begin
  Option(CURLOPT_EXPECT_100_TIMEOUT_MS, ATimeout.Milliseconds);
end;

end.

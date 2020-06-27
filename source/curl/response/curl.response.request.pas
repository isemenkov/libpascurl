(******************************************************************************)
(*                                 libPasCURL                                 *)
(*                 object pascal wrapper around cURL library                  *)
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

unit curl.response.request;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  curl.utils.errorstack, libpascurl;

type
  { Request info }
  TRequest = class
  public
    { Get size of sent request }
    function Length : TDataSize;
  private
    constructor Create(ACurl : CURL; AErrors : PErrorStack);
  private
    FCurl : CURL;
    FErrors : PErrorStack;
  end;

implementation

{ TRequest }

constructor TRequest.Create(ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

function TRequest.Length : TDataSize;
var
  bytes : Longint = 0;
begin
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_REQUEST_SIZE, @bytes));
  Result := TDataSize.Create;
  Result.Bytes := bytes;
end;

end.
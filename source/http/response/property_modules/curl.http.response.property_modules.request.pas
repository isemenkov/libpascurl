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

unit curl.http.response.property_modules.request;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, utils.datasize, curl.http.request.method,
  curl.response.property_module;

type
  TModuleRequest = class(TPropertyModule)
  protected
    { Get size of sent request. }
    function GetLength : TDataSize;

    { Get the last used HTTP method. }
    function GetMethod : TMethod;

    { Get the last used URL. }
    function GetUrl : String;
  public
    { Get size of sent request.
      The total size of the issued requests. }
    property Length : TDataSize read GetLength;

    { Get the last used HTTP method. }
    property Method : TMethod read GetMethod;

    { Get the last used URL. }
    property Url : String read GetUrl;
  end;

implementation

{ TModuleRequest }

function TModuleRequest.GetLength : TDataSize;
begin
  Result := TDataSize.Create;
  Result.Bytes := GetLongintValue(CURLINFO_REQUEST_SIZE);
end;

function TModuleRequest.GetMethod : TMethod;
var
  method_str : String;
begin
  method_str := GetStringValue(CURLINFO_EFFECTIVE_METHOD);

  if UpperCase(method_str) = 'GET' then
    Result := TMethod.GET
  else if UpperCase(method_str) = 'HEAD' then
    Result := TMethod.HEAD
  else if UpperCase(method_str) = 'POST' then
    Result := TMethod.POST
  else if UpperCase(method_str) = 'PUT' then
    Result := TMethod.PUT
  else if UpperCase(method_str) = 'DELETE' then
    Result := TMethod.DELETE
  else if UpperCase(method_str) = 'CONNECT' then
    Result := TMethod.CONNECT
  else if UpperCase(method_str) = 'OPTIONS' then
    Result := TMethod.OPTIONS
  else if UpperCase(method_str) = 'TRACE' then
    Result := TMethod.TRACE
  else if UpperCase(method_str) = 'PATCH' then
    Result := TMethod.PATCH
  else
    Result := TMethod.CUSTOM;
end;

function TModuleRequest.GetUrl : String;
begin
  Result := GetStringValue(CURLINFO_EFFECTIVE_URL);
end;

end.

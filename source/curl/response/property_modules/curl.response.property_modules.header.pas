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

unit curl.response.property_modules.header;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, utils.datasize, curl.response.property_module;

type
  TModuleHeader = class(TPropertyModule)
  protected
    { Get the response code. }
    function GetResponseCode : Longint;

    { Get size of retrieved headers. }
    function GetLength : TDataSize;

    { Get Content-Type. }
    function GetContentType : String;
  protected
    { Get the response code. 
      The stored value will be zero if no server response code has been 
      received. }
    property ResponseCode : Longint read GetResponseCode;

    { Get size of retrieved headers. 
      Measured in number of bytes. }
    property Length : TDataSize read GetLength;

    { Get Content-Type.
      This is the value read from the Content-Type: field. If you get NULL, it 
      means that the server didn't send a valid Content-Type header or that the 
      protocol used doesn't support this. }
    property ContentType : String read GetContentType;
  end;

implementation

{ TModuleHeader }

function TModuleHeader.GetResponseCode : Longint;
begin
  Result := GetLongintValue(CURLINFO_RESPONSE_CODE);
end;

function TModuleHeader.GetLength : TDataSize;
begin
  Result := TDataSize.Create;
  Result.Bytes := GetLongintValue(CURLINFO_HEADER_SIZE);
end;

function TModuleHeader.GetContentType : String;
begin
  Result := GetStringValue(CURLINFO_CONTENT_TYPE);
end;

end.
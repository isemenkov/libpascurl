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

unit curl.response.property_module;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.utils.errors_stack;

type
  { Base class for all curl.response property modules classes. }
  TPropertyModule = class
  public
    { Property module constructor. }
    constructor Create (ACURL : libpascurl.CURL; AErrorsStack : PErrorsStack);
  protected
    { CURL library handle. }
    FCURL : libpascurl.CURL;

    { Errors messages stores. } 
    FErrorsStack : PErrorsStack;
  
    { Set CURL library option. }
    function GetBooleanValue (ACURLInfo : CURLINFO) : Boolean;
    function GetLongintValue (ACURLInfo : CURLINFO) : Longint;
    //function GetInt64Value (ACURLInfo : CURLINFO): Int64;
    function GetStringValue (ACURLInfo : CURLINFO) : String;
    //function GetPointerValue (ACURLInfo : CURLINFO) : Pointer;
  end;

implementation

{ TPropertyModule }

constructor TPropertyModule.Create (ACURL : libpascurl.CURL; AErrorsStack :
  PErrorsStack);
begin
  FCURL := ACURL;
  FErrorsStack := AErrorsStack;
end;

function TPropertyModule.GetLongintValue (ACURLInfo : CURLINFO) : Longint;
begin
  FErrorsStack^.Push(curl_easy_getinfo(FCURL, ACURLInfo, @Result));
end;

function TPropertyModule.GetBooleanValue (ACURLInfo : CURLINFO) : Boolean;
var
  res : Longint;
begin
  FErrorsStack^.Push(curl_easy_getinfo(FCURL, ACURLInfo, @res));
  Result := Boolean(res);
end;

function TPropertyModule.GetStringValue (ACURLInfo : CURLINFO) : String;
var
  res : PChar;
begin
  New(res);
  res := '';  
  FErrorsStack^.Push(curl_easy_getinfo(FCURL, ACURLInfo, @res));
  Result := res;
end;

end.
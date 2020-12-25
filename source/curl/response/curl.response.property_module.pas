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

unit curl.response.property_module;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.utils.errors_stack, math, container.memorybuffer;

type
  { Base class for all curl.response property modules classes. }
  TPropertyModule = class
  public
    type
      PMemoryBuffer = ^TMemoryBuffer;
  public
    { Property module constructor. }
    constructor Create (ACURL : libpascurl.CURL; AErrorsStack : PErrorsStack);
  protected
    { CURL library handle. }
    FCURL : libpascurl.CURL;

    { Errors messages stores. } 
    FErrorsStack : PErrorsStack;
  
    { Set CURL library option. }
    function GetBooleanValue (ACURLInfo : CURLINFO) : Boolean; overload;
    function GetLongintValue (ACURLInfo : CURLINFO) : Longint; overload;
    function GetInt64Value (AOldProp : CURLINFO; ANewProp : CURLINFO): Int64;
      overload;
    function GetStringValue (ACURLInfo : CURLINFO) : String; overload;
    function GetListValue (ACURLInfo : CURLINFO) : pcurl_slist; overload;
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
var
  res : Longint;
begin
  res := 0;
  FErrorsStack^.Push(curl_easy_getinfo(FCURL, ACURLInfo, @Res));
  Result := res;
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

function TPropertyModule.GetInt64Value (AOldProp : CURLINFO; ANewProp : 
  CURLINFO) : Int64;

  function ResultValue (AValue : Longint) : Int64;
  begin
    if AValue >= 0 then
    begin
      Exit(AValue);
    end;
    Result := 0;
  end;

var
  size : curl_off_t;
  dsize : Double;
  curl_res : CURLcode;
begin
  size := 0;
  dsize := 0;

  curl_res := curl_easy_getinfo(FCURL, ANewProp, @size);
  Result := ResultValue(size);

  if curl_res <> CURLE_OK then
  begin
    curl_res := curl_easy_getinfo(FCURL, AOldProp, @dsize);
    Result := ResultValue(ceil(dsize));
  end;

  FErrorsStack^.Push(curl_res);
end;

function TPropertyModule.GetListValue (ACURLInfo : CURLINFO) : pcurl_slist;
var
  list : pcurl_slist;
begin
  FErrorsStack^.Push(curl_easy_getinfo(FCURL, ACURLInfo, @list));
  Result := list;
end;

end.

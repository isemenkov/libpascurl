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

unit curl.session.property_modules.header;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.utils.headers_list, curl.utils.errors_stack,
  curl.session.property_module;

type
  TModuleHeader = class(TPropertyModule)
  public
    type
      PHeadersList = ^THeadersList;

      { Set callback that receives header data. }
      THeaderCallbackFunction = function (ABuffer : PByte; ASize : LongWord) :
        LongWord of object;
  public  
    constructor Create (ACURL : libpascurl.CURL; AErrorsStack : PErrorsStack;
      AHeadersList : PHeadersList);
  protected
    FHeaders : PHeadersList;
    FHeaderCallbackFunction : THeaderCallbackFunction;
  protected
    { Set callback that receives header data.
      This function gets called by libcurl as soon as it has received header 
      data. The header callback will be called once for each header and only 
      complete header lines are passed on to the callback. Parsing headers is 
      very easy using this. }
    property HeaderCallback : THeaderCallbackFunction 
      read FHeaderCallbackFunction write FHeaderCallbackFunction;
  private
    class function HeaderFunctionCallback (ABuffer : PByte; ASize : LongWord;
      ANitems : LongWord; AData : Pointer) : LongWord; static; cdecl;
    function HeaderFunction (ABuffer : PByte; ASize : LongWord) : LongWord;
  end;    

implementation

{ TModuleHeader }

class function TModuleHeader.HeaderFunctionCallback (ABuffer : PByte; ASize :
  LongWord; ANitems : LongWord; AData : Pointer) : LongWord; cdecl;
begin
  if Assigned(TModuleHeader(AData).FHeaderCallbackFunction) then
  begin
    TModuleHeader(AData).HeaderFunction(ABuffer, ASize * ANitems);
    Result := TModuleHeader(AData).FHeaderCallbackFunction(ABuffer,
      ASize * ANitems);
  end else
  begin
    Result := TModuleHeader(AData).HeaderFunction(ABuffer, ASize * ANitems);
  end;
end;

constructor TModuleHeader.Create (ACURL : libpascurl.CURL; AErrorsStack :
  PErrorsStack; AHeadersList : PHeadersList);
begin
  inherited Create(ACURL, AErrorsStack);
  FHeaders := AHeadersList;

  Option(CURLOPT_HEADERDATA, Pointer(Self));
  Option(CURLOPT_HEADERFUNCTION, @TModuleHeader.HeaderFunctionCallback);
end;

function TModuleHeader.HeaderFunction (ABuffer : PByte; ASize : LongWord) :
  LongWord;
var
  Index : Integer;
  Letter : Char;
  HeaderStr : String;
begin
  HeaderStr := '';

  for index := 0 to ASize do
  begin
    if (ABuffer[index] >= 32) and (ABuffer[index] <= 127) then
    begin
      Letter := Chr(ABuffer[index]);
      HeaderStr := HeaderStr + Letter;
    end;
  end;

  if HeaderStr <> '' then
    FHeaders^.Append(HeaderStr);
  Result := ASize;
end;

end.

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

unit curl.session.writer;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.utils.errorsstack, container.arraylist, 
  container.memorybuffer;

type
  TWriter = class
  public
    type
      { Set callback for writing received data. }
      TDownloadFunction = function (ABuffer : PChar; ASize : LongWord) : 
        LongWord  of object;
  protected
    FCURL : CURL;
    FErrorsStack : PErrorsStack;
    FBuffer : TMemoryBuffer;
    FDownloadFunction : TDownloadFunction;
  private
    class function DownloadFunctionCallback (APtr : PChar; ASize : LongWord;
      ANmemb : LongWord; AData : Pointer) : LongWord; static; cdecl;
    function DownloadFunction (APtr : PChar; ASize : LongWord) : LongWord;

    function GetDownloadBufferData : Pointer;
    function GetDownloadBufferDataSize : Cardinal;
  protected
    constructor Create (ACURL : CURL; AErrorsStack : PErrorsStack);

    { Get download data buffer pointer. } 
    property DownloadData : Pointer read GetDownloadBufferData;

    { Get download data size. }
    property DownloadDataSize : Cardinal read GetDownloadBufferDataSize;

    { Set callback for writing received data. 
      This callback function gets called by libcurl as soon as there is data 
      received that needs to be saved. For most transfers, this callback gets 
      called many times and each invoke delivers another chunk of data. ptr 
      points to the delivered data, and the size of that data is nmemb; size is 
      always 1. }
    property DownloadCallback : TDownloadFunction read FDownloadFunction
      write FDownloadFunction;
  end;

implementation

constructor TWriter.Create (ACURL : CURL; AErrorsStack : PErrorsStack);
begin
  FCURL := ACURL;
  FErrorsStack := AErrorsStack;
  FBuffer := TMemoryBuffer.Create;
  FErrorsStack^.Push(curl_easy_setopt(FCURL, CURLOPT_WRITEDATA, Pointer(Self)));
  FErrorsStack^.Push(curl_easy_setopt(FCURL, CURLOPT_WRITEFUNCTION,
    @TWriter.DownloadFunctionCallback));  
end;

class function TWriter.DownloadFunctionCallback (APtr : PChar; ASize : LongWord;
  ANmemb : LongWord; AData : Pointer) : LongWord; cdecl;
begin
  if Assigned(TWriter(AData).FDownloadFunction) then
  begin
    Result := TWriter(AData).FDownloadFunction(APtr, ASize * ANmemb);
  end else
  begin
    Result := TWriter(AData).DownloadFunction(APtr, ASize * ANmemb);
  end;
end;

function TWriter.DownloadFunction (APtr : PChar; ASize : LongWord) : LongWord;
var
  size : Cardinal;
begin
  size := FBuffer.GetBufferDataSize;
  Move(APtr^, FBuffer.GetAppendBuffer(ASize)^, ASize);
  FBuffer.SetBufferDataSize(size + ASize);
  Result := ASize;
end;

function TWriter.GetDownloadBufferData : Pointer;
begin
  Result := FBuffer.GetBufferData;
end;

function TWriter.GetDownloadBufferDataSize : Cardinal;
begin
  Result := FBuffer.GetBufferDataSize;
end;

end.
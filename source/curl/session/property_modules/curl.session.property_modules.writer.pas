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

unit curl.session.property_modules.writer;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, curl.utils.errors_stack, container.memorybuffer,
  utils.datasize, curl.session.property_module;

type
  TModuleWriter = class(TPropertyModule)
  public
    type
      { Set callback for writing received data. }
      TDownloadFunction = function (ABuffer : PChar; ASize : LongWord) : 
        LongWord  of object;
  public
    constructor Create (ACURL : libpascurl.CURL; AErrorsStack : PErrorsStack;
      ABuffer : PMemoryBuffer);
  public
    const
      MIN_BUFFER_SIZE                                      = 1024;
      MAX_BUFFER_SIZE                                      = CURL_MAX_READ_SIZE;
  protected
    FBuffer : PMemoryBuffer;
    FDownloadFunction : TDownloadFunction;

    { Set preferred receive buffer size. }
    procedure SetBufferSize (ASize : TDataSize);
  protected
    { Set callback for writing received data. 
      This callback function gets called by libcurl as soon as there is data 
      received that needs to be saved. For most transfers, this callback gets 
      called many times and each invoke delivers another chunk of data. }
    property DownloadCallback : TDownloadFunction read FDownloadFunction
      write FDownloadFunction;

    { Set preferred receive buffer size.
      Specifying your preferred size (in bytes) for the receive buffer in 
      libcurl. The maximum buffer size allowed to be set is MAX_BUFFER_SIZE. }
    property BufferSize : TDataSize write SetBufferSize;

  private
    class function DownloadFunctionCallback (APtr : PChar; ASize : LongWord;
      ANmemb : LongWord; AData : Pointer) : LongWord; static; cdecl;
    function DownloadFunction (APtr : PChar; ASize : LongWord) : LongWord;
  end;

implementation

{ TModuleWriter }

class function TModuleWriter.DownloadFunctionCallback (APtr : PChar; ASize : 
  LongWord; ANmemb : LongWord; AData : Pointer) : LongWord; cdecl;
begin
  if Assigned(TModuleWriter(AData).FDownloadFunction) then
  begin
    Result := TModuleWriter(AData).FDownloadFunction(APtr, ASize * ANmemb);
  end else
  begin
    Result := TModuleWriter(AData).DownloadFunction(APtr, ASize * ANmemb);
  end;
end;

constructor TModuleWriter.Create (ACURL : libpascurl.CURL; AErrorsStack :
  PErrorsStack; ABuffer : PMemoryBuffer);
begin
  inherited Create(ACURL, AErrorsStack);
  FBuffer := ABuffer;

  Option(CURLOPT_WRITEDATA, Pointer(Self));
  Option(CURLOPT_WRITEFUNCTION, @TModuleWriter.DownloadFunctionCallback);
end;

function TModuleWriter.DownloadFunction (APtr : PChar; ASize : LongWord) : 
  LongWord;
var
  size : Cardinal;
begin
  size := FBuffer^.GetBufferDataSize;
  Move(APtr^, FBuffer^.GetAppendBuffer(ASize)^, ASize);
  FBuffer^.SetBufferDataSize(size + ASize);
  Result := ASize;
end;

procedure TModuleWriter.SetBufferSize (ASize : TDataSize);
begin
  if ASize.Bytes < MIN_BUFFER_SIZE then
  begin
    ASize.Bytes := MIN_BUFFER_SIZE;
  end else if ASize.Bytes > MAX_BUFFER_SIZE then
  begin
    ASize.Bytes := MAX_BUFFER_SIZE;
  end;

  Option(CURLOPT_BUFFERSIZE, Longint(ASize.Bytes));
end;

end.

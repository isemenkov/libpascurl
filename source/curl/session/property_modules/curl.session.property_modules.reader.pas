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

unit curl.session.property_modules.reader;

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
  TModuleReader = class(TPropertyModule)
  public
    type
      { Seek callback result. }
      TSeekResult = (
        SEEK_OK                                                         = 0,
        SEEK_FAIL,
        SEEK_CANTSEEK
      );

      { Seek position. }
      TSeekPosition = (
        FROM_BEGIN                                                      = 0,
        FROM_CURRENT,
        FROM_END
      );

      { Set callback for reading uploading data. }
      TUploadFunction = function (ABuffer : PChar; ASize : LongWord) :
        LongWord of object;

      { Set callback for seek uploading data buffer. }
      TSeekFunction = function (AOffset : LongWord; APosition : TSeekPosition) : 
        TSeekResult;
  public
    constructor Create (ACURL : libpascurl.CURL; AErrorsStack : PErrorsStack;
        ABuffer : PMemoryBuffer);

    { Set upload options. }
    procedure InitUpload;
  public
    const
      MIN_BUFFER_SIZE                                      = 1024;
      MAX_BUFFER_SIZE                                      = CURL_MAX_READ_SIZE;
  protected
    FBuffer : PMemoryBuffer; 
    FBufferOffset : LongWord;
    FUploadFunction : TUploadFunction;
    FSeekFunction : TSeekFunction;

    { Set preferred receive buffer size. }
    procedure SetBufferSize (ASize : TDataSize);
  protected
    { Set preferred receive buffer size.
      Specifying your preferred size (in bytes) for the receive buffer in 
      libcurl. The maximum buffer size allowed to be set is MAX_BUFFER_SIZE. }
    property BufferSize : TDataSize write SetBufferSize;  

    { Read callback for data uploads.
      This callback function gets called by libcurl as soon as it needs to read 
      data in order to send it to the peer - like if you ask it to upload or 
      post data to the server. The data area pointed at by the pointer buffer 
      should be filled up with at most size multiplied with nitems number of 
      bytes by your function. }
    property UploadCallback : TUploadFunction read FUploadFunction
      write FUploadFunction;

    { Callback for seeking in input stream.
      This function gets called by libcurl to seek to a certain position in the 
      input stream and can be used to fast forward a file in a resumed upload 
      (instead of reading all uploaded bytes with the normal read 
      function/callback). It is also called to rewind a stream when data has 
      already been sent to the server and needs to be sent again. This may 
      happen when doing an HTTP PUT or POST with a multi-pass authentication 
      method, or when an existing HTTP connection is reused too late and the 
      server closes the connection. }
    property SeekCallback : TSeekFunction read FSeekFunction
      write FSeekFunction;
  private
    class function UploadFunctionCallback (APtr : PChar; ASize : LongWord;
        ANmemb : LongWord; AData : Pointer) : LongWord; static; cdecl;
    class function SeekFunctionCallback (APtr : Pointer; AOffset : LongWord;
      AOrigin : Integer) : Integer; static; cdecl;
    function UploadFunction (APtr : PChar; ASize : LongWord) : LongWord; 
    function SeekFunction (AOffset : LongWord; APosition : TSeekPosition) : 
      TSeekResult;
  end;

implementation

{ TModuleReader }

class function TModuleReader.UploadFunctionCallback (APtr : PChar; ASize : 
    LongWord; ANmemb : LongWord; AData : Pointer) : LongWord; cdecl;
begin
  if Assigned(TModuleReader(AData).FUploadFunction) then
  begin
    Result := TModuleReader(AData).FUploadFunction(APtr, ASize * ANmemb);
  end else
  begin
    Result := TModuleReader(AData).UploadFunction(APtr, ASize * ANmemb);
  end;
end;

class function TModuleReader.SeekFunctionCallback (APtr : Pointer; AOffset :
  LongWord; AOrigin : Integer) : Integer; cdecl;
begin
  if Assigned(TModuleReader(APtr).FSeekFunction) then
  begin
    Result := Integer(TModuleReader(APtr).FSeekFunction(AOffset, 
      TSeekPosition(AOrigin)));
  end else 
  begin
    Result := Integer(TModuleReader(APtr).SeekFunction(AOffset, 
      TSeekPosition(AOrigin)));
  end;
end;

constructor TModuleReader.Create (ACURL : libpascurl.CURL; AErrorsStack :
    PErrorsStack; ABuffer : PMemoryBuffer);
begin
  inherited Create(ACURL, AErrorsStack);
  FBuffer := ABuffer;
  FBufferOffset := 0;

  Option(CURLOPT_UPLOAD, True);
  Option(CURLOPT_READDATA, Pointer(Self));
  Option(CURLOPT_READFUNCTION, @TModuleReader.UploadFunctionCallback);
  Option(CURLOPT_SEEKDATA, Pointer(Self));
  Option(CURLOPT_SEEKFUNCTION, @TModuleReader.SeekFunctionCallback);
end;

function TModuleReader.UploadFunction (APtr : PChar; ASize : LongWord) :
    LongWord;
var
  size : LongWord;
  dataptr : Pointer;
begin
  size := FBuffer^.GetBufferDataSize - FBufferOffset;
  if size > 0 then
  begin
    dataptr := FBuffer^.GetBufferData;
    Move(dataptr, APtr, size);
  end;
  Result := size;
end;

function TModuleReader.SeekFunction (AOffset : LongWord; APosition : 
  TSeekPosition) : TSeekResult;
begin
  case APosition of
    FROM_BEGIN :   begin FBufferOffset := AOffset; end;
    FROM_CURRENT : begin Inc(FBufferOffset, AOffset); end;
    FROM_END :     begin FBufferOffset := FBuffer^.GetBufferDataSize - AOffset;
      end;
  end;
  Result := SEEK_OK;
end;

procedure TModuleReader.SetBufferSize (ASize : TDataSize);
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

procedure TModuleReader.InitUpload;
begin
  Option(CURLOPT_INFILESIZE_LARGE, Int64(FBuffer^.GetBufferDataSize));
end;

end.

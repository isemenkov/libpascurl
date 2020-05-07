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
(* Module:          Unit 'pascurl'                                            *)
(* Functionality:                                                             *)
(*                                                                            *)
(*                                                                            *)
(*                                                                            *)
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

unit http;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils, libpascurl, result, timeinterval, datasize, errorstack;

type
  { Simple request to get data by http(s) protocol }
  THTTPPlainRequest = class
  public
    const
      DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) ' +
                           'AppleWebKit/537.36 (KHTML, like Gecko)  '   +
                           'Chrome/71.0.3578.98 Safari/537.36';

    type
      THTTPPlainErrors = (
        ERROR_NONE,
        ERROR_SOMETHING_WRONG
      );

      THTTPPlainResult = specialize TResult<String, THTTPPlainErrors>;
  public
    constructor Create;
    constructor Create (AURL : String);
    destructor Destroy; override;

    function URL (AURL : String) : THTTPPlainRequest;
    function Port (APort : Word) : THTTPPlainRequest;
    function UserAgent (AAgent : String) : THTTPPlainRequest;
    function FollowRedirect (AFollow : Boolean = True) : THTTPPlainRequest;

    function Content : THTTPPlainResult;
    function ErrorMessage : THTTPPlainResult;

    function EffectiveUrl : THTTPPlainResult;
    function ContentType : THTTPPlainResult;
  private
    class function WriteFunctionCallback (ptr : PChar; size : LongWord; nmemb :
      LongWord; data : Pointer) : LongWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Write (ptr : PChar; size : LongWord; nmemb : LongWord) : LongWord;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    function HasInfo : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    FHandle : CURL;
    FErrorStack : TErrorStack;
    FBuffer : TMemoryStream;
    FErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
    FHasInfo : Boolean;
  end;

implementation

{ THHTPPlainRequest }

class function THTTPPlainRequest.WriteFunctionCallback (ptr : PChar; size :
  LongWord; nmemb : LongWord; data : Pointer) : LongWord;
begin
  Result := THTTPPlainRequest(data).Write(ptr, size, nmemb);
end;

function THTTPPlainRequest.Write (ptr : PChar; size : LongWord; nmemb :
  LongWord) : LongWord;
begin
  Result := FBuffer.Write(ptr^, size * nmemb);
end;

constructor THTTPPlainRequest.Create;
begin
  FHandle := curl_easy_init;
  FErrorStack := TErrorStack.Create;
  FBuffer := TMemoryStream.Create;
  FHasInfo := False;

  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_READDATA, Pointer(Self)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_READFUNCTION,
    @THTTPPlainRequest.WriteFunctionCallback));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_FOLLOWLOCATION,
    Longint(True)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_USERAGENT,
    PChar(DEFAULT_USER_AGENT)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_ERRORBUFFER,
    PChar(FErrorBuffer)));
end;

constructor THTTPPlainRequest.Create (AURL : String);
begin
  FHandle := curl_easy_init;
  FErrorStack := TErrorStack.Create;
  FBuffer := TMemoryStream.Create;
  FHasInfo := False;

  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_READDATA, Pointer(Self)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_READFUNCTION,
    @THTTPPlainRequest.WriteFunctionCallback));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_FOLLOWLOCATION,
    Longint(True)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_URL, PChar(AURL)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_USERAGENT,
    PChar(DEFAULT_USER_AGENT)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_ERRORBUFFER,
    PChar(FErrorBuffer)));
end;

destructor THTTPPlainRequest.Destroy;
begin
  curl_easy_cleanup(FHandle);
  FreeAndNil(FBuffer);
  FreeAndNil(FErrorStack);
end;

function THTTPPlainRequest.URL (AURL : String) : THTTPPlainRequest;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_URL, PChar(AURL)));
  Result := Self;
end;

function THTTPPlainRequest.Port (APort : Word) : THTTPPlainRequest;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_LOCALPORT,
    Longint(APort)));
  Result := Self;
end;

function THTTPPlainRequest.UserAgent (AAgent : String) : THTTPPlainRequest;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_USERAGENT, PChar(AAgent)));
  Result := Self;
end;

function THTTPPlainRequest.FollowRedirect (AFollow : Boolean) :
  THTTPPlainRequest;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_FOLLOWLOCATION,
    Longint(AFollow)));
  Result := Self;
end;

function THTTPPlainRequest.Content : THTTPPlainResult;
var
  Stream : TStringStream;
begin
  if FHandle <> nil then
  begin
    Stream := TStringStream.Create('');
    Stream.Write(FBuffer.Memory^, FBuffer.Size);
    Result := THTTPPlainResult.Create(Stream.DataString, ERROR_NONE, True);
    FreeAndNil(Stream);
  end else
  begin
    Result := THTTPPlainResult.Create('', ERROR_SOMETHING_WRONG, False);
  end;
end;

function THTTPPlainRequest.HasInfo : Boolean;
begin
  if (FHandle <> nil) and (not FHasInfo) then
  begin
    FHasInfo := (curl_easy_perform(FHandle) = CURLE_OK);
    Result := FHasInfo;
    Exit;
  end;

  Result := FHasInfo;
end;

function THTTPPlainRequest.ErrorMessage : THTTPPlainResult;
begin
  if not FHasInfo then
  begin
    Result := THTTPPlainResult.Create(String(FErrorBuffer),
      ERROR_SOMETHING_WRONG, True);
    Exit;
  end;

  Result := THTTPPlainResult.Create('', ERROR_NONE, False);
end;

function THTTPPlainRequest.EffectiveUrl : THTTPPlainResult;
var
  url : PChar;
begin
  if not FHasInfo then
  begin
    Result := THTTPPlainResult.Create('', ERROR_SOMETHING_WRONG, False);
    Exit;
  end;

  New(url);
  url := '';
  curl_easy_getinfo(FHandle, CURLINFO_EFFECTIVE_URL, @url);
  Result := THTTPPlainResult.Create(String(url), ERROR_NONE, True);
end;

function THTTPPlainRequest.ContentType : THTTPPlainResult;
var
  content_type : PChar;
begin
  if not FHasInfo then
  begin
    Result := THTTPPlainResult.Create('', ERROR_SOMETHING_WRONG, False);
    Exit;
  end;

  New(content_type);
  content_type := '';
  curl_easy_getinfo(FHandle, CURLINFO_CONTENT_TYPE, @content_type);
  Result := THTTPPlainResult.Create(String(content_type), ERROR_NONE, True);
end;

end.


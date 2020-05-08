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
  THTTPRequestPlain = class;
  THTTPSessionPlain = class
  public
    const
      DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) ' +
                           'AppleWebKit/537.36 (KHTML, like Gecko)  '   +
                           'Chrome/71.0.3578.98 Safari/537.36';

    type
      THTTPErrors = (
        ERROR_NONE,
        ERROR_SOMETHING_WRONG
      );

      THTTPRequestResult = specialize TResult<THTTPRequestPlain, THTTPErrors>;
  public
    constructor Create;
    constructor Create (AURL : String);
    destructor Destroy; override;

    function URL (AURL : String) : THTTPSessionPlain;
    function Port (APort : Word) : THTTPSessionPlain;
    function UserAgent (AAgent : String = DEFAULT_USER_AGENT) :
      THTTPSessionPlain;
    function FollowRedirect (AFollow : Boolean = True) : THTTPSessionPlain;
    function Get : THTTPRequestResult;
  private
    FHandle : CURL;
    FErrorStack : TErrorStack;
  end;

  THTTPRequestPlain = class
  public
    constructor Create (AHandle : CURL; AErrorStack : PErrorStack);
    destructor Destroy; override;

    function Content : String;
    function ErrorMessage : String;
    function EffectiveUrl : String;
    function ContentType : String;
  private
    class function WriteFunctionCallback (ptr : PChar; size : LongWord; nmemb :
      LongWord; data : Pointer) : LongWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Write (ptr : PChar; size : LongWord; nmemb : LongWord) : LongWord;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    FErrorStack : TErrorStack;
    FHandle : CURL;
    FBuffer : TMemoryStream;
    FErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
  end;

implementation

{ THHTPSessionPlain }

constructor THTTPSessionPlain.Create;
begin
  FHandle := curl_easy_init;
  FErrorStack := TErrorStack.Create;

  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_FOLLOWLOCATION,
    Longint(True)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_USERAGENT,
    PChar(DEFAULT_USER_AGENT)));
end;

constructor THTTPSessionPlain.Create (AURL : String);
begin
  FHandle := curl_easy_init;
  FErrorStack := TErrorStack.Create;

  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_FOLLOWLOCATION,
    Longint(True)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_URL, PChar(AURL)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_USERAGENT,
    PChar(DEFAULT_USER_AGENT)));
end;

destructor THTTPSessionPlain.Destroy;
begin
  curl_easy_cleanup(FHandle);
  FreeAndNil(FErrorStack);

  inherited Destroy;
end;

function THTTPSessionPlain.URL (AURL : String) : THTTPSessionPlain;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_URL, PChar(AURL)));
  Result := Self;
end;

function THTTPSessionPlain.Port (APort : Word) : THTTPSessionPlain;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_LOCALPORT,
    Longint(APort)));
  Result := Self;
end;

function THTTPSessionPlain.UserAgent (AAgent : String) : THTTPSessionPlain;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_USERAGENT, PChar(AAgent)));
  Result := Self;
end;

function THTTPSessionPlain.FollowRedirect (AFollow : Boolean) :
  THTTPSessionPlain;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_FOLLOWLOCATION,
    Longint(AFollow)));
  Result := Self;
end;

function THTTPSessionPlain.Get : THTTPRequestResult;
var
  Handle : CURL;
  Request : THTTPRequestPlain;
  Curl_Ok : Boolean;
begin
  Handle := curl_easy_duphandle(FHandle);
  Request := THTTPRequestPlain.Create(Handle, @FErrorStack);
  Curl_Ok := curl_easy_perform(Handle) = CURLE_OK;
  Result := THTTPRequestResult.Create(Request, ERROR_SOMETHING_WRONG, Curl_Ok);
end;

{ THTTPRequestPlain }

class function THTTPRequestPlain.WriteFunctionCallback (ptr : PChar; size :
  LongWord; nmemb : LongWord; data : Pointer) : LongWord;
begin
  Result := THTTPRequestPlain(data).Write(ptr, size, nmemb);
end;

function THTTPRequestPlain.Write (ptr : PChar; size : LongWord; nmemb :
  LongWord) : LongWord;
begin
  Result := FBuffer.Write(ptr^, size * nmemb);
end;

constructor THTTPRequestPlain.Create (AHandle : CURL; AErrorStack :
  PErrorStack);
begin
  FHandle := AHandle;
  FErrorStack := TErrorStack.Create;
  FErrorStack := AErrorStack^;

  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_READDATA, Pointer(Self)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_READFUNCTION,
    @THTTPRequestPlain.WriteFunctionCallback));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_ERRORBUFFER,
    PChar(FErrorBuffer)));
end;

destructor THTTPRequestPlain.Destroy;
begin
  curl_easy_cleanup(FHandle);
  FreeAndNil(FErrorStack);
  FreeAndNil(FBuffer);

  inherited Destroy;
end;

function THTTPRequestPlain.Content : String;
var
  Stream : TStringStream;
begin
  Stream := TStringStream.Create('');
  Stream.Write(FBuffer.Memory^, FBuffer.Size);
  Result := Stream.DataString;
  FreeAndNil(Stream);
end;

function THTTPRequestPlain.ErrorMessage : String;
begin
  Result := String(FErrorBuffer);
end;

function THTTPRequestPlain.EffectiveUrl : String;
var
  url : PChar;
begin
  New(url);
  url := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_EFFECTIVE_URL, @url));
  Result := String(url);
end;

function THTTPRequestPlain.ContentType : String;
var
  content_type : PChar;
begin
  New(content_type);
  content_type := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_CONTENT_TYPE,
    @content_type));
  Result := String(content_type);
end;

end.


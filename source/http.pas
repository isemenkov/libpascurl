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
(* Functionality:   Provides THTTPSessionPlain and THTTPRequestPlain classes  *)
(*                  to get plain data by http(s) protocol.                    *)
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
  { HTTP(S) session result request data }
  THTTPRequest = class
  public
    type
      { Result errors code }
      THTTPErrors = (
        { All fine. Proceed as usual }
        ERROR_NONE,

        { The URL you passed to libcurl used a HTTP(S) protocol that this
          libcurl does not support. The support might be a compile-time option
          that you didn't use. }
        ERROR_UNSUPPORTED_PROTOCOL,

        { Very early initialization code failed. This is likely to be an
          internal error or problem, or a resource problem where something
          fundamental couldn't get done at init time.  }
        ERROR_FAILED_INIT,

        { The URL was not properly formatted.  }
        ERROR_URL_MALFORMAT,

        { A requested protocol was not found built-in in this libcurl due to a
          build-time decision. This means that a protocol was not enabled or
          explicitly disabled when libcurl was built and in order to get it to
          function you have to get a rebuilt libcurl. }
        ERROR_NOT_BUILT_IN,

        { Couldn't resolve proxy. The given proxy host could not be resolved. }
        ERROR_COULDNT_RESOLVE_PROXY,

        { Couldn't resolve host. The given remote host was not resolved. }
        ERROR_COULDNT_RESOLVE_HOST,

        { Failed to connect() to host or proxy. }
        ERROR_COULDNT_CONNECT,

        { A problem was detected in the HTTP2 framing layer. This is somewhat
          generic and can be one out of several problems, see the ErrorMessage
          for details. }
        ERROR_HTTP2,

        { This is returned if the HTTP server returns an error code that
          is >= 400. }
        ERROR_HTTP,

        { An error occurred when writing received data to a local file, or an
          error was returned to libcurl from a write callback. }
        ERROR_WRITE,

        { A memory allocation request failed. This is serious badness and things
          are severely screwed up if this ever occurs. }
        ERROR_OUT_OF_MEMORY,

        { Operation timeout. The specified time-out period was reached according
          to the conditions. }
        ERROR_OPERATION_TIMEDOUT,

        { A problem occurred somewhere in the SSL/TLS handshake. You really want
          the ErrorMessage and read the message there as it pinpoints the
          problem slightly more. Could be certificates (file formats, paths,
          permissions), passwords, and others. }
        ERROR_SSL_CONNECT,

        { Too many redirects. When following redirects, libcurl hit the maximum
          amount. Set your limit with CURLOPT_MAXREDIRS. }
        ERROR_TOO_MANY_REDIRECTS,

        { Nothing was returned from the server, and under the circumstances,
          getting nothing is considered an error. }
        ERROR_GOT_NOTHING,

        { The specified crypto engine wasn't found. }
        ERROR_SSL_ENGINE_NOTFOUND,

        { Failed setting the selected SSL crypto engine as default! }
        ERROR_SSL_ENGINE_SETFAILED,

        { Problem with the local client certificate. }
        ERROR_SSL_CERTPROBLEM,

        { Couldn't use specified cipher. }
        ERROR_SSL_CIPHER,

        { The remote server's SSL certificate or SSH md5 fingerprint was deemed
          not OK. }
        ERROR_PEER_FAILED_VERIFICATION,

        { Unrecognized transfer encoding. }
        ERROR_BAD_CONTENT_ENCODING,

        { Initiating the SSL Engine failed. }
        ERROR_SSL_ENGINE_INITFAILED,

        { Stream error in the HTTP/2 framing layer. }
        ERROR_HTTP2_STREAM,

        { A problem was detected in the HTTP/3 layer. This is somewhat generic
          and can be one out of several problems, see the ErrorMessage for
          details. }
        ERROR_HTTP3,

        { Something another goes wrong. Oppps! }
        ERROR_SOMETHING_WRONG
      );
  public
    destructor Destroy; override;

    { Get the response content plain data }
    function Content : String;

    { Return last error message or empty string if none }
    function ErrorMessage : String;

    { Get the last used URL

      Get the last used effective URL. In cases when you've asked libcurl to
      follow redirects, it may very well not be the same value you set. }
    function EffectiveUrl : String;

    { Get Content-Type

      This is the value read from the Content-Type: field. If you get empty,
      it means that the server didn't send a valid Content-Type header. }
    function ContentType : String;
  private
    {%H-}constructor Create (AHandle : CURL; AErrorStack : PErrorStack);

    { Callback for writting received data

      This callback function gets called by libcurl as soon as there is data
      received that needs to be saved. For most transfers, this callback gets
      called many times and each invoke delivers another chunk of data. ptr
      points to the delivered data, and the size of that data is nmemb; size is
      always 1. }
    class function WriteFunctionCallback (ptr : PChar; size : LongWord; nmemb :
      LongWord; data : Pointer) : LongWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Write (ptr : PChar; size : LongWord; nmemb : LongWord) : LongWord;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    class function CurlErrorToRequestError (ACode : CURLcode) : THTTPErrors;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    FHandle : CURL;
    FErrorStack : TErrorStack;
    FResultCode : CURLcode;
    FBuffer : TMemoryStream;
    FErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
  end;

  { Simple request to get data by HTTP(S) protocol }
  THTTPSessionPlain = class
  public
    const
      { Default used user agent }
      DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) ' +
                           'AppleWebKit/537.36 (KHTML, like Gecko)  '   +
                           'Chrome/71.0.3578.98 Safari/537.36';

    type
      { Request result type }
      THTTPRequestResult = specialize TResult<THTTPRequest,
        THTTPRequest.THTTPErrors>;
  public
    { Create new plain http session }
    constructor Create;

    { Create new plain http session with picked url }
    constructor Create (AURL : String);

    destructor Destroy; override;

    { Set the URL to use in the request

      The parameter should be a string which must be URL-encoded in the
      following format: scheme://host:port/path
      libcurl doesn't validate the syntax or use this variable until the
      transfer is issued.
      The host part of the URL contains the address of the server that you want
      to connect to. This can be the fully qualified domain name of the server,
      the local network name of the machine on your network or the IP address
      of the server or machine represented by either an IPv4 or IPv6 address.
      http://www.example.com/
      http://hostname/
      http://192.168.0.1/
      http://[2001:1890:1112:1::20]/
      It is also possible to specify the user name, password and any supported
      login options as part of the host, for the following protocols, when
      connecting to servers that require authentication:
      http://user:password@www.example.com }
    function URL (AURL : String) : THTTPSessionPlain;

    { Set remote port number to work with

      This option sets number to be the remote port number to connect to,
      instead of the one specified in the URL or the default port for the
      http(s) protocol.
      Usually, you just let the URL decide which port to use but this
      allows the application to override that. }
    function Port (APort : Word) : THTTPSessionPlain;

    { Set HTTP user-agent header

      It will be used to set the User-Agent: header in the HTTP request sent to
      the remote server. This can be used to fool servers or scripts. }
    function UserAgent (AAgent : String = DEFAULT_USER_AGENT) :
      THTTPSessionPlain;

    { Follow HTTP 3XXX redirects

      Tells the library to follow any Location: header that the server
      sends as part of an HTTP header in a 3xx response. The Location:
      header can specify a relative or an absolute URL to follow.
      libcurl will issue another request for the new URL and follow new
      Location: headers all the way until no more such headers are
      returned. }
    function FollowRedirect (AFollow : Boolean = True) : THTTPSessionPlain;

    { Ask for an HTTP GET request

      This forces the HTTP request to get back to using GET.
      When setting Get, it will automatically set NoBody to False and Upload to
      False. }
    function Get : THTTPRequestResult;
  private
    FHandle : CURL;
    FErrorStack : TErrorStack;
  end;

implementation

{ THTTPRequest }

class function THTTPRequest.CurlErrorToRequestError (ACode : CURLcode) :
  THTTPErrors;
begin
  case ACode of
    CURLE_OK :                         Result := ERROR_NONE;
    CURLE_UNSUPPORTED_PROTOCOL :       Result := ERROR_UNSUPPORTED_PROTOCOL;
    CURLE_FAILED_INIT :                Result := ERROR_FAILED_INIT;
    CURLE_URL_MALFORMAT :              Result := ERROR_URL_MALFORMAT;
    CURLE_NOT_BUILT_IN :               Result := ERROR_NOT_BUILT_IN;
    CURLE_COULDNT_RESOLVE_PROXY :      Result := ERROR_COULDNT_RESOLVE_PROXY;
    CURLE_COULDNT_RESOLVE_HOST :       Result := ERROR_COULDNT_RESOLVE_HOST;
    CURLE_COULDNT_CONNECT :            Result := ERROR_COULDNT_CONNECT;
    CURLE_HTTP2 :                      Result := ERROR_HTTP2;
    CURLE_HTTP_RETURNED_ERROR :        Result := ERROR_HTTP;
    CURLE_WRITE_ERROR :                Result := ERROR_WRITE;
    CURLE_OUT_OF_MEMORY :              Result := ERROR_OUT_OF_MEMORY;
    CURLE_OPERATION_TIMEDOUT :         Result := ERROR_OPERATION_TIMEDOUT;
    CURLE_SSL_CONNECT_ERROR :          Result := ERROR_SSL_CONNECT;
    CURLE_TOO_MANY_REDIRECTS :         Result := ERROR_TOO_MANY_REDIRECTS;
    CURLE_GOT_NOTHING :                Result := ERROR_GOT_NOTHING;
    CURLE_SSL_ENGINE_NOTFOUND :        Result := ERROR_SSL_ENGINE_NOTFOUND;
    CURLE_SSL_ENGINE_SETFAILED :       Result := ERROR_SSL_ENGINE_SETFAILED;
    CURLE_SSL_CERTPROBLEM :            Result := ERROR_SSL_CERTPROBLEM;
    CURLE_SSL_CIPHER :                 Result := ERROR_SSL_CIPHER;
    CURLE_PEER_FAILED_VERIFICATION :   Result := ERROR_PEER_FAILED_VERIFICATION;
    CURLE_BAD_CONTENT_ENCODING :       Result := ERROR_BAD_CONTENT_ENCODING;
    CURLE_SSL_ENGINE_INITFAILED :      Result := ERROR_SSL_ENGINE_INITFAILED;
    CURLE_HTTP2_STREAM :               Result := ERROR_HTTP2_STREAM;
    CURLE_HTTP3 :                      Result := ERROR_HTTP3;
    otherwise                          Result := ERROR_SOMETHING_WRONG
  end;
end;

class function THTTPRequest.WriteFunctionCallback (ptr : PChar; size :
  LongWord; nmemb : LongWord; data : Pointer) : LongWord;
begin
  Result := THTTPRequest(data).Write(ptr, size, nmemb);
end;

function THTTPRequest.Write (ptr : PChar; size : LongWord; nmemb :
  LongWord) : LongWord;
begin
  Result := FBuffer.Write(ptr^, size * nmemb);
end;

constructor THTTPRequest.Create (AHandle : CURL; AErrorStack :
  PErrorStack);
begin
  FHandle := AHandle;
  FErrorStack := TErrorStack.Create;
  FErrorStack := AErrorStack^;

  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_READDATA, Pointer(Self)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_READFUNCTION,
    @THTTPRequest.WriteFunctionCallback));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_ERRORBUFFER,
    PChar(FErrorBuffer)));
end;

destructor THTTPRequest.Destroy;
begin
  curl_easy_cleanup(FHandle);
  FreeAndNil(FErrorStack);
  FreeAndNil(FBuffer);

  inherited Destroy;
end;

function THTTPRequest.Content : String;
var
  Stream : TStringStream;
begin
  Stream := TStringStream.Create('');
  Stream.Write(FBuffer.Memory^, FBuffer.Size);
  Result := Stream.DataString;
  FreeAndNil(Stream);
end;

function THTTPRequest.ErrorMessage : String;
begin
  Result := String(FErrorBuffer);
end;

function THTTPRequest.EffectiveUrl : String;
var
  url : PChar;
begin
  New(url);
  url := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_EFFECTIVE_URL, @url));
  Result := String(url);
end;

function THTTPRequest.ContentType : String;
var
  content_type : PChar;
begin
  New(content_type);
  content_type := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_CONTENT_TYPE,
    @content_type));
  Result := String(content_type);
end;

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
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_PORT, Longint(APort)));
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
  Request : THTTPRequest;
begin
  Handle := curl_easy_duphandle(FHandle);
  Request := THTTPRequest.Create(Handle, @FErrorStack);
  Request.FResultCode := curl_easy_perform(Handle);
  Result := THTTPRequestResult.Create(Request,
    THTTPRequest.CurlErrorToRequestError(Request.FResultCode),
    Request.FResultCode = CURLE_OK);
end;

end.

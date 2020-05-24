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
  Classes, SysUtils, libpascurl, curlresult, timeinterval, datasize, errorstack,
  httpstatuscode;

type
  { HTTP(S) session result response data }
  THTTPResponse = class
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

      { HTTP protocol version }
      THTTPVersion = (
        HTTP_VERSION_UNKNOWN                                             = 0,

        { Enforce HTTP 1.0 requests. }
        HTTP_VERSION_1_0                       = Longint(CURL_HTTP_VERSION_1_0),

        { Enforce HTTP 1.1 requests. }
        HTTP_VERSION_1_1                       = Longint(CURL_HTTP_VERSION_1_1),

        { Attempt HTTP 2 requests. libcurl will fall back to HTTP 1.1 if
          HTTP 2 can't be negotiated with the server. }
        HTTP_VERSION_2_0                       = Longint(CURL_HTTP_VERSION_2_0),

        { Attempt HTTP 2 over TLS (HTTPS) only. libcurl will fall back to
          HTTP 1.1 if HTTP 2 can't be negotiated with the HTTPS server. For
          clear text HTTP servers, libcurl will use 1.1. }
        HTTP_VERSION_2TLS                     = Longint(CURL_HTTP_VERSION_2TLS),

        { Issue non-TLS HTTP requests using HTTP/2 without HTTP/1.1 Upgrade.
          It requires prior knowledge that the server supports HTTP/2
          straight away. HTTPS requests will still do HTTP/2 the standard way
          with negotiated protocol version in the TLS handshake. }
        HTTP_VERSION_2_PRIOR_KNOWLEDGE
                                 = Longint(CURL_HTTP_VERSION_2_PRIOR_KNOWLEDGE),

        { Setting this value will make libcurl attempt to use HTTP/3 directly
          to server given in the URL. Note that this cannot gracefully
          downgrade to earlier HTTP version if the server doesn't support
          HTTP/3. }
        HTTP_VERSION_3_0                         = Longint(CURL_HTTP_VERSION_3)
      );

      TError = class
      public
        constructor Create;
        function HasErrors : Boolean;
        function ErrorMessage : String;
        function OsErrno : Longint;
        function Errors : TErrorStack;
      end;
  public
    destructor Destroy; override;

    { Return TRUE if error is present }
    function HasErrors : Boolean;

    { Return last error message or empty string if none }
    function ErrorMessage : String;

    { Get errno number from last connect failure }
    function OsErrno : Longint;

    { Return all errors }
    function Errors : TErrorStack;

    { Get the last used URL

      Get the last used effective URL. In cases when you've asked libcurl to
      follow redirects, it may very well not be the same value you set. }
    function EffectiveUrl : String;

    { Get the URL a redirect would go to }
    function RedirectUrl : String;

    { Get Content-Type

      This is the value read from the Content-Type: field. If you get empty,
      it means that the server didn't send a valid Content-Type header. }
    function ContentType : String;

    { Get IP address of last connection  }
    function PrimaryIP : String;

    { Get local IP address of last connection }
    function LocalIP : String;

    { Get the last response code }
    function ResponseCode : THTTPStatusCode;

    { Get the response content plain data }
    function Content : String;

    { Get the result of the certificate verification  }
    function VerifySSLResult : Boolean;

    { Get the result of the proxy certificate verification

      This is only used for HTTPS proxies. }
    function VerifySSLProxyResult : Boolean;

    { Get the CONNECT response code
      Last received HTTP proxy response code to a CONNECT request. }
    function ConnectResponseCode : THTTPStatusCode;

    { Get the HTTP version used in the connection }
    function HTTPVersion : THTTPVersion;

    { Get the number of redirects }
    function RedirectCount : Longint;

    { Get the number of uploaded bytes }
    function Uploaded : TDataSize;

    { Get the number of downloaded bytes }
    function Downloaded : TDataSize;

    { Get upload speed per second }
    function UploadSpeed : TDataSize;

    { Get download speed per second }
    function DownloadSpeed : TDataSize;

    { Get size of retrieved headers }
    function HeaderSize : TDataSize;

    { Get size of sent request }
    function RequestSize : TDataSize;

    { Get content-length of download }
    function ContentLengthDownload : TDataSize;

    { Get the specified size of the upload }
    function ContentLengthUpload : TDataSize;

    { Get number of created connections }
    function NumConnects : Longint;

    { Get the latest destination port number }
    function PrimaryPort : Longint;

    { Get the latest local port number }
    function LocalPort : Longint;

    { Get total time of previous transfer }
    function TotalTime : TTimeInterval;

    { Get the name lookup time }
    function NameLookup : TTimeInterval;

    { Get the time until connect }
    function ConnectTime : TTimeInterval;

    { Get the time until the SSL/SSH handshake is completed
      When a redirect is followed, the time from each request is added
      together. }
    function AppConnectTime : TTimeInterval;

    { Get the time until the file transfer start
      When a redirect is followed, the time from each request is added
      together. }
    function PretransferTime : TTimeInterval;

    { Get time until the first byte is received
      When a redirect is followed, the time from each request is added
      together. }
    function StartTransferTime : TTimeInterval;

    { Get the time for all redirection steps
      When a redirect is followed, the time from each request is added
      together. }
    function RedirectTime : TTimeInterval;

    { Returns the Retry-After retry delay
      The information from the "Retry-After:" header. Returns zero delay if
      there was no header. }
    function RetryAfterDelay : TTimeInterval;

    { Get the last socket used

      If the socket is no longer valid, -1 is returned. When you finish working
      with the socket, you must call curl_easy_cleanup() as usual and let
      libcurl close the socket and cleanup other resources associated with the
      handle. }
    function LastSocket : Longint;

    { Get the active socket }
    function ActiveSocket : curl_socket_t;

    { Get info on unmet time conditional

      Receive the TRUE if the condition provided in the previous request didn't
      match. Alas, if this returns a TRUE you know that the reason you didn't
      get data in return is because it didn't fulfill the condition. }
    function ConditionUnmet : Boolean;

    { Get the URL scheme (sometimes called protocol) used in the connection }
    function Scheme : String;
  private
    {%H-}constructor Create (AHandle : CURL; AErrorStack : PErrorStack);

    { Callback for writting received data

      This callback function gets called by libcurl as soon as there is data
      received that needs to be saved. For most transfers, this callback gets
      called many times and each invoke delivers another chunk of data. ptr
      points to the delivered data, and the size of that data is nmemb; size is
      always 1. }
    class function WriteFunctionCallback (ptr : PChar; size : LongWord; nmemb :
      LongWord; data : Pointer) : LongWord; static; cdecl;
    function Write (ptr : PChar; size : LongWord; nmemb : LongWord) :
      LongWord;

    { Convert cURL lib error codes to THTTPErrors }
    class function CurlErrorToRequestError (ACode : CURLcode) : THTTPErrors;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    FHandle : CURL;
    FErrorStack : TErrorStack;
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
      THTTPResponseResult = specialize TCurlResult<THTTPResponse,
        THTTPResponse.THTTPErrors>;
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
    function Get : THTTPResponseResult;
  private
    FHandle : CURL;
    FErrorStack : TErrorStack;
  end;

implementation

{ THTTPResponse }

class function THTTPResponse.CurlErrorToRequestError (ACode : CURLcode) :
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

class function THTTPResponse.WriteFunctionCallback (ptr : PChar; size :
  LongWord; nmemb : LongWord; data : Pointer) : LongWord; static; cdecl;
begin
  Result := THTTPResponse(data).Write(ptr, size, nmemb);
end;

function THTTPResponse.Write (ptr : PChar; size : LongWord; nmemb :
  LongWord) : LongWord;
begin
  Result := FBuffer.Write(ptr^, size * nmemb);
end;

constructor THTTPResponse.Create (AHandle : CURL; AErrorStack :
  PErrorStack);
begin
  FHandle := AHandle;
  FErrorStack := TErrorStack.Create;
  FErrorStack := AErrorStack^;
  FBuffer := TMemoryStream.Create;

  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_WRITEDATA, Pointer(Self)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_WRITEFUNCTION,
    @THTTPResponse.WriteFunctionCallback));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_ERRORBUFFER,
    PChar(FErrorBuffer)));
end;

destructor THTTPResponse.Destroy;
begin
  curl_easy_cleanup(FHandle);
  FreeAndNil(FErrorStack);
  FreeAndNil(FBuffer);

  inherited Destroy;
end;

function THTTPResponse.HasErrors : Boolean;
begin
  Result := FErrorStack.Count > 0;
end;

function THTTPResponse.ErrorMessage : String;
begin
  Result := String(FErrorBuffer);
end;

function THTTPResponse.Errors : TErrorStack;
begin
  Result := FErrorStack;
end;

function THTTPResponse.Content : String;
var
  Stream : TStringStream;
begin
  Stream := TStringStream.Create('');
  Stream.Write(FBuffer.Memory^, FBuffer.Size);
  Result := Stream.DataString;
  FreeAndNil(Stream);
end;

function THTTPResponse.EffectiveUrl : String;
var
  url : PChar;
begin
  New(url);
  url := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_EFFECTIVE_URL, @url));
  Result := String(url);
end;

function THTTPResponse.ContentType : String;
var
  content_type : PChar;
begin
  New(content_type);
  content_type := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_CONTENT_TYPE,
    @content_type));
  Result := String(content_type);
end;

function THTTPResponse.RedirectUrl : String;
var
  url : PChar;
begin
  New(url);
  url := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_REDIRECT_URL, @url));
  Result := url;
end;

function THTTPResponse.PrimaryIP : String;
var
  ip : PChar;
begin
  New(ip);
  ip := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_PRIMARY_IP, @ip));
  Result := ip;
end;

function THTTPResponse.LocalIP : String;
var
  ip : PChar;
begin
  New(ip);
  ip := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_LOCAL_IP, @ip));
  Result := ip;
end;

function THTTPResponse.ResponseCode : THTTPStatusCode;
var
  Code : Longint;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_RESPONSE_CODE, @Code));
  Result := THTTPStatusCode(Code);
end;

function THTTPResponse.VerifySSLResult : Boolean;
var
  verify : Longint = 1;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SSL_VERIFYRESULT,
    @verify));
  Result := (verify = 0);
end;

function THTTPResponse.VerifySSLProxyResult : Boolean;
var
  verify : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_PROXY_SSL_VERIFYRESULT,
    @verify));
  Result := Boolean(verify);
end;

function THTTPResponse.ConnectResponseCode : THTTPStatusCode;
var
  code : Longint;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_HTTP_CONNECTCODE,
    @code));
  Result := THTTPStatusCode(code);
end;

function THTTPResponse.HTTPVersion : THTTPVersion;
var
  version : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_HTTP_VERSION, @version));
  Result := THTTPVersion(version);
end;

function THTTPResponse.RedirectCount : Longint;
var
  count : Longint;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_REDIRECT_COUNT,
    @count));
  Result := count;
end;

function THTTPResponse.Downloaded : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_SIZE_DOWNLOAD_T,
    @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SIZE_DOWNLOAD,
      @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.DownloadSpeed : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_SPEED_DOWNLOAD_T,
    @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SPEED_DOWNLOAD,
      @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.HeaderSize : TDataSize;
var
  bytes : LongWord = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_HEADER_SIZE, @bytes));
  Result := TDataSize.Create;
  Result.Bytes := bytes;
end;

function THTTPResponse.RequestSize : TDataSize;
var
  bytes : LongWord = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_REQUEST_SIZE, @bytes));
  Result := TDataSize.Create;
  Result.Bytes := bytes;
end;

function THTTPResponse.Uploaded : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_SIZE_UPLOAD_T, @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SIZE_UPLOAD,
      @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.UploadSpeed : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_SPEED_UPLOAD_T, @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SPEED_UPLOAD,
      @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.ContentLengthDownload : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T,
    @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle,
      CURLINFO_CONTENT_LENGTH_DOWNLOAD, @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.ContentLengthUpload : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_CONTENT_LENGTH_UPLOAD_T,
    @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_CONTENT_LENGTH_UPLOAD,
      @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.NumConnects : Longint;
var
  connects : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_NUM_CONNECTS,
    @connects));
  Result := connects;
end;

function THTTPResponse.PrimaryPort : Longint;
var
  port : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_PRIMARY_PORT, @port));
  Result := port;
end;

function THTTPResponse.LocalPort : Longint;
var
  port : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_LOCAL_PORT, @port));
  Result := port;
end;

function THTTPResponse.TotalTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_TOTAL_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_TOTAL_TIME, @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.NameLookup : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_NAMELOOKUP_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_NAMELOOKUP_TIME,
      @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.ConnectTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_CONNECT_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_CONNECT_TIME, @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.AppConnectTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_APPCONNECT_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_APPCONNECT_TIME,
      @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.PreTransferTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_PRETRANSFER_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_PRETRANSFER_TIME,
      @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.StartTransferTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_STARTTRANSFER_TIME_T,
    @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_STARTTRANSFER_TIME,
      @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.RedirectTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_REDIRECT_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_REDIRECT_TIME,
      @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.RetryAfterDelay : TTimeInterval;
var
  delay : LongWord = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_RETRY_AFTER, @delay));
  Result := TTimeInterval.Create;
  Result.Seconds := delay;
end;

function THTTPResponse.OsErrno : Longint;
var
  errno : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_OS_ERRNO, @errno));
  Result := errno;
end;

function THTTPResponse.LastSocket : Longint;
var
  socket : Longint;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_LASTSOCKET, @socket));
  Result := socket;
end;

function THTTPResponse.ActiveSocket : curl_socket_t;
var
  socket : curl_socket_t;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_ACTIVESOCKET, @socket));
  Result := socket;
end;

function THTTPResponse.ConditionUnmet : Boolean;
var
  unmet : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_CONDITION_UNMET,
    @unmet));
  Result := Boolean(unmet);
end;

function THTTPResponse.Scheme : String;
var
  sc : PChar;
begin
  New(sc);
  scheme := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SCHEME, @sc));
  Result := sc;
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

function THTTPSessionPlain.Get : THTTPResponseResult;
var
  Handle : CURL;
  Request : THTTPResponse;
  ErrorCode : CURLcode;
begin
  Handle := curl_easy_duphandle(FHandle);
  Request := THTTPResponse.Create(Handle, @FErrorStack);
  ErrorCode := curl_easy_perform(Handle);
  FErrorStack.Push(ErrorCode);
  Result := THTTPResponseResult.Create(Request,
    THTTPResponse.CurlErrorToRequestError(ErrorCode), ErrorCode = CURLE_OK);
end;

end.


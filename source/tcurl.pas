(******************************************************************************)
(*                                 libPasCURL                                 *)
(*                 object pascal wrapper around cURL library                  *)
(*                        https://github.com/curl/curl                        *)
(*                                                                            *)
(* Copyright (c) 2019                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpascurl                  ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* Module:          Unit 'tcurl'                                              *)
(* Functionality:   Provides  TSession class  which present  cURL session  to *)
(*                  assign request params.  And TSessionInfo class to getting *)
(*                  information from server response.                         *)
(*                                                                            *)
(******************************************************************************)

unit tcurl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pascurl, BaseUnix;

type
  Protocol = (
    PROTOCOL_DICT                     = CURLPROTO_DICT,
    PROTOCOL_FILE                     = CURLPROTO_FILE,
    PROTOCOL_FTP                      = CURLPROTO_FTP{%H-},
    PROTOCOL_FTPS                     = CURLPROTO_FTPS,
    PROTOCOL_GOPHER                   = CURLPROTO_GOPHER,
    PROTOCOL_HTTP                     = CURLPROTO_HTTP,
    PROTOCOL_HTTPS                    = CURLPROTO_HTTPS,
    PROTOCOL_IMAP                     = CURLPROTO_IMAP,
    PROTOCOL_IMAPS                    = CURLPROTO_IMAPS,
    PROTOCOL_LDAP                     = CURLPROTO_LDAP,
    PROTOCOL_LDAPS                    = CURLPROTO_LDAPS,
    PROTOCOL_POP3                     = CURLPROTO_POP3,
    PROTOCOL_POP3S                    = CURLPROTO_POP3S,
    PROTOCOL_RTMP                     = CURLPROTO_RTMP,
    PROTOCOL_RTMPE                    = CURLPROTO_RTMPE,
    PROTOCOL_RTMPS                    = CURLPROTO_RTMPS,
    PROTOCOL_RTMPT                    = CURLPROTO_RTMPT,
    PROTOCOL_RTMPTE                   = CURLPROTO_RTMPTE,
    PROTOCOL_RTMPTS                   = CURLPROTO_RTMPTS,
    PROTOCOL_RTSP                     = CURLPROTO_RTSP,
    PROTOCOL_SCP                      = CURLPROTO_SCP,
    PROTOCOL_SFTP                     = CURLPROTO_SFTP,
    PROTOCOL_SMB                      = CURLPROTO_SMB,
    PROTOCOL_SMBS                     = CURLPROTO_SMBS,
    PROTOCOL_SMTP                     = CURLPROTO_SMTP,
    PROTOCOL_SMTPS                    = CURLPROTO_SMTPS,
    PROTOCOL_TELNET                   = CURLPROTO_TELNET,
    PROTOCOL_TFTP                     = CURLPROTO_TFTP
  );

  StatusCode = (
    HTTP_CONTINUE                     = 100,
    HTTP_SWITCHING_PROTOCOL           = 101,

    HTTP_OK                           = 200,
    HTTP_CREATED                      = 201,
    HTTP_ACCEPTED                     = 202,
    HTTP_NON_AUTHORITATIVE_INFORMATION = 203,
    HTTP_NO_CONTENT                   = 204,
    HTTP_RESET_CONTENT                = 205,
    HTTP_PARTIAL_CONTENT              = 206,

    HTTP_MULTIPLE_CHOICES             = 300,
    HTTP_MOVED_PERMANENTLY            = 301,
    HTTP_FOUND                        = 302,
    HTTP_SEE_OTHER                    = 303,
    HTTP_NOT_MODIFIED                 = 304,
    HTTP_USE_PROXY                    = 305,
    HTTP_TEMPORARY_REDIRECT           = 307,

    HTTP_BAD_REQUEST                  = 400,
    HTTP_UNAUTHORIZED                 = 401,
    HTTP_FORBIDDEN                    = 403,
    HTTP_NOT_FOUND                    = 404,
    HTTP_METHOD_NOT_ALLOWED           = 405,
    HTTP_NOT_ACCEPTABLE               = 406,
    HTTP_PROXY_AUTHENTIFICATION_REQUIRED = 407,
    HTTP_REQUEST_TIMEOUT              = 408,
    HTTP_CONFLICT                     = 409,
    HTTP_GONE                         = 410,
    HTTP_LENGTH_REQUIRED              = 411,
    HTTP_PRECONDITION_FAILED          = 412,
    HTTP_REQUEST_ENTITY_TOO_LARGE     = 413,
    HTTP_REQUEST_URL_TOO_LONG         = 414,
    HTTP_UNSUPPORTED_MEDIA_TYPE       = 415,
    HTTP_REQUESTED_RANGE_NOT_SATISFIABLE = 416,
    HTTP_EXPECTATION_FAILED           = 417,

    HTTP_INTERNAL_SERVER_ERROR        = 500,
    HTTP_NOT_IMPLEMENTED              = 501,
    HTTP_BAD_GETEWAY                  = 502,
    HTTP_SERVICE_UNAVAIBLE            = 503,
    HTTP_GATEWAY_TIMEOUT              = 504,
    HTTP_VERSION_NOT_SUPPORTED        = 505
  );

  HTTPVersionCode = (
    HTTP_VERSION_UNKNOWN              = 0,
    HTTP_VERSION_1_0                  = Longint(CURL_HTTP_VERSION_1_0),
    HTTP_VERSION_1_1                  = Longint(CURL_HTTP_VERSION_1_1),
    HTTP_VERSION_2_0                  = Longint(CURL_HTTP_VERSION_2_0),
    HTTP_VERSION_3_0                  = Longint(CURL_HTTP_VERSION_3),
  );

  { TSession }
  { Present cURL session to assign request params }

  TSession = class
  protected
    handle : CURL;
    buffer : TStringStream;
  protected
    (**
     * Callback for writting received data
     *
     * This callback function gets called by libcurl as soon as there is data
     * received that needs to be saved. For most transfers, this callback gets
     * called many times and each invoke delivers another chunk of data. ptr
     * points to the delivered data, and the size of that data is nmemb; size is
     * always 1.
     *)
    class function WriteFunction (ptr : PChar; size : LongWord;
      nmemb : LongWord; data : Pointer) : LongWord; static; cdecl;

    (**
     * Callback for data uploads
     *
     * This callback function gets called by libcurl as soon as it needs to read
     * data in order to send it to the peer - like if you ask it to upload or
     * post data to the server. The data area pointed at by the pointer buffer
     * should be filled up with at most size multiplied with nitems number of
     * bytes by your function.
     *)
    class function ReadFunction (buf : PChar; size : LongWord;
      nitems : LongWord; data : Pointer) : LongWord; static; cdecl;

    (**
     * Save received data to the inner buffer
     *)
    function Write (ptr : PChar; size : LongWord; nmemb : LongWord) : LongWord;
      inline;

    (**
     * Write uploads data to the buf
     *)
    function Read (buf : PChar; size : LongWord; nitems : LongWord) : LongWord;
      inline;
    function IsOpened : Boolean;
    procedure SetUrl (url : string);
    procedure SetProxy (proxy : string);
    procedure SetUserAgent (agent : string);
    procedure SetPort (port : Longint);
    procedure SetProxyPort (port : Longint);
    procedure SetFollowRedirect (redirect : boolean);
    procedure SetAutoReferer (updateHeaders : boolean);
    procedure SetIncludeHeader (includeHeader : boolean);
    procedure SetIgnoreContentLength (ignoreLength : boolean);
    procedure SetNoBody (noBody : boolean);
    procedure SetTransferEncoding (encoding : boolean);
  public
    constructor Create;
    destructor Destroy; override;

    (**
     * Check if session opened and correctly
     *)
    property Opened : Boolean read IsOpened;

    (**
     * Set the URL to use in the request
     *
     * The parameter should be a string which must be URL-encoded in the
     * following format: scheme://host:port/path
     * libcurl doesn't validate the syntax or use this variable until the
     * transfer is issued.
     * If the given URL is missing a scheme name (such as "http://" or "ftp://"
     * etc) then libcurl will make a guess based on the host. If the outermost
     * sub-domain name matches DICT, FTP, IMAP, LDAP, POP3 or SMTP then that
     * protocol will be used, otherwise HTTP will be used.
     * The host part of the URL contains the address of the server that you want
     * to connect to. This can be the fully qualified domain name of the server,
     * the local network name of the machine on your network or the IP address
     * of the server or machine represented by either an IPv4 or IPv6 address.
     * http://www.example.com/
     * http://hostname/
     * http://192.168.0.1/
     * http://[2001:1890:1112:1::20]/
     * It is also possible to specify the user name, password and any supported
     * login options as part of the host, for the following protocols, when
     * connecting to servers that require authentication:
     * http://user:password@www.example.com
     * ftp://user:password@ftp.example.com
     * smb://domain%2fuser:password@server.example.com
     * imap://user:password;options@mail.example.com
     * pop3://user:password;options@mail.example.com
     * smtp://user:password;options@mail.example.com
     *)
    property Url : string write SetUrl;

    (**
     * Set proxy to use
     *
     * Set the proxy to use for the upcoming request. The parameter should be a
     * string holding the host name or dotted numerical IP address. A numerical
     * IPv6 address must be written within [brackets].
     * To specify port number in this string, append :[port] to the end of the
     * host name. If not specified, libcurl will default to using port 1080 for
     * proxies.
     * The proxy string may be prefixed with [scheme]:// to specify which kind
     * of proxy is used.
     * http://    HTTP Proxy. Default when no scheme or proxy type is specified.
     * https://   HTTPS Proxy.
     * socks4://  SOCKS4 Proxy.
     * socks4a:// SOCKS4a Proxy. Proxy resolves URL hostname.
     * socks5://  SOCKS5 Proxy.
     * socks5h:// SOCKS5 Proxy. Proxy resolves URL hostname.
     * When you tell the library to use an HTTP proxy, libcurl will
     * transparently convert operations to HTTP even if you specify an FTP URL
     * etc.
     * Setting the proxy string to "" (an empty string) will explicitly disable
     * the use of a proxy, even if there is an environment variable set for it.
     * A proxy host string can also include protocol scheme (http://) and
     * embedded user + password.
     *)
    property Proxy : string write SetProxy;

    (**
     * Set HTTP user-agent header
     *
     * It will be used to set the User-Agent: header in the HTTP request sent to
     * the remote server. This can be used to fool servers or scripts.
     *)
    property UserAgent : string write SetUserAgent;

    (**
     * Set remote port number to work with
     *
     * This option sets number to be the remote port number to connect to,
     * instead of the one specified in the URL or the default port for the used
     * protocol.
     * Usually, you just let the URL decide which port to use but this allows
     * the application to override that.
     *)
    property Port : Longint write SetPort;

    (**
     * Port number the proxy listens on
     *
     * Set the proxy port to connect to unless it is specified in the proxy
     * string.
     *)
    property ProxyPort : Longint write SetProxyPort;

    (**
     * Follow HTTP 3XXX redirects
     *
     * Tells the library to follow any Location: header that the server sends as
     * part of an HTTP header in a 3xx response. The Location: header can
     * specify a relative or an absolute URL to follow.
     * libcurl will issue another request for the new URL and follow new
     * Location: headers all the way until no more such headers are returned.
     * libcurl limits what protocols it automatically follows to.
     * By default libcurl will allow HTTP, HTTPS, FTP and FTPS on redirect.
     *)
    property FollowRedirect : Boolean write SetFollowRedirect default True;

    (**
     * Automatically update the referer header
     *
     * When enabled, libcurl will automatically set the Referer: header field in
     * HTTP requests where it follows a Location: redirect.
     *)
    property AutoReferer : Boolean write SetAutoReferer default True;

    (**
     * Pass headers to the data stream
     *
     * Ask libcurl to include the headers in the data stream.
     * When asking to get the headers passed to the body, it is not possible to
     * accurately separate them again without detailed knowledge about the
     * protocol in use.
     *)
    property IncludeHeader : Boolean write SetIncludeHeader default False;

    (**
     * Ignore content length
     *
     * Ignore the Content-Length header in the HTTP response and ignore asking
     * for or relying on it for FTP transfers.
     * This is useful for HTTP with Apache 1.x (and similar servers) which will
     * report incorrect content length for files over 2 gigabytes. If this
     * option is used, curl will not be able to accurately report progress,
     * and will simply stop the download when the server ends the connection.
     * It is also useful with FTP when for example the file is growing while the
     * transfer is in progress which otherwise will unconditionally cause
     * libcurl to report error.
     *)
    property IgnoreContentLength : Boolean write SetIgnoreContentLength
      default False;

    (**
     * Do the download request without getting the body
     *
     * Tells libcurl to not include the body-part in the output when doing what
     * would otherwise be a download. For HTTP(S), this makes libcurl do a HEAD
     * request. For most other protocols it means just not asking to transfer
     * the body data.
     *)
    property NoBody : Boolean write SetNoBody default False;

    (**
     * Ask for HTTP Transfer Encoding
     *
     * Add a request for compressed Transfer Encoding in the outgoing HTTP
     * request. If the server supports this and so desires, it can respond with
     * the HTTP response sent using a compressed Transfer-Encoding that will be
     * automatically uncompressed by libcurl on reception.
     *)
    property TransferEncoding : Boolean write SetTransferEncoding
      default False;
  end;

  { TSessionInfo }
  { Getting information from server response. }

  TSessionInfo = class
  protected
    session : TSession;
    hasInfo : Boolean;
    errorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
  protected
    function GetStringProperty (const value : PChar) : string;

    function IsOpened : Boolean;
    function CheckErrors : Boolean;
    function GetErrorMessage : string;
    function GetEffectiveUrl : string;
    function GetRedirectUrl : string;
    function GetContentType : string;
    function GetPrimaryIP : string;
    function GetLocalIP : string;
    function GetResponseCode : StatusCode;
    function GetContent : string;
    function GetVerifySSLResult : boolean;
    function GetVerifySSLProxyResult : boolean;
    function GetConnectResponseCode : StatusCode;
    function GetHttpVersion : HTTPVersionCode;
    function GetRedirectCount : Longint;
    function GetUploadedBytes : LongWord;
    function GetDownloadedBytes : LongWord;
    function GetDownloadSpeedBytesPerSecond : LongWord;
    function GetUploadSpeedBytesPerSecond : LongWord;
    function GetHeaderSizeBytes : LongWord;
    function GetRequestSizeBytes : Longint;
    function GetContentLengthDownload : LongWord;
    function GetContentLengthUpload : LongWord;
    function GetNumConnects : Longint;
    function GetPrimaryPort : Longint;
    function GetLocalPort : Longint;
    function GetFileTime : time_t;
    function GetTotalTime : time_t;
    function GetNameLookup : Double;
    function GetConnectTime : LongWord;
  public
    constructor Create; overload;
    constructor Create (var s : TSession); overload;

    (**
     * Perform a bloking file transfer
     *
     * Will perform the transfer as described in the TSession options.
     * Performs the entire request in a blocking manner and returns when done,
     * or if it failed.
     *)
    property Opened : Boolean read IsOpened;

    (**
     * Check if has errors on last request
     *)
    property HasErrors : Boolean read CheckErrors;

    (**
     * Return last error message or empty string if none
     *)
    property ErrorMessage : string read GetErrorMessage;

    (**
     * Get the last used URL
     *
     * Get the last used effective URL. In cases when you've asked libcurl to
     * follow redirects, it may very well not be the same value you set.
     *)
    property EffectiveUrl : string read GetEffectiveUrl;

    (**
     * Get the URL a redirect would go to
     *)
    property RedirectUrl : string read GetRedirectUrl;

    (**
     * Get Content-Type
     *
     * This is the value read from the Content-Type: field. If you get empty,
     * it means that the server didn't send a valid Content-Type header or that
     * the protocol used doesn't support this.
     *)
    property ContentType : string read GetContentType;

    (**
     * Get IP address of last connection
     *)
    property PimaryIP : string read GetPrimaryIP;

    (**
     * Get local IP address of last connection
     *)
    property LocalIP : string read GetLocalIP;

    (**
     * Get the last response code
     *)
    property ResponseCode : StatusCode read GetResponseCode;

    (**
     * Get the response content
     *)
    property Content : string read GetContent;

    (**
     * Get the result of the certificate verification
     *)
    property VerifySSLResult : boolean read GetVerifySSLResult;

    (**
     * Get the result of the proxy certificate verification
     *
     * This is only used for HTTPS proxies.
     *)
    property VerifySSLProxyResult : boolean read GetVerifySSLProxyResult;

    (**
     * Get the CONNECT response code
     *
     * Last received HTTP proxy response code to a CONNECT request.
     *)
    property ConnectResponseCode : StatusCode read GetConnectResponseCode;

    (**
     * Get the HTTP version used in the connection
     *)
    property HttpVersion : HttpVersionCode read GetHttpVersion;

    (**
     * Get the number of redirects
     *)
    property RedirectCount : Longint read GetRedirectCount;

    (**
     * Get the number of uploaded bytes
     *)
    property UploadedBytes : LongWord read GetUploadedBytes;

    (**
     * Get the number of downloaded bytes
     *)
    property DownloadedBytes : LongWord read GetDownloadedBytes;

    (**
     * Get download speed
     *)
    property DownloadSpeedBytesPerSecond : LongWord
      read GetDownloadSpeedBytesPerSecond;

    (**
     * Get upload speed
     *)
    property UploadSpeedBytesPerSecond : LongWord
      read GetUploadSpeedBytesPerSecond;

    (**
     * Get size of retrieved headers
     *)
    property HeaderSizeBytes : LongWord read GetHeaderSizeBytes;

    (**
     * Get size of sent request
     *)
    property RequestSizeBytes : Longint read GetRequestSizeBytes;

    (**
     * Get content-length of download
     *)
    property ContentLengthDownload : LongWord read GetContentLengthDownload;

    (**
     * Get the specified size of the upload
     *)
    property ContentLengthUpload : LongWord read GetContentLengthUpload;

    (**
     * Get number of created connections
     *)
    property NumConnects : Longint read GetNumConnects;

    (**
     * Get the latest destination port number
     *)
    property PrimaryPort : Longint read GetPrimaryPort;

    (**
     * Get the latest local port number
     *)
    property LocalPort : Longint read GetLocalPort;

    (**
     * Get the remote time of the retrieved document
     *)
    property FileTime : time_t read GetFileTime;

    (**
     * Get total time of previous transfer
     *)
    property TotalTime : time_t read GetTotalTime;

    (**
     * Get the name lookup time
     *)
    property NameLookup : Double read GetNameLookup;

    (**
     * Get the time until connect
     *)
    property ConnectTime : LongWord read GetConnectTime;
  end;

implementation

{ TSessionInfo }

function TSessionInfo.GetStringProperty(const value: PChar): string;
begin
  if value <> nil then
  begin
    Result := value;
    UniqueString(Result);
  end;
end;

function TSessionInfo.IsOpened: Boolean;
begin
  Result := (session.Opened and hasInfo);
end;

function TSessionInfo.CheckErrors: Boolean;
begin
  Result := not Opened;
end;

function TSessionInfo.GetErrorMessage: string;
begin
  if HasErrors then
  begin
    Result := errorBuffer;
  end;
end;

function TSessionInfo.GetEffectiveUrl: string;
var
  url : PChar;
begin
  if Opened then
  begin
    New(url);
    curl_easy_getinfo(session.handle, CURLINFO_EFFECTIVE_URL, @url);
    Result := GetStringProperty(url);
  end;
end;

function TSessionInfo.GetRedirectUrl: string;
var
  url : PChar;
begin
  if Opened then
  begin
    New(url);
    curl_easy_getinfo(session.handle, CURLINFO_REDIRECT_URL, @url);
    Result := GetStringProperty(url);
  end;
end;

function TSessionInfo.GetContentType: string;
var
  content_type : PChar;
begin
  if Opened then
  begin
    New(content_type);
    curl_easy_getinfo(session.handle, CURLINFO_CONTENT_TYPE, @content_type);
    Result := GetStringProperty(content_type);
  end;
end;

function TSessionInfo.GetPrimaryIP: string;
var
  ip : PChar;
begin
  if Opened then
  begin
    New(ip);
    curl_easy_getinfo(session.handle, CURLINFO_PRIMARY_IP, @ip);
    Result := GetStringProperty(ip);
  end;
end;

function TSessionInfo.GetLocalIP: string;
var
  ip : PChar;
begin
  if Opened then
  begin
    New(ip);
    curl_easy_getinfo(session.handle, CURLINFO_LOCAL_IP, @ip);
    Result := GetStringProperty(ip);
  end;
end;

function TSessionInfo.GetResponseCode: StatusCode;
var
  code : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_RESPONSE_CODE, @code);
    Result := StatusCode(code);
  end;
end;

function TSessionInfo.GetContent: string;
begin
  if Opened then
  begin
    Result := session.buffer.DataString;
  end;
end;

function TSessionInfo.GetVerifySSLResult: boolean;
var
  verify : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_SSL_VERIFYRESULT, @verify);
    Result := (verify = 0);
  end;
end;

function TSessionInfo.GetVerifySSLProxyResult: boolean;
var
  verify : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_PROXY_SSL_VERIFYRESULT, @verify);
    Result := Boolean(verify);
  end;
end;

function TSessionInfo.GetConnectResponseCode: StatusCode;
var
  code : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_HTTP_CONNECTCODE, @code);
    Result := StatusCode(code);
  end;
end;

function TSessionInfo.GetHttpVersion: HTTPVersionCode;
var
  ver : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_HTTP_VERSION, @ver);
    Result := HTTPVersionCode(ver);
  end;
end;

function TSessionInfo.GetRedirectCount: Longint;
var
  count : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_REDIRECT_COUNT, @count);
    Result := count;
  end;
end;

function TSessionInfo.GetUploadedBytes: LongWord;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_SIZE_UPLOAD_T, @bytes);
    Result := bytes;
  end;
end;

function TSessionInfo.GetDownloadedBytes: LongWord;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_SIZE_DOWNLOAD_T, @bytes);
    Result := bytes;
  end;
end;

function TSessionInfo.GetDownloadSpeedBytesPerSecond: LongWord;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_SPEED_DOWNLOAD_T, @bytes);
    Result := bytes;
  end;
end;

function TSessionInfo.GetUploadSpeedBytesPerSecond: LongWord;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_SPEED_UPLOAD_T, @bytes);
    Result := bytes;
  end;
end;

function TSessionInfo.GetHeaderSizeBytes: LongWord;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_HEADER_SIZE, @bytes);
    Result := bytes;
  end;
end;

function TSessionInfo.GetRequestSizeBytes: Longint;
var
  bytes : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_REQUEST_SIZE, @bytes);
    Result := bytes;
  end;
end;

function TSessionInfo.GetContentLengthDownload: LongWord;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T, @bytes);
    Result := bytes;
  end;
end;

function TSessionInfo.GetContentLengthUpload: LongWord;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_CONTENT_LENGTH_UPLOAD_T, @bytes);
    Result := bytes;
  end;
end;

function TSessionInfo.GetNumConnects: Longint;
var
  num : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_NUM_CONNECTS, @num);
    Result := num;
  end;
end;

function TSessionInfo.GetPrimaryPort: Longint;
var
  port : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_PRIMARY_PORT, @port);
    Result := port;
  end;
end;

function TSessionInfo.GetLocalPort: Longint;
var
  port : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_LOCAL_PORT, @port);
    Result := port;
  end;
end;

function TSessionInfo.GetFileTime: time_t;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_FILETIME_T, @time);
    Result := time_t(time);
  end;
end;

function TSessionInfo.GetTotalTime: time_t;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_TOTAL_TIME_T, @time);
    Result := time_t(time);
  end;
end;

function TSessionInfo.GetNameLookup: Double;
var
  time : Double;
begin
  if Opened thne
  begin
    curl_easy_getinfo(session.handle, CURLINFO_NAMELOOKUP_TIME, @time);
    Result := time;
  end;
end;

function TSessionInfo.GetConnectTime: LongWord;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_CONNECT_TIME_T, @time);
    Result := time;
  end;
end;

constructor TSessionInfo.Create;
begin
  // Do nothing
end;

constructor TSessionInfo.Create(var s: TSession);
begin
  if s.Opened then
  begin
    Self.session := s;
    curl_easy_setopt(session.handle, CURLOPT_ERRORBUFFER, PChar(errorBuffer));
    hasInfo := (curl_easy_perform(session.handle) = CURLE_OK);
  end;
end;

{ TSession }

class function TSession.WriteFunction (ptr: PChar; size: LongWord;
  nmemb: LongWord; data: Pointer): LongWord; cdecl;
begin
  Result := TSession(data).Write(ptr, size, nmemb);
end;

class function TSession.ReadFunction (buf: PChar; size: LongWord;
  nitems: LongWord; data: Pointer): LongWord; cdecl;
begin
  Result := TSession(data).Read(buf, size, nitems);
end;

function TSession.Write(ptr: PChar; size: LongWord; nmemb: LongWord): LongWord;
begin
  buffer.WriteString(string(ptr));
  Result := size * nmemb;
end;

function TSession.Read(buf: PChar; size: LongWord; nitems: LongWord): LongWord;
begin
  Result := 0;
end;

constructor TSession.Create;
begin
  inherited Create;

  handle := curl_easy_init;
  buffer := TStringStream.Create('');

  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(Self));
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, @TSession.WriteFunction);
    FollowRedirect := True;
  end;
end;

destructor TSession.Destroy;
begin
  curl_easy_cleanup(handle);
  FreeAndNil(buffer);

  inherited Destroy;
end;

function TSession.IsOpened: Boolean;
begin
  Result := {%H-}LongWord(handle) <> 0;
end;

procedure TSession.SetUrl(url: string);
begin
  if Opened then
  begin
    buffer.Size := 0;
    curl_easy_setopt(handle, CURLOPT_URL, PChar(url));
  end;
end;

procedure TSession.SetProxy(proxy: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PROXY, PChar(proxy));
  end;
end;

procedure TSession.SetUserAgent(agent: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_USERAGENT, PChar(agent));
  end;
end;

procedure TSession.SetPort(port: Longint);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PORT, port);
  end;
end;

procedure TSession.SetProxyPort(port: Longint);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PROXYPORT, port);
  end;
end;

procedure TSession.SetFollowRedirect(redirect: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, Longint(redirect));
  end;
end;

procedure TSession.SetAutoReferer(updateHeaders: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_AUTOREFERER, Longint(updateHeaders));
  end;
end;

procedure TSession.SetIncludeHeader(includeHeader: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_HEADER, Longint(includeHeader));
  end;
end;

procedure TSession.SetIgnoreContentLength(ignoreLength: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_IGNORE_CONTENT_LENGTH,
      Longint(ignoreLength));
  end;
end;

procedure TSession.SetNoBody(noBody: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_NOBODY, Longint(noBody));
  end;
end;

procedure TSession.SetTransferEncoding(encoding: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_TRANSFER_ENCODING, Longint(encoding));
  end;
end;

initialization
  curl_global_init(CURL_GLOBAL_ALL);

finalization
  curl_global_cleanup;

end.


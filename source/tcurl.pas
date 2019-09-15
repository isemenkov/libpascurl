unit tcurl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pascurl;

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
    HTTP_VERSION_2_0                  = Longint(CURL_HTTP_VERSION_2_0)
  );

  { TSession }

  TSession = class
  protected
    handle : CURL;
    buffer : TStringStream;
  protected
    class function WriteFunction (ptr : PChar; size : LongWord;
      nmemb : LongWord; data : Pointer) : LongWord; static; cdecl;
    class function ReadFunction (buf : PChar; size : LongWord;
      nitems : LongWord; data : Pointer) : LongWord; static; cdecl;

    function Write (ptr : PChar; size : LongWord; nmemb : LongWord) : LongWord;
      inline;
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
    procedure SetReceivedData (received : boolean);
    procedure SetTransferEncoding (encoding : boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property Opened : Boolean read IsOpened;
    property Url : string write SetUrl;
    property Proxy : string write SetProxy;
    property UserAgent : string write SetUserAgent;
    property Port : Longint write SetPort;
    property ProxyPort : Longint write SetProxyPort;
    property FollowRedirect : Boolean write SetFollowRedirect default True;
    property AutoReferer : Boolean write SetAutoReferer default True;
    property IncludeHeader : Boolean write SetIncludeHeader default False;
    property IgnoreContentLength : Boolean write SetIgnoreContentLength
      default False;
    property NoBody : Boolean write SetNoBody default False;
    property ReceivedData : Boolean write SetReceivedData default True;
    property TransferEncoding : Boolean write SetTransferEncoding
      default False;
  end;

  { TSessionInfo }

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
  public
    constructor Create; overload;
    constructor Create (var sess : TSession); overload;

    property Opened : Boolean read IsOpened;
    property HasErrors : Boolean read CheckErrors;
    property ErrorMessage : string read GetErrorMessage;
    property EffectiveUrl : string read GetEffectiveUrl;
    property RedirectUrl : string read GetRedirectUrl;
    property ContentType : string read GetContentType;
    property PimaryIP : string read GetPrimaryIP;
    property LocalIP : string read GetLocalIP;
    property ResponseCode : StatusCode read GetResponseCode;
    property Content : string read GetContent;
    property VerifySSLResult : boolean read GetVerifySSLResult;
    property VerifySSLProxyResult : boolean read GetVerifySSLProxyResult;
    property ConnectResponseCode : StatusCode read GetConnectResponseCode;
    property HttpVersion : HttpVersionCode read GetHttpVersion;
    property RedirectCount : Longint read GetRedirectCount;
    property UploadedBytes : LongWord read GetUploadedBytes;
    property DownloadedBytes : LongWord read GetDownloadedBytes;
    property DownloadSpeedBytesPerSecond : LongWord
      read GetDownloadSpeedBytesPerSecond;
    property UploadSpeedBytesPerSecond : LongWord
      read GetUploadSpeedBytesPerSecond;
    property HeaderSizeBytes : LongWord read GetHeaderSizeBytes;
    property RequestSizeBytes : Longint read GetRequestSizeBytes;
    property ContentLengthDownload : LongWord read GetContentLengthDownload;
    property ContentLengthUpload : LongWord read GetContentLengthUpload;
    property NumConnects : Longint read GetNumConnects;
    property PrimaryPort : Longint read GetPrimaryPort;
    property LocalPort : Longint read GetLocalPort;
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
    Result := Boolean(verify);
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

constructor TSessionInfo.Create;
begin
  // Do nothing
end;

constructor TSessionInfo.Create(var sess: TSession);
begin
  if sess.Opened then
  begin
    Self.session := sess;
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

  FollowRedirect := True;
  ReceivedData := True;
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

procedure TSession.SetReceivedData(received: boolean);
begin
  if Opened and received then
  begin
    curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(Self));
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, @TSession.WriteFunction);
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


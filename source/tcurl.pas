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
    class function write_function (ptr : PChar; size : Cardinal;
      nmemb : Cardinal; data : Pointer) : Cardinal; static; cdecl;
    class function read_function (buf : PChar; size : QWord; nitems : QWord;
      data : Pointer) : QWord; static; cdecl;

    function Write (ptr : PChar; size : QWord; nmemb : QWord) : QWord;
    function Read (buf : PChar; size : QWord; nitems : QWord) : QWord;
    function IsOpened : Boolean;
    procedure SetUrl (url : string);
    procedure SetFollowRedirect (redirect : boolean);
    procedure SetReceivedData (received : boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property Opened : Boolean read IsOpened;
    property Url : string write SetUrl;
    property FollowRedirect : Boolean write SetFollowRedirect default True;
    property ReceivedData : Boolean write SetReceivedData default true;
  end;

  { TSessionInfo }

  TSessionInfo = class
  protected
    session : TSession;
    hasInfo : Boolean;
    errorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
  protected
    function IsOpened : Boolean;
    function CheckErrors : Boolean;
    function GetErrorMessage : string;
    function GetEffectiveUrl : string;
    function GetResponseCode : StatusCode;
    function GetContent : string;
  public
    constructor Create; overload;
    constructor Create (var sess : TSession); overload;

    property Opened : Boolean read IsOpened;
    property HasErrors : Boolean read CheckErrors;
    property ErrorMessage : string read GetErrorMessage;
    property EffectiveUrl : string read GetEffectiveUrl;
    property ResponseCode : StatusCode read GetResponseCode;
    property Content : string read GetContent;
  end;

implementation

{ TSessionInfo }

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
  Result := '';
end;

function TSessionInfo.GetEffectiveUrl: string;
var
  url : string = '';
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_EFFECTIVE_URL, PChar(url));
    Result := url;
  end;
  Result := '';
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

class function TSession.write_function (ptr: PChar; size: Cardinal;
  nmemb: Cardinal; data: Pointer): Cardinal; cdecl;
begin
  Result := TSession(data).Write(ptr, size, nmemb);
end;

class function TSession.read_function (buf: PChar; size: QWord;
  nitems: QWord; data: Pointer): QWord; cdecl;
begin
  Result := TSession(data).Read(buf, size, nitems);
end;

function TSession.Write(ptr: PChar; size: QWord; nmemb: QWord): QWord;
begin
  buffer.WriteString(string(ptr));
  Result := size * nmemb;
end;

function TSession.Read(buf: PChar; size: QWord; nitems: QWord): QWord;
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
  Result := true;
end;

procedure TSession.SetUrl(url: string);
begin
  if Opened then
  begin
    buffer.Size := 0;
    curl_easy_setopt(handle, CURLOPT_URL, PChar(url));
  end;
end;

procedure TSession.SetFollowRedirect(redirect: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_FOLLOWLOCATION, Longint(redirect));
  end;
end;

procedure TSession.SetReceivedData(received: boolean);
begin
  if Opened and received then
  begin
    curl_easy_setopt(handle, CURLOPT_WRITEDATA, Pointer(Self));
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, @TSession.write_function);
  end;
end;

end.


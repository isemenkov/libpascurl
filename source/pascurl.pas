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
(* Module:          Unit 'pascurl'                                            *)
(* Functionality:   Provides  TSession class  which present  cURL session  to *)
(*                  assign request params.  And TSessionInfo class to getting *)
(*                  information from server response.                         *)
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

unit pascurl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libpascurl, BaseUnix, math, typinfo;

type
  TProtocol = (
    (**
     * DICT is a dictionary network protocol, it allows clients to ask
     * dictionary servers about a meaning or explanation for words. See RFC
     * 2229. Dict servers and clients use TCP port 2628.
     *)
    PROTOCOL_DICT,

    (**
     * FILE is not actually a "network" protocol. It is a URL scheme that allows
     * you to tell curl to get a file from the local file system instead of
     * getting it over the network from a remote server. See RFC 1738.
     *)
    PROTOCOL_FILE,

    (**
     * FTP stands for File Transfer Protocol and is an old (originates in the
     * early 1970s) way to transfer files back and forth between a client and a
     * server. See RFC 959. It has been extended greatly over the years. FTP
     * servers and clients use TCP port 21 plus one more port, though the second
     * one is usually dynamically established during communication.
     *)
    PROTOCOL_FTP,

    (**
     * FTPS stands for Secure File Transfer Protocol. It follows the tradition
     * of appending an 'S' to the protocol name to signify that the protocol is
     * done like normal FTP but with an added SSL/TLS security layer. See RFC
     * 4217.
     * This protocol is problematic to use through firewalls and other network
     * equipment.
     *)
    PROTOCOL_FTPS,

    (**
     * Designed for "distributing, searching, and retrieving documents over the
     * Internet", Gopher is somewhat of the grand father to HTTP as HTTP has
     * mostly taken over completely for the same use cases. See RFC 1436. Gopher
     * servers and clients use TCP port 70.
     *)
    PROTOCOL_GOPHER,

    (**
     * The Hypertext Transfer Protocol, HTTP, is the most widely used protocol
     * for transferring data on the web and over the Internet. See RFC 7230 for
     * HTTP/1.1 and RFC 7540 for HTTP/2. HTTP servers and clients use TCP port
     * 80.
     *)
    PROTOCOL_HTTP,

    (**
     * Secure HTTP is HTTP done over an SSL/TLS connection. See RFC 2818. HTTPS
     * servers and clients use TCP port 443, unless they speak HTTP/3 which then
     * uses QUIC and is done over UDP...
     *)
    PROTOCOL_HTTPS,

    (**
     * The Internet Message Access Protocol, IMAP, is a protocol for accessing,
     * controlling and "reading" email. See RFC 3501. IMAP servers and clients
     * use TCP port 143. Whilst connections to the server start out as
     * cleartext, SSL/TLS communication may be supported by the client
     * explicitly requesting to upgrade the connection using the STARTTLS
     * command. See RFC 2595.
     *)
    PROTOCOL_IMAP,

    (**
     * Secure IMAP is IMAP done over an SSL/TLS connection. Such connections
     * implicitly start out using SSL/TLS and as such servers and clients use
     * TCP port 993 to communicate with each other. See RFC 8314.
     *)
    PROTOCOL_IMAPS,

    (**
     * The Lightweight Directory Access Protocol, LDAP, is a protocol for
     * accessing and maintaining distributed directory information. Basically a
     * database lookup. See RFC 4511. LDAP servers and clients use TCP port 389.
     *)
    PROTOCOL_LDAP,

    (**
     * Secure LDAP is LDAP done over an SSL/TLS connection.
     *)
    PROTOCOL_LDAPS,

    (**
     * The Post Office Protocol version 3 (POP3) is a protocol for retrieving
     * email from a server. See RFC 1939. POP3 servers and clients use TCP port
     * 110. Whilst connections to the server start out as cleartext, SSL/TLS
     * communication may be supported by the client explicitly requesting to
     * upgrade the connection using the STLS command. See RFC 2595.
     *)
    PROTOCOL_POP3,

    (**
     * Secure POP3 is POP3 done over an SSL/TLS connection. Such connections
     * implicitly start out using SSL/TLS and as such servers and clients use
     * TCP port 995 to communicate with each other. See RFC 8314.
     *)
    PROTOCOL_POP3S,

    (**
     * The Real-Time Messaging Protocol (RTMP) is a protocol for streaming
     * audio, video and data. RTMP servers and clients use TCP port 1935.
     *)
    PROTOCOL_RTMP,
    PROTOCOL_RTMPE,
    PROTOCOL_RTMPS,
    PROTOCOL_RTMPT,
    PROTOCOL_RTMPTE,
    PROTOCOL_RTMPTS,

    (**
     * The Real Time Streaming Protocol (RTSP) is a network control protocol to
     * control streaming media servers. See RFC 2326. RTSP servers and clients
     * use TCP and UDP port 554.
     *)
    PROTOCOL_RTSP,

    (**
     * The Secure Copy (SCP) protocol is designed to copy files to and from a
     * remote SSH server. SCP servers and clients use TCP port 22.
     *)
    PROTOCOL_SCP,

    (**
     * The SSH File Transfer Protocol (SFTP) that provides file access, file
     * transfer, and file management over a reliable data stream. SFTP servers
     * and clients use TCP port 22.
     *)
    PROTOCOL_SFTP,

    (**
     * The Server Message Block (SMB) protocol is also known as CIFS. It is an
     * application-layer network protocol mainly used for providing shared
     * access to files, printers, and serial ports and miscellaneous
     * communications between nodes on a network. SMB servers and clients use
     * TCP port 445.
     *)
    PROTOCOL_SMB,
    PROTOCOL_SMBS,

    (**
     * The Simple Mail Transfer Protocol (SMTP) is a protocol for email
     * transmission. See RFC 5321. SMTP servers and clients use TCP port 25.
     * Whilst connections to the server start out as cleartext, SSL/TLS
     * communication may be supported by the client explicitly requesting to
     * upgrade the connection using the STARTTLS command. See RFC 3207.
     *)
    PROTOCOL_SMTP,

    (**
     * Secure SMTP, sometimes called SSMTP, is SMTP done over an SSL/TLS
     * connection. Such connections implicitly start out using SSL/TLS and as
     * such servers and clients use TCP port 465 to communicate with each other.
     * See RFC 8314.
     *)
    PROTOCOL_SMTPS,

    (**
     * TELNET is an application layer protocol used over networks to provide a
     * bidirectional interactive text-oriented communication facility using a
     * virtual terminal connection. See RFC 854. TELNET servers and clients use
     * TCP port 23.
     *)
    PROTOCOL_TELNET,

    (**
     * The Trivial File Transfer Protocol (TFTP) is a protocol for doing simple
     * file transfers over UDP to get a file from or put a file onto a remote
     * host. TFTP servers and clients use UDP port 69.
     *)
    PROTOCOL_TFTP
  );

  TProtocols = set of TProtocol;

  TStatusCode = (
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
    HTTP_VERSION_3_0                  = Longint(CURL_HTTP_VERSION_3)
  );

  (**
   * Proxy protocol type
   *)
  TProxyType = (
    (**
     * HTTP Proxy
     *)
    PROXY_HTTP                        = Longint(CURLPROXY_HTTP),

    (**
     * HTTP 1.0 Proxy. This is very similar to CURLPROXY_HTTP except it uses
     * HTTP/1.0 for any CONNECT tunnelling. It does not change the HTTP version
     * of the actual HTTP requests
     *)
    PROXY_HTTP_1_0                    = Longint(CURLPROXY_HTTP_1_0),

    (**
     * HTTPS Proxy
     *)
    PROXY_HTTPS                       = Longint(CURLPROXY_HTTPS),

    (**
     * SOCKS4 Proxy
     *)
    PROXY_SOCKS4                      = Longint(CURLPROXY_SOCKS4),

    (**
     * SOCKS5 Proxy
     *)
    PROXY_SOCKS5                      = Longint(CURLPROXY_SOCKS5),

    (**
     * SOCKS4a Proxy. Proxy resolves URL hostname
     *)
    PROXY_SOCKS4A                     = Longint(CURLPROXY_SOCKS4A),

    (**
     * SOCKS5 Proxy. Proxy resolves URL hostname.
     *)
    PROXY_SOCKS5_HOSTNAME             = Longint(CURLPROXY_SOCKS5_HOSTNAME)
  );

  TAuthMethod = (
    AUTH_NONE,

    (**
     * HTTP Basic authentication. This is the default choice, and the only
     * method that is in wide-spread use and supported virtually everywhere.
     * This sends the user name and password over the network in plain text,
     * easily captured by others.
     *)
    AUTH_BASIC,

    (**
     * HTTP Digest authentication. Digest authentication is defined in RFC 2617
     * and is a more secure way to do authentication over public networks than
     * the regular old-fashioned Basic method.
     *)
    AUTH_DIGEST,

    (**
     * HTTP Negotiate (SPNEGO) authentication. Negotiate authentication is
     * defined in RFC 4559 and is the most secure way to perform authentication
     * over HTTP.
     *)
    AUTH_NEGOTIATE,

    (**
     * Same as AUTH_NEGOTIATE
     *)
    AUTH_GSSAPI,

    (**
     * HTTP NTLM authentication. A proprietary protocol invented and used by
     * Microsoft. It uses a challenge-response and hash concept similar to
     * Digest, to prevent the password from being eavesdropped.
     *)
    AUTH_NTLM,

    (**
     * HTTP Digest authentication with an IE flavor. Digest authentication is
     * defined in RFC 2617 and is a more secure way to do authentication over
     * public networks than the regular old-fashioned Basic method. The IE
     * flavor is simply that libcurl will use a special "quirk" that IE is known
     * to have used before version 7 and that some servers require the client to
     * use.
     *)
    AUTH_DIGEST_IE,

    (**
     * NTLM delegating to winbind helper. Authentication is performed by a
     * separate binary application that is executed when needed. The name of the
     * application is specified at compile time but is typically
     * /usr/bin/ntlm_auth
     *)
    AUTH_NTLM_WB,

    (**
     * HTTP Bearer token authentication, used primarily in OAuth 2.0 protocol.
     *)
    AUTH_BEARER,

    (**
     * This is sets all bits and thus makes libcurl pick any it finds suitable.
     * libcurl will automatically select the one it finds most secure.
     *)
    AUTH_ANY,

    (**
     * This is sets all bits except Basic and thus makes libcurl pick any it
     * finds suitable. libcurl will automatically select the one it finds most
     * secure.
     *)
    AUTH_ANYSAFE
  );

  TAuthMethods = set of TAuthMethod;

  TTLSAuthMethod = (
    (**
     * TLS-SRP authentication. Secure Remote Password authentication for TLS is
     * defined in RFC 5054 and provides mutual authentication if both sides have
     * a shared secret.
     *)
    SRP
  );

  TPostRedirects = (
    REDIRECT_POST_NONE                = 0,

    (**
     * Tells the library to respect RFC 7231 (section 6.4.2 to 6.4.4) and not
     * convert POST requests into GET requests when following a 301 redirection
     *)
    REDIRECT_POST_301                 = CURL_REDIR_POST_301,

    (**
     * Makes libcurl maintain the request method after a 302 redirect
     *)
    REDIRECT_POST_302                 = CURL_REDIR_POST_302,

    (**
     * Makes libcurl maintain the request method after a 303 redirect
     *)
    REDIRECT_POST_303                 = CURL_REDIR_POST_303,

    REDIRECT_POST_ALL                 = CURL_REDIR_POST_ALL
  );

  { TTimeInterval }

  TTimeIntervalType = (
    tiSeconds,
    tiMilliseconds,
    tiMicroseconds
  );

  TTimeInterval = class
  protected
    FMicroseconds : QWord; (* 1/1 000 000 of second *)

    function  GetSeconds : Double; inline;
    procedure SetSeconds ( s : Double); inline;
    function  GetMilliseconds : Double; inline;
    procedure SetMilliseconds ( ms : Double); inline;
    function  GetMicroseconds : QWord; inline;
    procedure SetMicroseconds ( ms : QWord); inline;
  public
    constructor Create;
    constructor Create (Microseconds : QWord);
    constructor Create (Interval : Double; IntervalType : TTimeIntervalType);

    (**
     * Formats time interval using the format specification
     *
     * The following format specifiers are supported:
     *  0  is a digit place holder. If there is a corresponding digit in the
     *     value being formatted, then it replaces the 0. If not, the 0 is left
     *     as-is.
     *  #  is also a digit place holder. If there is a corresponding digit in
     *     the value being formatted, then it replaces the #. If not, it is
     *     removed
     *  .  determines the location of the decimal point. Only the first '.'
     *     character is taken into account.
     *  ,  determines the use of the thousand separator character in the output
     *     string.
     *)
    function Format (IntervalType : TTimeIntervalType = tiMilliseconds;
      const FormatType : string = '0.000###') : string;

    (* 1 s *)
    property Seconds : Double read GetSeconds write SetSeconds;

    (* 1/1 000 s *)
    property Milliseconds : Double read GetMilliseconds write SetMilliseconds;

    (* 1/1 000 000 s *)
    property Microseconds : QWord read GetMicroseconds
      write SetMicroseconds;
  end;

  { TDataSize }

  TDataSizeType = (
    dsBytes,
    dsKiloBytes,
    dsMegaBytes,
    dsGigaBytes,
    dsTeraBytes
  );

  TDataSize = class
  protected
    FBytes : QWord;

    function  GetBytes : QWord;
    procedure SetBytes (bytes : QWord);
    function  GetKiloBytes : Double;
    procedure SetKiloBytes (Kb : Double);
    function  GetMegaBytes : Double;
    procedure SetMegaBytes (Mb : Double);
    function  GetGigaBytes : Double;
    procedure SetGigaBytes (Gb : Double);
    function  GetTeraBytes : Double;
    procedure SetTeraBytes (Tb : Double);
  public
    constructor Create;
    constructor Create (Bytes : QWord);
    constructor Create (Size : Double; SizeType : TDataSizeType);
    (**
     * Formats data size using the format specification
     *
     * The following format specifiers are supported:
     *  0  is a digit place holder. If there is a corresponding digit in the
     *     value being formatted, then it replaces the 0. If not, the 0 is left
     *     as-is.
     *  #  is also a digit place holder. If there is a corresponding digit in
     *     the value being formatted, then it replaces the #. If not, it is
     *     removed
     *  .  determines the location of the decimal point. Only the first '.'
     *     character is taken into account.
     *  ,  determines the use of the thousand separator character in the output
     *     string.
     *)
    function Format (SizeType : TDataSizeType = dsMegaBytes;
      const FormatType : string = '0.000###') : string;

    (* 1024 Gb *)
    property TeraBytes : Double read GetTeraBytes write SetTeraBytes;
    property TB : Double read GetTeraBytes write SetTeraBytes;

    (* 1024 Mb *)
    property GigaBytes : Double read GetGigaBytes write SetGigaBytes;
    property GB : Double read GetGigaBytes write SetGigaBytes;

    (* 1024 Kb *)
    property MegaBytes : Double read GetMegaBytes write SetMegaBytes;
    property MB : Double read GetMegaBytes write SetMegaBytes;

    (* 1024 b *)
    property KiloBytes : Double read GetKiloBytes write SetKiloBytes;
    property KB : Double read GetKiloBytes write SetKiloBytes;

    (* b *)
    property Bytes : QWord read GetBytes write SetBytes;
    property B : QWord read GetBytes write SetBytes;
  end;

  { TSession }
  { Present cURL session to assign request params }

  (**
   * Callback for writting received data
   *)
  TDownloadFunction = function (buffer : PChar; size : LongWord) : LongWord
    of object;

  (**
   * Callback for data uploads
   *)
  TUploadFunction = function (buffer : PChar; size : LongWord) : LongWord
    of object;

  TSession = class
  protected
    handle : CURL;
    buffer : TStringStream;

    FDownloadFunction : TDownloadFunction;
    FUploadFunction : TUploadFunction;
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
    class function WriteFunctionCallback (ptr : PChar; size : LongWord;
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
    class function ReadFunctionCallback (buf : PChar; size : LongWord;
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

    { Proxy settings }
    procedure SetPreProxy (preProxy : string);
    procedure SetProxy (proxy : string);
    procedure SetProxyPort (port : Longint);
    procedure SetProxyType (proxy : TProxyType);
    procedure SetNoProxyHosts (hosts : string);
    procedure SetHttpProxyTunnel (proxyTunnel : Boolean);
    procedure SetProxyUserPassword (userpwd : string);
    procedure SetProxyUsername (name : string);
    procedure SetProxyPassword (pass : string);
    procedure SetProxyTLSUsername (name : string);
    procedure SetProxyTLSPassword (pass : string);
    procedure SetProxyTLSAuth (method : TTLSAuthMethod);
    procedure SetProxyHTTPAuth (method : Longint);
    procedure SetSOCKS5Auth (AMethod : TAuthMethods);


    procedure SetUserAgent (agent : string);
    procedure SetPort (port : Longint);
    procedure SetFollowRedirect (redirect : boolean);
    procedure SetAutoReferer (updateHeaders : boolean);
    procedure SetIncludeHeader (includeHeader : boolean);
    procedure SetIgnoreContentLength (ignoreLength : boolean);
    procedure SetNoBody (noBody : boolean);
    procedure SetTransferEncoding (encoding : boolean);
    procedure SetVerbose (verbose : boolean);
    procedure SetWildcardMatch (match : boolean);
    procedure SetFailOnError (failOnError : boolean);
    procedure SetKeepSendingOnError (keepSending : boolean);
    procedure SetPathAsIs (pathAsIs : boolean);
    procedure SetLocalPort (port : Longint);
    procedure SetLocalPortRange (range : Longint);
    procedure SetDNSCacheTimeout (timeout : TTimeInterval);
    procedure SetDNSGlobalCache (enable : Boolean);
    procedure SetDNSoverHTTPS (url : string);
    procedure SetBufferSize (size : TDataSize);
    procedure SetTCPFastOpen (fastOpen : Boolean);
    procedure SetTCPNoDelay (noDelay : Boolean);
    procedure SetAddressScope (scope : Longint);
    procedure SetTCPKeepalive (probe : Boolean);
    procedure SetTCPKeepIdle (time : TTimeInterval);
    procedure SetTCPKeepInterval (time : TTimeInterval);
    procedure SetUnixSocketPath (path : string);
    procedure SetAbstractUnixSocketPath (path : string);
    procedure SetUserPassword (userpwd : string);
    procedure SetUsername (name : string);
    procedure SetPassword (pass : string);
    procedure SetLoginOptions (options : string);
    procedure SetHTTPAuth (AMethod : TAuthMethods);
    procedure SetTLSUsername (name : string);
    procedure SetTLSPassword (pass : string);
    procedure SetTLSAuth (method : TTLSAuthMethod);
    procedure SetSASLAuthzid (authzid : string);
    procedure SetSASLIR (send : Boolean);
    procedure SetXOAuth2Bearer (token : string);
    procedure SetAllowUsernameInURL (allow : Boolean);
    procedure SetUnrestrictedAuth (send : Boolean);
    procedure SetMaxRedirects (amount : Longint);
    procedure SetPostRedirect (redir : Longint);
    procedure SetPutMethod (put : Boolean);
    procedure SetPostMethod (post : Boolean);
    procedure SetPostFields (data : string);
    procedure SetPostFieldsSize (size : Longint);
    procedure SetPostFieldsSizeLarge (size : LongWord);
    procedure SetAllowedProtocols (AProtocols : TProtocols);
    procedure SetAllowedRedirectProtocols (AProtocols : TProtocols);
    procedure SetDefaultProtocol (AProtocol : TProtocol);
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
     * Set pre-proxy to use
     *
     * Set the preproxy to use for the upcoming request. The parameter should be
     * a string holding the host name or dotted numerical IP address. A
     * numerical IPv6 address must be written within [brackets].
     * To specify port number in this string, append :[port] to the end of the
     * host name. The proxy's port number may optionally be specified with the
     * separate option Proxy. If not specified, libcurl will default to using
     * port 1080 for proxies.
     * A pre proxy is a SOCKS proxy that curl connects to before it connects to
     * the HTTP(S) proxy specified in the CURLOPT_PROXY option. The pre proxy
     * can only be a SOCKS proxy.
     * The pre proxy string should be prefixed with [scheme]:// to specify which
     * kind of socks is used. Use socks4://, socks4a://, socks5:// or socks5h://
     * (the last one to enable socks5 and asking the proxy to do the resolving,
     * also known as CURLPROXY_SOCKS5_HOSTNAME type) to request the specific
     * SOCKS version to be used. Otherwise SOCKS4 is used as default.
     * Setting the pre proxy string to "" (an empty string) will explicitly
     * disable the use of a pre proxy.
     *)
    property PreProxy : string write SetPreProxy;

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

    (**
     * Set verbose mode on/off
     *
     * Make the library display a lot of verbose information about its
     * operations. Very useful for libcurl and/or protocol debugging and
     * understanding. The verbose information will be sent to stderr.
     *)
    property VerboseMode : Boolean write SetVerbose default False;

    (**
     * Enable directory wildcard transfers
     * [This feature is only supported for FTP download]
     *
     * Transfer multiple files according to a file name pattern. The pattern can
     * be specified as part of the Url option, using an fnmatch-like pattern
     * (Shell Pattern Matching) in the last part of URL (file name).
     *
     * A brief introduction of its syntax follows:
     *
     * * - ASTERISK
     * ftp://example.com/some/path/*.txt (for all txt's from the root
     * directory). Only two asterisks are allowed within the same pattern
     * string.
     *
     * ? - QUESTION MARK
     * Question mark matches any (exactly one) character.
     * ftp://example.com/some/path/photo?.jpeg
     *
     * [ - BRACKET EXPRESSION
     * The left bracket opens a bracket expression. The question mark and
     * asterisk have no special meaning in a bracket expression. Each bracket
     * expression ends by the right bracket and matches exactly one character.
     * Some examples follow:
     * [a-zA-Z0-9] or [f-gF-G] - character interval
     * [abc] - character enumeration
     * [^abc] or [!abc] - negation
     * [[:name:]] class expression. Supported classes are alnum,lower, space,
     * alpha, digit, print, upper, blank, graph, xdigit.
     * [][-!^] - special case - matches only '-', ']', '[', '!' or '^'. These
     * characters have no special purpose.
     * [\[\]\\] - escape syntax. Matches '[', ']' or '´.
     * Using the rules above, a file name pattern can be constructed:
     * ftp://example.com/some/path/[a-z[:upper:]\\].jpeg
     *)
    property WildcardMatch : Boolean write SetWildcardMatch;

    (**
     * Request failure on HTTP response >= 400
     *
     * Tells the library to fail the request if the HTTP code returned is equal
     * to or larger than 400. The default action would be to return the page
     * normally, ignoring that code.
     *)
    property FailOnError : Boolean write SetFailOnError default False;

    (**
     * Keep sending on early HTTP response >= 300
     *
     * Tells the library to keep sending the request body if the HTTP code
     * returned is equal to or larger than 300. The default action would be to
     * stop sending and close the stream or connection.
     *)
    property KeepSendingOnError : Boolean write SetKeepSendingOnError
      default False;

    (**
     * Do not handle dot dot sequences
     *
     * Tell libcurl to not alter the given path before passing it on to the
     * server.
     * This instructs libcurl to NOT squash sequences of "/../" or "/./" that
     * may exist in the URL's path part and that is supposed to be removed
     * according to RFC 3986 section 5.2.4.
     * Some server implementations are known to (erroneously) require the dot
     * dot sequences to remain in the path and some clients want to pass these
     * on in order to try out server implementations.
     * By default libcurl will merge such sequences before using the path.
     *)
    property PathAsIs : Boolean write SetPathAsIs default False;

    (**
     * Proxy protocol type
     *)
    property ProxyProtocol : TProxyType write SetProxyType default PROXY_HTTP;

    (**
     * Disable proxy use for specific hosts
     *
     * The string consists of a comma separated list of host names that do not
     * require a proxy to get reached, even if one is specified. The only
     * wildcard available is a single * character, which matches all hosts, and
     * effectively disables the proxy. Each name in this list is matched as
     * either a domain which contains the hostname, or the hostname itself. For
     * example, example.com would match example.com, example.com:80, and
     * www.example.com, but not www.notanexample.com or example.com.othertld.
     * If the name in the noproxy list has a leading period, it is a domain
     * match against the provided host name. This way ".example.com" will switch
     * off proxy use for both "www.example.com" as well as for
     * "foo.example.com".
     * Setting the noproxy string to "" (an empty string) will explicitly enable
     * the proxy for all host names, even if there is an environment variable
     * set for it.
     * Enter IPv6 numerical addresses in the list of host names without
     * enclosing brackets: "example.com,::1,localhost"
     *)
    property NoProxyHosts : string write SetNoProxyHosts;

    (**
     * Tunnel through HTTP proxy
     *
     * Make libcurl tunnel all operations through the HTTP proxy (set with
     * Proxy property). There is a big difference between using a proxy and to
     * tunnel through it.
     * Tunneling means that an HTTP CONNECT request is sent to the proxy, asking
     * it to connect to a remote host on a specific port number and then the
     * traffic is just passed through the proxy. Proxies tend to white-list
     * specific port numbers it allows CONNECT requests to and often only port
     * 80 and 443 are allowed.
     *)
    property HttpProxyTunnel : Boolean write SetHttpProxyTunnel;

    (**
     * Set local port number to use for socket
     *
     * This sets the local port number of the socket used for the connection.
     * 0, disabled - use whatever the system thinks is fine
     *)
    property LocalPort : Longint write SetLocalPort default 0;

    (**
     * Number of additional local ports to try
     *
     * Pass a long. The range argument is the number of attempts libcurl will
     * make to find a working local port number. It starts with the given
     * LocalPort and adds one to the number for each retry. Setting this option
     * to 1 or below will make libcurl do only one try for the exact port
     * number. Port numbers by nature are scarce resources that will be busy at
     * times so setting this value to something too low might cause unnecessary
     * connection setup failures.
     *)
    property LocalPortRange : Longint write SetLocalPortRange default 1;

    (**
     * Set life-time for DNS cache entries
     *
     * Name resolves will be kept in memory and used for this time interval.
     * Set to zero to completely disable caching.
     *)
    property DNSCacheTimeout : TTimeInterval write SetDNSCacheTimeout;

    (**
     * Enable/disable global DNS cache
     *
     * Tells curl to use a global DNS cache that will survive between easy
     * handle creations and deletions. This is not thread-safe and this will use
     * a global variable.
     *
     * WARNING: this option is considered obsolete. Stop using it. Switch over
     * to using the share interface instead!
     *)
    property DNSGlobalCache : Boolean write SetDNSGlobalCache;

    (**
     * Provide the DNS-over-HTTPS URL
     *
     * Pass in a string to a URL for the DOH server to use for name resolving.
     * The parameter should be a string which must be URL-encoded in the
     * following format: "https://host:port/path". It MUST specify a HTTPS URL.
     * Disable DOH use again by setting this option to '' (empty string).
     *)
    property DNSoverHTTPS : string write SetDNSoverHTTPS;

    (**
     * Set preffered receive buffer size
     *
     * Specifying your preferred size for the receive buffer in libcurl. The
     * main point of this would be that the write callback gets called more
     * often and with smaller chunks. Secondly, for some protocols, there's a
     * benefit of having a larger buffer for performance.
     * The minimum buffer size allowed to be set is 1024 bytes.
     *)
    property BufferSize : TDataSize write SetBufferSize;

    (**
     * Enable/disable TCP Fast Open
     *
     * TCP Fast Open (RFC7413) is a mechanism that allows data to be carried in
     * the SYN and SYN-ACK packets and consumed by the receiving end during the
     * initial connection handshake, saving up to one full round-trip time
     * (RTT).
     *)
    property TCPFastOpen : Boolean write SetTCPFastOpen default False;

    (**
     * Disable TCP's Nagle algorithm on this connection
     *
     * The purpose of this algorithm is to try to minimize the number of small
     * packets on the network (where "small packets" means TCP segments less
     * than the Maximum Segment Size (MSS) for the network).
     * Maximizing the amount of data sent per TCP segment is good because it
     * amortizes the overhead of the send. However, in some cases small segments
     * may need to be sent without delay. This is less efficient than sending
     * larger amounts of data at a time, and can contribute to congestion on the
     * network if overdone.
     *)
    property TCPNoDelay : Boolean write SetTCPNoDelay default True;

    (**
     * Set scope id for IPv6 addresses
     *
     * Pass a long specifying the scope id value to use when connecting to IPv6
     * addresses.
     *)
    property AddressScope : Longint write SetAddressScope default 0;

    (**
     * Enable/disable TCP keep-alive probing
     *
     * If set, TCP keepalive probes will be sent. The delay and frequency of
     * these probes can be controlled by the TCPKeepIdle and TCPKeepInterval
     * options, provided the operating system supports them. Set to False
     * (default behavior) to disable keepalive probes
     *)
    property TCPKeepalive : Boolean write SetTCPKeepalive default False;

    (**
     * Set TCP keep-alive idle time wait
     *
     * Sets the delay, that the operating system will wait while the connection
     * is idle before sending keepalive probes. Not all operating systems
     * support this option.
     *)
    property TCPKeepIdle : TTimeInterval write SetTCPKeepIdle;

    (**
     * Set TCP keep-alive interval
     *
     * Sets the interval, that the operating system will wait between sending
     * keepalive probes. Not all operating systems support this option.
     *)
    property TCPKeepInterval : TTimeInterval write SetTCPKeepInterval;

    (**
     * Set UNIX domain socket
     *
     * Enables the use of Unix domain sockets as connection endpoint and sets
     * the path to path. If path is '' (empty string), then Unix domain sockets
     * are disabled.
     * When enabled, curl will connect to the Unix domain socket instead of
     * establishing a TCP connection to a host. Since no TCP connection is
     * created, curl does not need to resolve the DNS hostname in the URL.
     * The maximum path length on Cygwin, Linux and Solaris is 107. On other
     * platforms it might be even less.
     *)
    property UnixSocketPath : string write SetUnixSocketPath;

    (**
     * Set an abstract Unix domain socket
     *
     * Enables the use of an abstract Unix domain socket instead of establishing
     * a TCP connection to a host.
     *)
    property AbstractUnixSocketPath : string write SetAbstractUnixSocketPath;

    (**
     * User name and password to use in authentification
     *
     * Login details string for the connection. The format of which is:
     * [user name]:[password].
     * When using Kerberos V5 authentication with a Windows based server, you
     * should specify the user name part with the domain name in order for the
     * server to successfully obtain a Kerberos Ticket. If you don't then the
     * initial part of the authentication handshake may fail.
     * When using NTLM, the user name can be specified simply as the user name
     * without the domain name should the server be part of a single domain and
     * forest.
     * To specify the domain name use either Down-Level Logon Name or UPN (User
     * Principal Name) formats. For example, EXAMPLE\user and user@example.com
     * respectively.
     * When using HTTP and FollowLocation, libcurl might perform several
     * requests to possibly different hosts. libcurl will only send this user
     * and password information to hosts using the initial host name, so if
     * libcurl follows locations to other hosts it will not send the user and
     * password to those. This is enforced to prevent accidental information
     * leakage.
     *)
    property UserPassword : string write SetUserPassword;

    (**
     * User name and password to use for proxy authentification
     *
     * Pass a parameter, which should be [user name]:[password] to use for the
     * connection to the HTTP proxy. Both the name and the password will be URL
     * decoded before use, so to include for example a colon in the user name
     * you should encode it as %3A. (This is different to how UserPassword is
     * used - beware.)
     *)
    property ProxyUserPasswod : string write SetProxyUserPassword;

    (**
     * User name to use in authentication
     *
     * Sets the user name to be used in protocol authentication. You should not
     * use this option together with the (older) UserPassword option.
     * When using Kerberos V5 authentication with a Windows based server, you
     * should include the domain name in order for the server to successfully
     * obtain a Kerberos Ticket. If you don't then the initial part of the
     * authentication handshake may fail.
     * When using NTLM, the user name can be specified simply as the user name
     * without the domain name should the server be part of a single domain and
     * forest.
     * To include the domain name use either Down-Level Logon Name or UPN (User
     * Principal Name) formats. For example, EXAMPLE\user and user@example.com
     * respectively.
     *)
    property Username : string write SetUsername;

    (**
     * Password to use in authentication
     *
     * The Password option should be used in conjunction with the Username
     * option.
     *)
    property Password : string write SetPassword;

    (**
     * Set login options
     *
     * For more information about the login options please see RFC 2384, RFC
     * 5092 and IETF draft draft-earhart-url-smtp-00.txt
     * LoginOptions can be used to set protocol specific login options, such as
     * the preferred authentication mechanism via "AUTH=NTLM" or "AUTH=*", and
     * should be used in conjunction with the Username option.
     *)
    property LoginOptions : string write SetLoginOptions;

    (**
     * User name to use for proxy authentication
     *
     * Sets the user name to be used in protocol authentication with the proxy.
     *)
    property ProxyUsername : string write SetProxyUsername;

    (**
     * Password to use with proxy authentication
     *
     * The option should be used in conjunction with the ProxyUsername option.
     *)
    property ProxyPassword : string write SetProxyPassword;

    (**
     * Tell libcurl which authentication method(s) you want it to use speaking
     * to the remote server.
     *)
    property HTTPAuth : TAuthMethods write SetHTTPAuth default [AUTH_BASIC];

    (**
     * User name to use for TLS authentication
     *)
    property TLSUsername : string write SetTLSUsername;

    (**
     * User name to use for proxy TLS authentication
     *)
    property ProxyTLSUsername : string write SetProxyTLSUsername;

    (**
     * Password to use for TLS authentication
     *
     * Requires that the TLSUsername option also be set.
     *)
    property TLSPassword : string write SetTLSPassword;

    (**
     * Password to use for proxy TLS
     *
     * Requires that the ProxyTLSUsername option also be set.
     *)
    property ProxyTLSPassword : string write SetProxyTLSPassword;

    (**
     * Set TLS authentication methods
     *)
    property TLSAuth : TTLSAuthMethod write SetTLSAuth;

    (**
     * Set proxy TLS authentication methods
     *)
    property ProxyTLSAuth : TTLSAuthMethod write SetProxyTLSAuth;

    (**
     * Set HTTP proxy authentication methods to try
     *
     * Tell libcurl which HTTP authentication method(s) you want it to use for
     * your proxy authentication. If more than one bit is set, libcurl will
     * first query the site to see what authentication methods it supports and
     * then pick the best one you allow it to use. For some methods, this will
     * induce an extra network round-trip.
     *)
    property ProxyHTTPAuth : Longint write SetProxyHTTPAuth;

    (**
     * Authorisation identity (identity to act as)
     *
     * Authorisation identity (authzid) for the transfer. Only applicable to the
     * PLAIN SASL authentication mechanism where it is optional.
     * When not specified only the authentication identity (authcid) as
     * specified by the username will be sent to the server, along with the
     * password. The server will derive a authzid from the authcid when not
     * provided, which it will then uses internally.
     * When the authzid is specified, the use of which is server dependent, it
     * can be used to access another user's inbox, that the user has been
     * granted access to, or a shared mailbox for example.
     *)
    property SASLAuthzid : string write SetSASLAuthzid;

    (**
     * Enable/disable sending initial response in first packet
     *
     * curl will send the initial response to the server in the first
     * authentication packet in order to reduce the number of ping pong
     * requests. Only applicable to the following supporting SASL authentication
     * mechanisms:
     * Login * Plain * GSSAPI * NTLM * OAuth 2.0
     *)
    property SASLInitialResponse : Boolean write SetSASLIR default False;

    (**
     * Specify OAuth 2.0 access token
     *
     * OAuth 2.0 Bearer Access Token for use with HTTP, IMAP, POP3 and SMTP
     * servers that support the OAuth 2.0 Authorization Framework.
     *)
    property XOAuth2BearerToken : string write SetXOAuth2Bearer;

    (**
     * Allow/disallow specifying user name in the url
     *)
    property AllowUsernameInURL : Boolean write SetAllowUsernameInURL
      default True;

    (**
     * Send credentials to other hosts too
     *
     * Make libcurl continue to send authentication (user+password) credentials
     * when following locations, even when hostname changed.
     * By default, libcurl will only send given credentials to the initial host
     * name as given in the original URL, to avoid leaking username + password
     * to other sites.
     *)
    property UnrestrictedAuth : Boolean write SetUnrestrictedAuth;

    (**
     * Meximum numbers of redirects allowed
     *
     * Setting the limit to 0 will make libcurl refuse any redirect.
     * Set it to -1 for an infinite number of redirects.
     *)
    property MaxRedirects : Longint write SetMaxRedirects default -1;

    (**
     * How to act on an HTTP POST redirect
     *)
    property PostRedirect : Longint write SetPostRedirect;

    (**
     * Make an HTTP PUT request
     *
     * This option is deprecated since version 7.12.1. Use Upload!
     *)
    property Put : Boolean write SetPutMethod;

    (**
     * Request an HTTP POST
     *)
    property Post : Boolean write SetPostMethod default False;

    (**
     * Specify data to POST to server
     *
     * Pass the full data to send in an HTTP POST operation. You must make sure
     * that the data is formatted the way you want the server to receive it.
     * libcurl will not convert or encode it for you in any way. For example,
     * the web server may assume that this data is url-encoded.
     *)
    property PostFields : string write SetPostFields;

    (**
     * Size of POST data
     *
     * If you want to post data to the server without having libcurl do a
     * strlen() to measure the data size, this option must be used. When this
     * option is used you can post fully binary data, which otherwise is likely
     * to fail. If this size is set to -1, the library will use strlen() to get
     * the size.
     * If you post more than 2GB, use PostFieldsSizeLarge.
     *)
    property PostFieldsSize : Longint write SetPostFieldsSize default -1;

    (**
     * Size of POST data
     *
     * If you want to post data to the server without having libcurl do a
     * strlen() to measure the data size, this option must be used. When this
     * option is used you can post fully binary data, which otherwise is likely
     * to fail. If this size is set to -1, the library will use strlen() to get
     * the size.
     *)
    property PostFieldsSizeLarge : LongWord write SetPostFieldsSizeLarge
      default -1;

    (**
     * Set allowed protocols
     *
     * Limits what protocols libcurl may use in the transfer. This allows you to
     * have a libcurl built to support a wide range of protocols but still limit
     * specific transfers to only be allowed to use a subset of them. By default
     * libcurl will accept all protocols it supports
     *)
    property Protocols : TProtocols write SetAllowedProtocols
      default [PROTOCOL_DICT, PROTOCOL_FILE, PROTOCOL_FTP, PROTOCOL_FTPS,
      PROTOCOL_GOPHER, PROTOCOL_HTTP, PROTOCOL_HTTPS, PROTOCOL_IMAP,
      PROTOCOL_IMAPS, PROTOCOL_LDAP, PROTOCOL_LDAPS, PROTOCOL_POP3,
      PROTOCOL_POP3S, PROTOCOL_RTMP, PROTOCOL_RTMPE, PROTOCOL_RTMPS,
      PROTOCOL_RTMPT, PROTOCOL_RTMPTE, PROTOCOL_RTMPTS, PROTOCOL_RTSP,
      PROTOCOL_SCP, PROTOCOL_SFTP, PROTOCOL_SMB, PROTOCOL_SMBS, PROTOCOL_SMTP,
      PROTOCOL_SMTPS, PROTOCOL_TELNET, PROTOCOL_TFTP];

    (**
     * Set protocols allowed to redirect to
     *
     * Limits what protocols libcurl may use in a transfer that it follows to in
     * a redirect when FollowRedirect is enabled. This allows you to limit
     * specific transfers to only be allowed to use a subset of protocols in
     * redirections.
     *)
    property RedirectProtocols : TProtocols write SetAllowedRedirectProtocols
      default [PROTOCOL_HTTP, PROTOCOL_HTTPS, PROTOCOL_FTP, PROTOCOL_FTPS];

    (**
     * Default protocol to use if the URL is missing a scheme name
     *
     * This option does not change the default proxy protocol (http).
     * Without this option libcurl would make a guess based on the host.
     *)
    property DefaultProtocol : TProtocol write SetDefaultProtocol;

    (**
     * Set allowed methods for SOCKS5 proxy authentication
     *
     * Tell libcurl which authentication method(s) are allowed for SOCKS5 proxy
     * authentication. The only supported flags are AUTH_BASIC, which allows
     * username/password authentication, AUTH_GSSAPI, which allows GSS-API
     * authentication, and AUTH_NONE, which allows no authentication.
     *)
    property SOCKS5Auth : TAuthMethods write SetSOCKS5Auth
      default [AUTH_BASIC, AUTH_GSSAPI];
  public

    (**
     * Callback for writing received data
     *
     * This callback function gets called by libcurl as soon as there is data
     * received that needs to be saved.
     * This option shares the same semantics as UnixSocketPath in which
     * documentation more details can be found. Internally, these two options
     * share the same storage and therefore only one of them can be set per
     * handle.
     *)
    property OnDownload : TDownloadFunction read FDownloadFunction
      write FDownloadFunction;

    (**
     * Callback for data uploads
     *
     * This callback function gets called by libcurl as soon as it needs to read
     * data in order to send it to the peer - like if you ask it to upload or
     * post data to the server.
     *)
    property OnUpload : TUploadFunction read FUploadFunction
      write FUploadFunction;
  end;

  { TSessionInfo }
  { Getting information from server response. }

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
    function GetRedirectUrl : string;
    function GetContentType : string;
    function GetPrimaryIP : string;
    function GetLocalIP : string;
    function GetResponseCode : TStatusCode;
    function GetContent : string;
    function GetVerifySSLResult : boolean;
    function GetVerifySSLProxyResult : boolean;
    function GetConnectResponseCode : TStatusCode;
    function GetHttpVersion : HTTPVersionCode;
    function GetRedirectCount : Longint;
    function GetUploaded : TDataSize;
    function GetDownloaded : TDataSize;
    function GetDownloadSpeed : TDataSize;
    function GetUploadSpeed : TDataSize;
    function GetHeaderSize : TDataSize;
    function GetRequestSize : TDataSize;
    function GetContentLengthDownload : LongWord;
    function GetContentLengthUpload : LongWord;
    function GetNumConnects : Longint;
    function GetPrimaryPort : Longint;
    function GetLocalPort : Longint;
    function GetFileTime : time_t;
    function GetTotalTime : TTimeInterval;
    function GetNameLookup : TTimeInterval;
    function GetConnectTime : TTimeInterval;
    function GetAppConnectTime : TTimeInterval;
    function GetPretransferTime : TTimeInterval;
    function GetStartTransferTime : TTimeInterval;
    function GetRedirectTime : TTimeInterval;
    function GetRetryAfterDelay : TTimeInterval;
    function GetOsErrno : Longint;
    function GetLastSocket : Longint;
    function GetActiveSocket : curl_socket_t;
    function GetFTPEntryPath : string;
    function GetConditionUnmet : Boolean;
    function GetRTSPSessionID : string;
    function GetRTSPClientCSeq : Longint;
    function GetRTSPServerCSeq : Longint;
    function GetRTSPReceivedCSeq : Longint;
    function GetScheme : string;
  public
    constructor Create (var s : TSession);

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
    property ResponseCode : TStatusCode read GetResponseCode;

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
    property ConnectResponseCode : TStatusCode read GetConnectResponseCode;

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
    property Uploaded : TDataSize read GetUploaded;

    (**
     * Get the number of downloaded bytes
     *)
    property Downloaded : TDataSize read GetDownloaded;

    (**
     * Get download speed per second
     *)
    property DownloadSpeed : TDataSize read GetDownloadSpeed;

    (**
     * Get upload speed per second
     *)
    property UploadSpeed : TDataSize read GetUploadSpeed;

    (**
     * Get size of retrieved headers
     *)
    property HeaderSize : TDataSize read GetHeaderSize;

    (**
     * Get size of sent request
     *)
    property RequestSize : TDataSize read GetRequestSize;

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
    property TotalTime : TTimeInterval read GetTotalTime;

    (**
     * Get the name lookup time
     *)
    property NameLookup : TTimeInterval read GetNameLookup;

    (**
     * Get the time until connect
     *)
    property ConnectTime : TTimeInterval read GetConnectTime;

    (**
     * Get the time until the SSL/SSH handshake is completed
     *
     * When a redirect is followed, the time from each request is added together.
     *)
    property AppConnectTime : TTimeInterval read GetAppConnectTime;

    (**
     * Get the time until the file transfer start
     *
     * When a redirect is followed, the time from each request is added together.
     *)
    property PretransferTime : TTimeInterval read GetPretransferTime;

    (**
     * Get time until the first byte is received
     *
     * When a redirect is followed, the time from each request is added together.
     *)
    property StartTransferTime : TTimeInterval read GetStartTransferTime;

    (**
     * Get the time for all redirection steps
     *
     * When a redirect is followed, the time from each request is added together.
     *)
    property RedirectTime : TTimeInterval read GetRedirectTime;

    (**
     * Returns the Retry-After retry delay
     *
     * The information from the "Retry-After:" header. Returns zero delay if
     * there was no header.
     *)
    property RetryAfterDelay : TTimeInterval read GetRetryAfterDelay;

    (**
     * Get errno number from last connect failure
     *)
    property OsErrno : Longint read GetOsErrno;

    (**
     * Get the last socket used
     *
     * If the socket is no longer valid, -1 is returned. When you finish working
     * with the socket, you must call curl_easy_cleanup() as usual and let
     * libcurl close the socket and cleanup other resources associated with the
     * handle.
     *)
    property LastSocket : Longint read GetLastSocket;

    (**
     * Get the active socket
     *)
    property ActiveSocket : curl_socket_t read GetActiveSocket;

    (**
     * Get entry path in FTP server
     *)
    property FTPEntryPath : string read GetFTPEntryPath;

    (**
     * Get info on unmet time conditional
     *
     * Receive the TRUE if the condition provided in the previous request didn't
     * match. Alas, if this returns a TRUE you know that the reason you didn't
     * get data in return is because it didn't fulfill the condition.
     *)
    property ConditionUnmet : Boolean read GetConditionUnmet;

    (**
     * Get RTSP session ID
     *
     * Applications wishing to resume an RTSP session on another connection
     * should retrieve this info before closing the active connection.
     *)
    property RTSPSessionId : string read GetRTSPSessionId;

    (**
     * Get the next RTSP client CSeq
     *
     * Receive the next CSeq that will be used by the application.
     *)
    property RTSPClientCSeq : Longint read GetRTSPClientCSeq;

    (**
     * Get the next RTSP server CSeq
     *)
    property RTSPServerCSeq : Longint read GetRTSPServerCSeq;

    (**
     * Get the recently received CSeq
     *)
    property RTSPReceivedCSeq : Longint read GetRTSPReceivedCSeq;

    (**
     * Get the URL scheme (sometimes called protocol) used in the connection
     *)
    property Scheme : string read GetScheme;
  end;

implementation

{ TDataSize }

function TDataSize.GetBytes: QWord;
begin
  Result := FBytes;
end;

procedure TDataSize.SetBytes(bytes: QWord);
begin
  FBytes := bytes;
end;

function TDataSize.GetKiloBytes: Double;
begin
  Result := FBytes / 1024;
end;

procedure TDataSize.SetKiloBytes(Kb: Double);
begin
  FBytes := QWord(Kb * 1024);
end;

function TDataSize.GetMegaBytes: Double;
begin
  Result := FBytes / (1024 * 1024);
end;

procedure TDataSize.SetMegaBytes(Mb: Double);
begin
  FBytes := QWord(Mb * (1024 * 1024));
end;

function TDataSize.GetGigaBytes: Double;
begin
  Result := FBytes / (1024 * 1024 * 1024);
end;

procedure TDataSize.SetGigaBytes(Gb: Double);
begin
  FBytes := QWord(Gb * (1024 * 1024 * 1024));
end;

function TDataSize.GetTeraBytes: Double;
begin
  Result := FBytes / (1024 * 1024 * 1024 * 1024);
end;

procedure TDataSize.SetTeraBytes(Tb: Double);
begin
  FBytes := QWord(Tb * (1024 * 1024 * 1024 * 1024));
end;

constructor TDataSize.Create;
begin
  // do nothing!
end;

constructor TDataSize.Create(Bytes: QWord);
begin
  FBytes := Bytes;
end;

constructor TDataSize.Create(Size: Double; SizeType: TDataSizeType);
begin
  case SizeType of
    dsBytes : Bytes := QWord(Ceil(Size));
    dsKiloBytes : KiloBytes := Size;
    dsMegaBytes : MegaBytes := Size;
    dsGigaBytes : GigaBytes := Size;
    dsTeraBytes : TeraBytes := Size;
  end;
end;

function TDataSize.Format(SizeType: TDataSizeType;
  const FormatType: string): string;
begin
  case SizeType of
    dsBytes : Result := FormatFloat(FormatType, Double(Bytes));
    dsKiloBytes : Result := FormatFloat(FormatType, KiloBytes);
    dsMegaBytes : Result := FormatFloat(FormatType, MegaBytes);
    dsGigaBytes : Result := FormatFloat(FormatType, GigaBytes);
    dsTeraBytes : Result := FormatFloat(FormatType, TeraBytes);
  end;
end;

{ TTimeInterval }

function TTimeInterval.GetSeconds: Double;
begin
  Result := FMicroseconds / 1000000;
end;

procedure TTimeInterval.SetSeconds(s: Double);
begin
  FMicroseconds := QWord(s * 1000000);
end;

function TTimeInterval.GetMilliseconds: Double;
begin
  Result := FMicroseconds / 1000;
end;

procedure TTimeInterval.SetMilliseconds(ms: Double);
begin
  FMicroseconds := QWord(ms * 1000);
end;

function TTimeInterval.GetMicroseconds: QWord;
begin
  Result := FMicroseconds;
end;

procedure TTimeInterval.SetMicroseconds(ms: QWord);
begin
  FMicroseconds := ms;
end;

constructor TTimeInterval.Create;
begin
  // do nothing!
end;

constructor TTimeInterval.Create(Microseconds: QWord);
begin
  FMicroseconds := Microseconds;
end;

constructor TTimeInterval.Create(Interval: Double;
  IntervalType: TTimeIntervalType);
begin
  case IntervalType of
    tiMicroseconds : Microseconds := QWord(Ceil(Interval));
    tiMilliseconds : Milliseconds := Interval;
    tiSeconds : Seconds := Interval;
  end;
end;

function TTimeInterval.Format(IntervalType: TTimeIntervalType;
  const FormatType: string): string;
begin
  case IntervalType of
    tiSeconds : Result := FormatFloat(FormatType, Seconds);
    tiMilliseconds : Result := FormatFloat(FormatType, Milliseconds);
    tiMicroseconds : Result := FormatFloat(FormatType, Double(Microseconds));
  end;
end;

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
end;

function TSessionInfo.GetEffectiveUrl: string;
var
  url : PChar;
begin
  if Opened then
  begin
    New(url);
    curl_easy_getinfo(session.handle, CURLINFO_EFFECTIVE_URL, @url);
    Result := url;
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
    Result := url;
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
    Result := content_type;
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
    Result := ip;
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
    Result := ip;
  end;
end;

function TSessionInfo.GetResponseCode: TStatusCode;
var
  code : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_RESPONSE_CODE, @code);
    Result := TStatusCode(code);
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

function TSessionInfo.GetConnectResponseCode: TStatusCode;
var
  code : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_HTTP_CONNECTCODE, @code);
    Result := TStatusCode(code);
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

function TSessionInfo.GetUploaded: TDataSize;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_SIZE_UPLOAD_T, @bytes);
    Result := TDataSize.Create(bytes);
  end;
end;

function TSessionInfo.GetDownloaded: TDataSize;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_SIZE_DOWNLOAD_T, @bytes);
    Result := TDataSize.Create(bytes);
  end;
end;

function TSessionInfo.GetDownloadSpeed: TDataSize;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_SPEED_DOWNLOAD_T, @bytes);
    Result := TDataSize.Create(bytes);
  end;
end;

function TSessionInfo.GetUploadSpeed: TDataSize;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_SPEED_UPLOAD_T, @bytes);
    Result := TDataSize.Create(bytes);
  end;
end;

function TSessionInfo.GetHeaderSize: TDataSize;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_HEADER_SIZE, @bytes);
    Result := TDataSize.Create(bytes);
  end;
end;

function TSessionInfo.GetRequestSize: TDataSize;
var
  bytes : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_REQUEST_SIZE, @bytes);
    Result := TDataSize.Create(bytes);
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

function TSessionInfo.GetTotalTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_TOTAL_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TSessionInfo.GetNameLookup: TTimeInterval;
var
  time : Longword;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_NAMELOOKUP_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TSessionInfo.GetConnectTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_CONNECT_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TSessionInfo.GetAppConnectTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_APPCONNECT_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TSessionInfo.GetPretransferTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_PRETRANSFER_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TSessionInfo.GetStartTransferTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_STARTTRANSFER_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TSessionInfo.GetRedirectTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_REDIRECT_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TSessionInfo.GetRetryAfterDelay: TTimeInterval;
var
  delay : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_RETRY_AFTER, @delay);
    Result := TTimeInterval.Create(Double(delay), tiSeconds);
  end;
end;

function TSessionInfo.GetOsErrno: Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_OS_ERRNO, @Result);
  end;
end;

function TSessionInfo.GetLastSocket: Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_LASTSOCKET, @Result);
  end;
end;

function TSessionInfo.GetActiveSocket: curl_socket_t;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_ACTIVESOCKET, @Result);
  end;
end;

function TSessionInfo.GetFTPEntryPath: string;
var
  path : PChar;
begin
  if Opened then
  begin
    New(path);
    curl_easy_getinfo(session.handle, CURLINFO_FTP_ENTRY_PATH, @path);
    Result := path;
  end;
end;

function TSessionInfo.GetConditionUnmet: Boolean;
var
  unmet : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_CONDITION_UNMET, @unmet);
    Result := Boolean(unmet);
  end;
end;

function TSessionInfo.GetRTSPSessionID: string;
var
  id : PChar;
begin
  if Opened then
  begin
    New(id);
    curl_easy_getinfo(session.handle, CURLINFO_RTSP_SESSION_ID, @id);
    Result := id;
  end;
end;

function TSessionInfo.GetRTSPClientCSeq: Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_RTSP_CLIENT_CSEQ, @Result);
  end;
end;

function TSessionInfo.GetRTSPServerCSeq: Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_RTSP_SERVER_CSEQ, @Result);
  end;
end;

function TSessionInfo.GetRTSPReceivedCSeq: Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.handle, CURLINFO_RTSP_CSEQ_RECV, @Result);
  end;
end;

function TSessionInfo.GetScheme: string;
var
  sc : PChar;
begin
  if Opened then
  begin
    New(sc);
    curl_easy_getinfo(session.handle, CURLINFO_SCHEME, @sc);
    Result := sc;
  end;
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

class function TSession.WriteFunctionCallback (ptr: PChar; size: LongWord;
  nmemb: LongWord; data: Pointer): LongWord; cdecl;
begin
  if Assigned(TSession(data).FDownloadFunction) then
  begin
    Result := TSession(data).FDownloadFunction(ptr, size);
  end else
  begin
    Result := TSession(data).Write(ptr, size, nmemb);
  end;
end;

class function TSession.ReadFunctionCallback (buf: PChar; size: LongWord;
  nitems: LongWord; data: Pointer): LongWord; cdecl;
begin
  if Assigned(TSession(data).FUploadFunction) then
  begin
    Result := TSession(data).FUploadFunction(buf, size);
  end else
  begin
    Result := TSession(data).Read(buf, size, nitems);
  end;
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
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION,
      @TSession.WriteFunctionCallback);
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

procedure TSession.SetPreProxy(preProxy: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PRE_PROXY, PChar(preProxy));
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

procedure TSession.SetVerbose(verbose: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_VERBOSE, Longint(verbose));
  end;
end;

procedure TSession.SetWildcardMatch(match: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_WILDCARDMATCH, Longint(match));
  end;
end;

procedure TSession.SetFailOnError(failOnError: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_FAILONERROR, Longint(failOnError));
  end;
end;

procedure TSession.SetKeepSendingOnError(keepSending: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_KEEP_SENDING_ON_ERROR,
      Longint(keepSending));
  end;
end;

procedure TSession.SetPathAsIs(pathAsIs: boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PATH_AS_IS, Longint(pathAsIs));
  end;
end;

procedure TSession.SetProxyType(proxy: TProxyType);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PROXYTYPE, Longint(proxy));
  end;
end;

procedure TSession.SetNoProxyHosts(hosts: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_NOPROXY, PChar(hosts));
  end;
end;

procedure TSession.SetHttpProxyTunnel(proxyTunnel: Boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_HTTPPROXYTUNNEL, Longint(proxyTunnel));
  end;
end;

procedure TSession.SetLocalPort(port: Longint);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_LOCALPORT, port);
  end;
end;

procedure TSession.SetLocalPortRange(range: Longint);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_LOCALPORTRANGE, range);
  end;
end;

procedure TSession.SetDNSCacheTimeout(timeout: TTimeInterval);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_DNS_CACHE_TIMEOUT,
      Longint(ceil(timeout.Seconds)));
  end;
end;

procedure TSession.SetDNSGlobalCache(enable: Boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_DNS_USE_GLOBAL_CACHE, Longint(enable));
  end;
end;

procedure TSession.SetDNSoverHTTPS(url: string);
begin
  if Opened then
  begin
    if url = '' then
    begin
      curl_easy_setopt(handle, CURLOPT_DOH_URL, 0);
    end else
    begin
      curl_easy_setopt(handle, CURLOPT_DOH_URL, PChar(url));
    end;
  end;
end;

procedure TSession.SetBufferSize(size: TDataSize);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_BUFFERSIZE, Longint(size.Bytes));
  end;
end;

procedure TSession.SetTCPFastOpen(fastOpen: Boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_TCP_FASTOPEN, Longint(fastOpen));
  end;
end;

procedure TSession.SetTCPNoDelay(noDelay: Boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_TCP_NODELAY, Longint(noDelay));
  end;
end;

procedure TSession.SetAddressScope(scope: Longint);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_ADDRESS_SCOPE, scope);
  end;
end;

procedure TSession.SetTCPKeepalive(probe: Boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_TCP_KEEPALIVE, Longint(probe));
  end;
end;

procedure TSession.SetTCPKeepIdle(time: TTimeInterval);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_TCP_KEEPIDLE, Longint(Ceil(time.Seconds)));
  end;
end;

procedure TSession.SetTCPKeepInterval(time: TTimeInterval);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_TCP_KEEPINTVL, Longint(Ceil(time.Seconds)));
  end;
end;

procedure TSession.SetUnixSocketPath(path: string);
begin
  if Opened then
  begin
    if path = '' then
    begin
      curl_easy_setopt(handle, CURLOPT_UNIX_SOCKET_PATH, 0);
    end else
    begin
      curl_easy_setopt(handle, CURLOPT_UNIX_SOCKET_PATH, PChar(path));
    end;
  end;
end;

procedure TSession.SetAbstractUnixSocketPath(path: string);
begin
  if Opened then
  begin
    if path = '' then
    begin
      curl_easy_setopt(handle, CURLOPT_ABSTRACT_UNIX_SOCKET, 0);
    end else
    begin
      curl_easy_setopt(handle, CURLOPT_ABSTRACT_UNIX_SOCKET, PChar(path));
    end;
  end;
end;

procedure TSession.SetUserPassword(userpwd: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_USERPWD, PChar(userpwd));
  end;
end;

procedure TSession.SetProxyUserPassword(userpwd: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PROXYUSERPWD, PChar(userpwd));
  end;
end;

procedure TSession.SetUsername(name: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_USERNAME, PChar(name));
  end;
end;

procedure TSession.SetPassword(pass: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PASSWORD, PChar(pass));
  end;
end;

procedure TSession.SetLoginOptions(options: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_LOGIN_OPTIONS, PChar(options));
  end;
end;

procedure TSession.SetProxyUsername(name: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PROXYUSERNAME, PChar(name));
  end;
end;

procedure TSession.SetProxyPassword(pass: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PROXYPASSWORD, PChar(pass));
  end;
end;

procedure TSession.SetHTTPAuth(AMethod: TAuthMethods);
var
  bitmask : Longint;
begin
  if Opened then
  begin
    bitmask := 0;
    if AUTH_BASIC in AMethod then
      bitmask := bitmask or CURLAUTH_BASIC;
    if AUTH_DIGEST in AMethod then
      bitmask := bitmask or CURLAUTH_DIGEST;
    if AUTH_NEGOTIATE in AMethod then
      bitmask := bitmask or CURLAUTH_NEGOTIATE;
    if AUTH_GSSAPI in AMethod then
      bitmask := bitmask or CURLAUTH_GSSAPI;
    if AUTH_NTLM in AMethod then
      bitmask := bitmask or CURLAUTH_NTLM;
    if AUTH_DIGEST_IE in AMethod then
      bitmask := bitmask or CURLAUTH_DIGEST_IE;
    if AUTH_NTLM_WB in AMethod then
      bitmask := bitmask or CURLAUTH_NTLM_WB;
    if AUTH_BEARER in AMethod then
      bitmask := bitmask or CURLAUTH_BEARER;
    if AUTH_ANY in AMethod then
      bitmask := CURLAUTH_BASIC or CURLAUTH_DIGEST or CURLAUTH_NEGOTIATE or
        CURLAUTH_NTLM or CURLAUTH_DIGEST_IE or CURLAUTH_NTLM_WB or
        CURLAUTH_BEARER;
    if AUTH_ANYSAFE in AMethod then
      bitmask := bitmask or CURLAUTH_DIGEST or CURLAUTH_NEGOTIATE or
        CURLAUTH_NTLM or CURLAUTH_NTLM_WB or CURLAUTH_BEARER;

    curl_easy_setopt(handle, CURLOPT_HTTPAUTH, bitmask);
  end;
end;

procedure TSession.SetTLSUsername(name: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_TLSAUTH_USERNAME, PChar(name));
  end;
end;

procedure TSession.SetProxyTLSUsername(name: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PROXY_TLSAUTH_USERNAME, PChar(name));
  end;
end;

procedure TSession.SetTLSPassword(pass: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_TLSAUTH_PASSWORD, PChar(pass));
  end;
end;

procedure TSession.SetProxyTLSPassword(pass: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PROXY_TLSAUTH_PASSWORD, PChar(pass));
  end;
end;

procedure TSession.SetTLSAuth(method: TTLSAuthMethod);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_TLSAUTH_TYPE,
      PChar(GetEnumName(TypeInfo(TTLSAuthMethod), ord(method))));
  end;
end;

procedure TSession.SetProxyTLSAuth(method: TTLSAuthMethod);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PROXY_TLSAUTH_TYPE,
      PChar(GetEnumName(TypeInfo(TTLSAuthMethod), ord(method))));
  end;
end;

procedure TSession.SetProxyHTTPAuth(method: Longint);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PROXYAUTH, method);
  end;
end;

procedure TSession.SetSOCKS5Auth(AMethod: TAuthMethods);
var
  bitmask : Longint;
begin
  if Opened then
  begin
    bitmask := 0;
    if AUTH_BASIC in AMethod then
      bitmask := bitmask or CURLAUTH_BASIC;
    if AUTH_GSSAPI in AMethod then
      bitmask := bitmask or CURLAUTH_GSSAPI;

    curl_easy_setopt(handle, CURLOPT_SOCKS5_AUTH, bitmask);
  end;
end;

procedure TSession.SetSASLAuthzid(authzid: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_SASL_AUTHZID, PChar(authzid));
  end;
end;

procedure TSession.SetSASLIR(send: Boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_SASL_IR, Longint(send));
  end;
end;

procedure TSession.SetXOAuth2Bearer(token: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_XOAUTH2_BEARER, PChar(token));
  end;
end;

procedure TSession.SetAllowUsernameInURL(allow: Boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_DISALLOW_USERNAME_IN_URL,
      Longint(Boolean(not allow)));
  end;
end;

procedure TSession.SetUnrestrictedAuth(send: Boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_UNRESTRICTED_AUTH, Longint(send));
  end;
end;

procedure TSession.SetMaxRedirects(amount: Longint);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_MAXREDIRS, amount);
  end;
end;

procedure TSession.SetPostRedirect(redir: Longint);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_POSTREDIR, redir);
  end;
end;

procedure TSession.SetPutMethod(put: Boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_PUT, Longint(put));
  end;
end;

procedure TSession.SetPostMethod(post: Boolean);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_POST, Longint(post));
  end;
end;

procedure TSession.SetPostFields(data: string);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_POSTFIELDS, PChar(data));
  end;
end;

procedure TSession.SetPostFieldsSize(size: Longint);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE, size);
  end;
end;

procedure TSession.SetPostFieldsSizeLarge(size: LongWord);
begin
  if Opened then
  begin
    curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE_LARGE, size);
  end;
end;

procedure TSession.SetAllowedProtocols(AProtocols: TProtocols);
var
  bitmask : Longint;
begin
  if Opened then
  begin
    bitmask := 0;
    if PROTOCOL_DICT in AProtocols then
      bitmask := bitmask or CURLPROTO_DICT;
    if PROTOCOL_FILE in AProtocols then
      bitmask := bitmask or CURLPROTO_FILE;
    if PROTOCOL_FTP in AProtocols then
      bitmask := bitmask or CURLPROTO_FTP;
    if PROTOCOL_FTPS in AProtocols then
      bitmask := bitmask or CURLPROTO_FTPS;
    if PROTOCOL_GOPHER in AProtocols then
      bitmask := bitmask or CURLPROTO_GOPHER;
    if PROTOCOL_HTTP in AProtocols then
      bitmask := bitmask or CURLPROTO_HTTP;
    if PROTOCOL_HTTPS in AProtocols then
      bitmask := bitmask or CURLPROTO_HTTPS;
    if PROTOCOL_IMAP in AProtocols then
      bitmask := bitmask or CURLPROTO_IMAP;
    if PROTOCOL_IMAPS in AProtocols then
      bitmask := bitmask or CURLPROTO_IMAPS;
    if PROTOCOL_LDAP in AProtocols then
      bitmask := bitmask or CURLPROTO_LDAP;
    if PROTOCOL_LDAPS in AProtocols then
      bitmask := bitmask or CURLPROTO_LDAPS;
    if PROTOCOL_POP3 in AProtocols then
      bitmask := bitmask or CURLPROTO_POP3;
    if PROTOCOL_POP3S in AProtocols then
      bitmask := bitmask or CURLPROTO_POP3S;
    if PROTOCOL_RTMP in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMP;
    if PROTOCOL_RTMPE in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMPE;
    if PROTOCOL_RTMPS in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMPS;
    if PROTOCOL_RTMPT in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMPT;
    if PROTOCOL_RTMPTE in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMPTE;
    if PROTOCOL_RTMPTS in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMPTS;
    if PROTOCOL_RTSP in AProtocols then
      bitmask := bitmask or CURLPROTO_RTSP;
    if PROTOCOL_SCP in AProtocols then
      bitmask := bitmask or CURLPROTO_SCP;
    if PROTOCOL_SFTP in AProtocols then
      bitmask := bitmask or CURLPROTO_SFTP;
    if PROTOCOL_SMB in AProtocols then
      bitmask := bitmask or CURLPROTO_SMB;
    if PROTOCOL_SMBS in AProtocols then
      bitmask := bitmask or CURLPROTO_SMBS;
    if PROTOCOL_SMTP in AProtocols then
      bitmask := bitmask or CURLPROTO_SMTP;
    if PROTOCOL_SMTPS in AProtocols then
      bitmask := bitmask or CURLPROTO_SMTPS;
    if PROTOCOL_TELNET in AProtocols then
      bitmask := bitmask or CURLPROTO_TELNET;
    if PROTOCOL_TFTP in AProtocols then
      bitmask := bitmask or CURLPROTO_TFTP;

    curl_easy_setopt(handle, CURLOPT_PROTOCOLS, bitmask);
  end;
end;

procedure TSession.SetAllowedRedirectProtocols(AProtocols: TProtocols);
var
  bitmask : Longint;
begin
  if Opened then
  begin
    bitmask := 0;
    if PROTOCOL_DICT in AProtocols then
      bitmask := bitmask or CURLPROTO_DICT;
    if PROTOCOL_FILE in AProtocols then
      bitmask := bitmask or CURLPROTO_FILE;
    if PROTOCOL_FTP in AProtocols then
      bitmask := bitmask or CURLPROTO_FTP;
    if PROTOCOL_FTPS in AProtocols then
      bitmask := bitmask or CURLPROTO_FTPS;
    if PROTOCOL_GOPHER in AProtocols then
      bitmask := bitmask or CURLPROTO_GOPHER;
    if PROTOCOL_HTTP in AProtocols then
      bitmask := bitmask or CURLPROTO_HTTP;
    if PROTOCOL_HTTPS in AProtocols then
      bitmask := bitmask or CURLPROTO_HTTPS;
    if PROTOCOL_IMAP in AProtocols then
      bitmask := bitmask or CURLPROTO_IMAP;
    if PROTOCOL_IMAPS in AProtocols then
      bitmask := bitmask or CURLPROTO_IMAPS;
    if PROTOCOL_LDAP in AProtocols then
      bitmask := bitmask or CURLPROTO_LDAP;
    if PROTOCOL_LDAPS in AProtocols then
      bitmask := bitmask or CURLPROTO_LDAPS;
    if PROTOCOL_POP3 in AProtocols then
      bitmask := bitmask or CURLPROTO_POP3;
    if PROTOCOL_POP3S in AProtocols then
      bitmask := bitmask or CURLPROTO_POP3S;
    if PROTOCOL_RTMP in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMP;
    if PROTOCOL_RTMPE in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMPE;
    if PROTOCOL_RTMPS in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMPS;
    if PROTOCOL_RTMPT in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMPT;
    if PROTOCOL_RTMPTE in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMPTE;
    if PROTOCOL_RTMPTS in AProtocols then
      bitmask := bitmask or CURLPROTO_RTMPTS;
    if PROTOCOL_RTSP in AProtocols then
      bitmask := bitmask or CURLPROTO_RTSP;
    if PROTOCOL_SCP in AProtocols then
      bitmask := bitmask or CURLPROTO_SCP;
    if PROTOCOL_SFTP in AProtocols then
      bitmask := bitmask or CURLPROTO_SFTP;
    if PROTOCOL_SMB in AProtocols then
      bitmask := bitmask or CURLPROTO_SMB;
    if PROTOCOL_SMBS in AProtocols then
      bitmask := bitmask or CURLPROTO_SMBS;
    if PROTOCOL_SMTP in AProtocols then
      bitmask := bitmask or CURLPROTO_SMTP;
    if PROTOCOL_SMTPS in AProtocols then
      bitmask := bitmask or CURLPROTO_SMTPS;
    if PROTOCOL_TELNET in AProtocols then
      bitmask := bitmask or CURLPROTO_TELNET;
    if PROTOCOL_TFTP in AProtocols then
      bitmask := bitmask or CURLPROTO_TFTP;

    curl_easy_setopt(handle, CURLOPT_REDIR_PROTOCOLS, bitmask);
  end;
end;

procedure TSession.SetDefaultProtocol(AProtocol: TProtocol);
var
  protocol : string;
begin
  if Opened then
  begin
    protocol := GetEnumName(TypeInfo(TProtocol), Ord(AProtocol));
    protocol := LowerCase(Copy(protocol, Length('PROTOCOL_'), Length(protocol) -
      Length('PROTOCOL_')));
    curl_easy_setopt(handle, CURLOPT_DEFAULT_PROTOCOL, PChar(protocol));
  end;
end;

initialization
  curl_global_init(CURL_GLOBAL_ALL);

finalization
  curl_global_cleanup;

end.


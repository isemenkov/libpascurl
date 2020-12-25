(******************************************************************************)
(*                                 libPasCURL                                 *)
(*            delphi and object pascal wrapper around cURL library            *)
(*                        https://github.com/curl/curl                        *)
(*                                                                            *)
(* Copyright (c) 2019 - 2020                                Ivan Semenkov     *)
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

unit libpascurl;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Types{$IFDEF LINUX}, Sockets, BaseUnix{$ELSE}, WinSock
  {$ENDIF};

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ENDIF}

const
  {$IFDEF FPC}
    {$IFDEF WINDOWS}
      CurlLib = 'libcurl.dll';
    {$ENDIF}
    {$IFDEF LINUX}
      CurlLib = 'libcurl.so';
    {$ENDIF}
  {$ELSE}
    {$IFDEF MSWINDOWS OR defined(MSWINDOWS)}
      CurlLib = 'libcurl.dll';
    {$ENDIF}
    {$IFDEF LINUX}
      CurlLib = 'libcurl.so';
    {$ENDIF}
  {$ENDIF}

  CURL_SOCKET_BAD                                                   = -1;

  { specified content is a file name }
  CURL_HTTPPOST_FILENAME                                            = 1 shl 0;
  { specified content is a file name }
  CURL_HTTPPOST_READFILE                                            = 1 shl 1;
  { name is only stored pointer do not free in formfree }
  CURL_HTTPPOST_PTRNAME                                             = 1 shl 2;
  { contents is only stored pointer do not free in formfree }
  CURL_HTTPPOST_PTRCONTENTS                                         = 1 shl 3;
  { upload file from buffer }
  CURL_HTTPPOST_BUFFER                                              = 1 shl 4;
  { upload file from pointer contents }
  CURL_HTTPPOST_PTRBUFFER                                           = 1 shl 5;
  { upload file contents by using the regular read callback to get the data and
    pass the given pointer as custom pointer }
  CURL_HTTPPOST_CALLBACK                                            = 1 shl 6;
  { use size in 'contentlen', added in 7.46.0 }
  CURL_HTTPPOST_LARGE                                               = 1 shl 7;

  { This is a return code for the progress callback that, when returned, will
    signal libcurl to continue executing the default progress function }
  CURL_PROGRESSFUNC_CONTINUE                                        = $10000001;

  { The maximum receive buffer size configurable via CURLOPT_BUFFERSIZE. }
  CURL_MAX_READ_SIZE                                                = 524288;

  { Tests have proven that 20K is a very bad buffer size for uploads on
    Windows, while 16K for some odd reason performed a lot better.
    We do the ifndef check to allow this value to easier be changed at build
    time for those who feel adventurous. The practical minimum is about
    400 bytes since libcurl uses a buffer of this size as a scratch area
    (unrelated to network send operations). }
  CURL_MAX_WRITE_SIZE                                               = 16384;

  { The only reason to have a max limit for this is to avoid the risk of a bad
    server feeding libcurl with a never-ending header that will cause reallocs
    infinitely }
  CURL_MAX_HTTP_HEADER                                           = (100 * 1024);

  { This is a magic return code for the write callback that, when returned,
    will signal libcurl to pause receiving on the current transfer. }
  CURL_WRITEFUNC_PAUSE                                              = $10000001;

  CURLFINFOFLAG_KNOWN_FILENAME                                      = 1 shl 0;
  CURLFINFOFLAG_KNOWN_FILETYPE                                      = 1 shl 1;
  CURLFINFOFLAG_KNOWN_TIME                                          = 1 shl 2;
  CURLFINFOFLAG_KNOWN_PERM                                          = 1 shl 3;
  CURLFINFOFLAG_KNOWN_UID                                           = 1 shl 4;
  CURLFINFOFLAG_KNOWN_GID                                           = 1 shl 5;
  CURLFINFOFLAG_KNOWN_SIZE                                          = 1 shl 6;
  CURLFINFOFLAG_KNOWN_HLINKCOUNT                                    = 1 shl 7;

  { return codes for CURLOPT_CHUNK_BGN_FUNCTION }
  CURL_CHUNK_BGN_FUNC_OK                                            = 0;
  CURL_CHUNK_BGN_FUNC_FAIL      { tell the lib to end the task }    = 1;
  CURL_CHUNK_BGN_FUNC_SKIP      { skip this chunk over }            = 2;

  { return codes for CURLOPT_CHUNK_END_FUNCTION }
  CURL_CHUNK_END_FUNC_OK                                            = 0;
  CURL_CHUNK_END_FUNC_FAIL      { tell the lib to end the task }    = 1;

  { return codes for FNMATCHFUNCTION }
  CURL_FNMATCHFUNC_MATCH      { string corresponds to the pattern } = 0;
  CURL_FNMATCHFUNC_NOMATCH    { pattern doesn't match the string }  = 1;
  CURL_FNMATCHFUNC_FAIL       { an error occurred }                 = 2;

  { These are the return codes for the seek callbacks }
  CURL_SEEKFUNC_OK                                                  = 0;
  CURL_SEEKFUNC_FAIL            { fail the entire transfer }        = 1;
  CURL_SEEKFUNC_CANTSEEK        { tell libcurl seeking can't }      = 2;
                                { be done, so libcurl might try other means }
                                { instead }

  { This is a return code for the read callback that, when returned, will
    signal libcurl to immediately abort the current transfer. }
  CURL_READFUNC_ABORT                                               = $10000000;
  { This is a return code for the read callback that, when returned, will
    signal libcurl to pause sending data on the current transfer. }
  CURL_READFUNC_PAUSE                                               = $10000001;

  { Return code for when the trailing headers' callback has terminated
    without any errors }
  CURL_TRAILERFUNC_OK                                               = 0;
  { Return code for when was an error in the trailing header's list and we
    want to abort the request }
  CURL_TRAILERFUNC_ABORT                                            = 1;

  { The return code from the sockopt_callback can signal information back
    to libcurl: }
  CURL_SOCKOPT_OK                                                   = 0;
  CURL_SOCKOPT_ERROR            { causes libcurl to abort and  }    = 1;
                                { return CURLE_ABORTED_BY_CALLBACK }
  CURL_SOCKOPT_ALREADY_CONNECTED                                    = 2;

  { This was the error code 50 in 7.7.3 and a few earlier versions, this
    is no longer used by libcurl but is instead #defined here only to not
    make programs break }
  CURLE_ALREADY_COMPLETE                                            = 99999;

  { Bitmasks for CURLOPT_HTTPAUTH and CURLOPT_PROXYAUTH options:

    CURLAUTH_NONE         - No HTTP authentication
    CURLAUTH_BASIC        - HTTP Basic authentication (default)
    CURLAUTH_DIGEST       - HTTP Digest authentication
    CURLAUTH_NEGOTIATE    - HTTP Negotiate (SPNEGO) authentication
    CURLAUTH_GSSNEGOTIATE - Alias for CURLAUTH_NEGOTIATE (deprecated)
    CURLAUTH_NTLM         - HTTP NTLM authentication
    CURLAUTH_DIGEST_IE    - HTTP Digest authentication with IE flavour
    CURLAUTH_NTLM_WB      - HTTP NTLM authentication delegated to winbind helper
    CURLAUTH_BEARER       - HTTP Bearer token authentication
    CURLAUTH_ONLY         - Use together with a single other type to force no
                            authentication or just that single type
    CURLAUTH_ANY          - All fine types set
    CURLAUTH_ANYSAFE      - All fine types except Basic }
  CURLAUTH_NONE                                            = Cardinal(0);
  CURLAUTH_BASIC                                           = Cardinal(1 shl 0);
  CURLAUTH_DIGEST                                          = Cardinal(1 shl 1);
  CURLAUTH_NEGOTIATE                                       = Cardinal(1 shl 2);
  CURLAUTH_GSSAPI                                          = CURLAUTH_NEGOTIATE;
  CURLAUTH_NTLM                                            = Cardinal(1 shl 3);
  CURLAUTH_DIGEST_IE                                       = Cardinal(1 shl 4);
  CURLAUTH_NTLM_WB                                         = Cardinal(1 shl 5);
  CURLAUTH_BEARER                                          = Cardinal(1 shl 6);
  CURLAUTH_ONLY                                            = Cardinal(1 shl 31);
  CURLAUTH_ANY                   = (Not CURLAUTH_DIGEST_IE);
  CURLAUTH_ANYSAFE               = (Not (CURLAUTH_BASIC or CURLAUTH_DIGEST_IE));

  CURLSSH_AUTH_ANY            { all types supported by the server } = (Not 0);
  CURLSSH_AUTH_NONE           { none allowed, silly but complete }  = 0;
  CURLSSH_AUTH_PUBLICKEY        { public/private key files }        = 1 shl 0;
  CURLSSH_AUTH_PASSWORD         { password }                        = 1 shl 1;
  CURLSSH_AUTH_HOST             { host key files }                  = 1 shl 2;
  CURLSSH_AUTH_KEYBOARD         { keyboard interactive }            = 1 shl 3;
  CURLSSH_AUTH_AGENT            { agent (ssh-agent, pageant...) }   = 1 shl 4;
  CURLSSH_AUTH_GSSAPI           { gssapi (kerberos, ...) }          = 1 shl 5;
  CURLSSH_AUTH_DEFAULT                                      = CURLSSH_AUTH_ANY;

  CURLGSSAPI_DELEGATION_NONE    { no delegation (default) }         = 0;
  CURLGSSAPI_DELEGATION_POLICY_FLAG { if permitted by policy }      = 1 shl 0;
  CURLGSSAPI_DELEGATION_FLAG    { delegate always }                 = 1 shl 1;

  CURL_ERROR_SIZE                                                   = 256;

  { Definition of bits for the CURLOPT_SSL_OPTIONS argument: }

  { - ALLOW_BEAST tells libcurl to allow the BEAST SSL vulnerability in the
    name of improving interoperability with older servers. Some SSL libraries
    have introduced work-arounds for this flaw but those work-arounds sometimes
    make the SSL communication fail. To regain functionality with those broken
    servers, a user can this way allow the vulnerability back. }
  CURLSSLOPT_ALLOW_BEAST                                            = 1 shl 0;

  { - NO_REVOKE tells libcurl to disable certificate revocation checks for
    those SSL backends where such behavior is present. }
  CURLSSLOPT_NO_REVOKE                                              = 1 shl 1;

  { - NO_PARTIALCHAIN tells libcurl to *NOT* accept a partial certificate chain
    if possible. The OpenSSL backend has this ability. }
  CURLSSLOPT_NO_PARTIALCHAIN                                        = 1 shl 2;

  { - REVOKE_BEST_EFFORT tells libcurl to ignore certificate revocation offline
   checks and ignore missing revocation list for those SSL backends where such
   behavior is present. }
   CURLSSLOPT_REVOKE_BEST_EFFORT                                    = 1 shl 3;

  { - CURLSSLOPT_NATIVE_CA tells libcurl to use standard certificate store of
   operating system. Currently implemented under MS-Windows. }
  CURLSSLOPT_NATIVE_CA                                              = 1 shl 4;

  { The default connection attempt delay in milliseconds for happy eyeballs.
    CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS.3 and happy-eyeballs-timeout-ms.d document
    this value, keep them in sync. }
  CURL_HET_DEFAULT                                               = Longint(200);

  { The default connection upkeep interval in milliseconds. }
  CURL_UPKEEP_INTERVAL_DEFAULT                                 = Longint(60000);

  { bitmask defines for CURLOPT_HEADEROPT }
  CURLHEADER_UNIFIED                                                = 0;
  CURLHEADER_SEPARATE                                               = 1 shl 0;

  { CURLALTSVC_* are bits for the CURLOPT_ALTSVC_CTRL option }
  CURLALTSVC_READONLYFILE                                           = 1 shl 2;
  CURLALTSVC_H1                                                     = 1 shl 3;
  CURLALTSVC_H2                                                     = 1 shl 4;
  CURLALTSVC_H3                                                     = 1 shl 5;

  { CURLPROTO_ defines are for the CURLOPT_*PROTOCOLS options }
  CURLPROTO_HTTP                                                    = 1 shl 0;
  CURLPROTO_HTTPS                                                   = 1 shl 1;
  CURLPROTO_FTP                                                     = 1 shl 2;
  CURLPROTO_FTPS                                                    = 1 shl 3;
  CURLPROTO_SCP                                                     = 1 shl 4;
  CURLPROTO_SFTP                                                    = 1 shl 5;
  CURLPROTO_TELNET                                                  = 1 shl 6;
  CURLPROTO_LDAP                                                    = 1 shl 7;
  CURLPROTO_LDAPS                                                   = 1 shl 8;
  CURLPROTO_DICT                                                    = 1 shl 9;
  CURLPROTO_FILE                                                    = 1 shl 10;
  CURLPROTO_TFTP                                                    = 1 shl 11;
  CURLPROTO_IMAP                                                    = 1 shl 12;
  CURLPROTO_IMAPS                                                   = 1 shl 13;
  CURLPROTO_POP3                                                    = 1 shl 14;
  CURLPROTO_POP3S                                                   = 1 shl 15;
  CURLPROTO_SMTP                                                    = 1 shl 16;
  CURLPROTO_SMTPS                                                   = 1 shl 17;
  CURLPROTO_RTSP                                                    = 1 shl 18;
  CURLPROTO_RTMP                                                    = 1 shl 19;
  CURLPROTO_RTMPT                                                   = 1 shl 20;
  CURLPROTO_RTMPE                                                   = 1 shl 21;
  CURLPROTO_RTMPTE                                                  = 1 shl 22;
  CURLPROTO_RTMPS                                                   = 1 shl 23;
  CURLPROTO_RTMPTS                                                  = 1 shl 24;
  CURLPROTO_GOPHER                                                  = 1 shl 25;
  CURLPROTO_SMB                                                     = 1 shl 26;
  CURLPROTO_SMBS                                                    = 1 shl 27;
  CURLPROTO_MQTT                                                    = 1 shl 28;
  CURLPROTO_ALL                 { enable everything }               = (Not 0);

  { long may be 32 or 64 bits, but we should never depend on anything else
    but 32 }
  CURLOPTTYPE_LONG                                                  = 0;
  { 'long' argument with a set of values/bitmask }                                                
  CURLOPTTYPE_VALUES = CURLOPTTYPE_LONG;
  CURLOPTTYPE_OBJECTPOINT                                           = 10000;
  { 'char *' argument to a string with a trailing zero }                                      
  CURLOPTTYPE_STRINGPOINT = CURLOPTTYPE_OBJECTPOINT;
  { 'struct curl_slist *' argument }
  CURLOPTTYPE_SLISTPOINT = CURLOPTTYPE_OBJECTPOINT;
  { 'void *' argument passed untouched to callback }
  CURLOPTTYPE_CBPOINT = CURLOPTTYPE_OBJECTPOINT;
  CURLOPTTYPE_FUNCTIONPOINT                                         = 20000;
  CURLOPTTYPE_OFF_T                                                 = 30000;
  CURLOPTTYPE_BLOB                                                  = 40000;

  { Below here follows defines for the CURLOPT_IPRESOLVE option. If a host
    name resolves addresses using more than one IP protocol version, this
    option might be handy to force libcurl to use a specific IP version. }
  CURL_IPRESOLVE_WHATEVER { default, resolves addresses to all }    = 0;
                          { IP versions that your system allows }
  CURL_IPRESOLVE_V4       { resolve to IPv4 addresses }             = 1;
  CURL_IPRESOLVE_V6       { resolve to IPv6 addresses }             = 2;

  { symbols to use with CURLOPT_POSTREDIR.
    CURL_REDIR_POST_301, CURL_REDIR_POST_302 and CURL_REDIR_POST_303
    can be bitwise ORed so that CURL_REDIR_POST_301 or CURL_REDIR_POST_302
    or CURL_REDIR_POST_303 == CURL_REDIR_POST_ALL }
  CURL_REDIR_GET_ALL                                                = 0;
  CURL_REDIR_POST_301                                               = 1;
  CURL_REDIR_POST_302                                               = 2;
  CURL_REDIR_POST_303                                               = 4;
  CURL_REDIR_POST_ALL = CURL_REDIR_POST_301 or CURL_REDIR_POST_302 or
    CURL_REDIR_POST_303;

  CURL_ZERO_TERMINATED                                           = Cardinal(-1);

  CURLINFO_STRING                                                   = $100000;
  CURLINFO_LONG                                                     = $200000;
  CURLINFO_DOUBLE                                                   = $300000;
  CURLINFO_SLIST                                                    = $400000;
  CURLINFO_PTR            { same as SLIST }                         = $400000;
  CURLINFO_SOCKET                                                   = $500000;
  CURLINFO_OFF_T                                                    = $600000;
  CURLINFO_MASK                                                     = $0fffff;
  CURLINFO_TYPEMASK                                                 = $f00000;

  CURL_GLOBAL_SSL         { no purpose since since 7.57.0 }         = 1 shl 0;
  CURL_GLOBAL_WIN32                                                 = 1 shl 1;
  CURL_GLOBAL_ALL                        = CURL_GLOBAL_SSL or CURL_GLOBAL_WIN32;
  CURL_GLOBAL_NOTHING                                               = 0;
  CURL_GLOBAL_DEFAULT                                         = CURL_GLOBAL_ALL;
  CURL_GLOBAL_ACK_EINTR                                             = 1 shl 2;

  CURL_VERSION_IPV6       { IPv6-enabled }                          = 1 shl 0;
  CURL_VERSION_KERBEROS4  { Kerberos V4 auth is supported }         = 1 shl 1;
                          { (deprecated) }
  CURL_VERSION_SSL        { SSL options are present }               = 1 shl 2;
  CURL_VERSION_LIBZ       { libz features are present }             = 1 shl 3;
  CURL_VERSION_NTLM       { NTLM auth is supported }                = 1 shl 4;
  CURL_VERSION_GSSNEGOTIATE { Negotiate auth is supported }         = 1 shl 5;
                          { (deprecated) }
  CURL_VERSION_DEBUG      { Built with debug capabilities }         = 1 shl 6;
  CURL_VERSION_ASYNCHDNS  { Asynchronous DNS resolves }             = 1 shl 7;
  CURL_VERSION_SPNEGO     { SPNEGO auth is supported }              = 1 shl 8;
  CURL_VERSION_LARGEFILE  { Supports files larger than 2GB }        = 1 shl 9;
  CURL_VERSION_IDN        { Internationized Domain Names are }      = 1 shl 10;
                          { supported }
  CURL_VERSION_SSPI       { Built against Windows SSPI }            = 1 shl 11;
  CURL_VERSION_CONV       { Character conversions supported }       = 1 shl 12;
  CURL_VERSION_CURLDEBUG  { Debug memory tracking supported }       = 1 shl 13;
  CURL_VERSION_TLSAUTH_SRP{ TLS-SRP auth is supported }             = 1 shl 14;
  CURL_VERSION_NTLM_WB    { NTLM delegation to winbind helper }     = 1 shl 15;
                          { is supported }
  CURL_VERSION_HTTP2      { HTTP2 support built-in }                = 1 shl 16;
  CURL_VERSION_GSSAPI     { Built against a GSS-API library }       = 1 shl 17;
  CURL_VERSION_KERBEROS5  { Kerberos V5 auth is supported }         = 1 shl 18;
  CURL_VERSION_UNIX_SOCKETS { Unix domain sockets support }         = 1 shl 19;
  CURL_VERSION_PSL        { Mozilla's Public Suffix List, }         = 1 shl 20;
                          { used for cookie domain verification }
  CURL_VERSION_HTTPS_PROXY{ HTTPS-proxy support built-in }          = 1 shl 21;
  CURL_VERSION_MULTI_SSL  { Multiple SSL backends available }       = 1 shl 22;
  CURL_VERSION_BROTLI     { Brotli features are present. }          = 1 shl 23;
  CURL_VERSION_ALTSVC     { Alt-Svc handling built-in }             = 1 shl 24;
  CURL_VERSION_HTTP3      { HTTP3 support built-in }                = 1 shl 25;
  CURL_VERSION_ZSTD       { zstd features are present }             = 1 shl 26;
  CURL_VERSION_UNICODE    { Unicode support on Windows }            = 1 shl 27;
  CURL_VERSION_HSTS       { HSTS is supported }                     = 1 shl 28;

  CURLPAUSE_RECV                                                    = 1 shl 0;
  CURLPAUSE_RECV_CONT                                               = 0;
  CURLPAUSE_SEND                                                    = 1 shl 2;
  CURLPAUSE_SEND_CONT                                               = 0;

  CURLPAUSE_ALL          = CURLPAUSE_RECV or CURLPAUSE_SEND;
  CURLPAUSE_CONT         = CURLPAUSE_RECV_CONT or CURLPAUSE_SEND_CONT;

  { bitmask bits for CURLMOPT_PIPELINING }
  CURLPIPE_NOTHING                                                 = Longint(0);
  CURLPIPE_HTTP1                                                   = Longint(1);
  CURLPIPE_MULTIPLEX                                               = Longint(2);

  { Based on poll(2) structure and values.
    We don't use pollfd and POLL* constants explicitly
    to cover platforms without poll(). }
  CURL_WAIT_POLLIN                                                  = $0001;
  CURL_WAIT_POLLPRI                                                 = $0002;
  CURL_WAIT_POLLOUT                                                 = $0004;

  CURL_POLL_NONE                                                    = 0;
  CURL_POLL_IN                                                      = 1;
  CURL_POLL_OUT                                                     = 2;
  CURL_POLL_INOUT                                                   = 3;
  CURL_POLL_REMOVE                                                  = 4;

  CURL_SOCKET_TIMEOUT                                         = CURL_SOCKET_BAD;

  CURL_CSELECT_IN                                                   = $01;
  CURL_CSELECT_OUT                                                  = $02;
  CURL_CSELECT_ERR                                                  = $04;

  CURL_PUSH_OK                                                      = 0;
  CURL_PUSH_DENY                                                    = 1;
  CURL_PUSH_ERROROUT      { added in 7.72.0 }                       = 2;

  CURLU_DEFAULT_PORT      { return default port number }            = 1 shl 0;
  CURLU_NO_DEFAULT_PORT   { act as if no port number was set, }     = 1 shl 1;
                          { if the port number matches the default }
                          { for the scheme }
  CURLU_DEFAULT_SCHEME    { return default scheme if missing }      = 1 shl 2;
  CURLU_NON_SUPPORT_SCHEME{ allow non-supported scheme }            = 1 shl 3;
  CURLU_PATH_AS_IS        { leave dot sequences }                   = 1 shl 4;
  CURLU_DISALLOW_USER     { no user+password allowed }              = 1 shl 5;
  CURLU_URLDECODE         { URL decode on get }                     = 1 shl 6;
  CURLU_URLENCODE         { URL encode on set }                     = 1 shl 7;
  CURLU_APPENDQUERY       { append a form style part }              = 1 shl 8;
  CURLU_GUESS_SCHEME      { legacy curl-style guessing }            = 1 shl 9;
  CURLU_NO_AUTHORITY      { Allow empty authority when the scheme } = 1 shl 10;
                          { is unknown. }

  { CURLHSTS_* are bits for the CURLOPT_HSTS option }
  CURLHSTS_ENABLE                                                   = 1 shl 0;
  CURLHSTS_READONLYFILE                                             = 1 shl 1;

type
  CURL = type Pointer;
  CURLSH = type Pointer;
  CURLM = type Pointer;
  curl_socket_t = type Integer;
  curl_off_t = type Longint;

  curl_hstsentry = record
    name : PChar;
    namelen : Cardinal;
    includeSubDomains : Byte;         { unsigned int includeSubDomains:1; }
    expire : array [0 .. 17] of Char; { YYYYMMDD HH:MM:SS [null-terminated] }
  end;

  curl_index = record
    index : Cardinal;                 { the provided entry's "index" or count }
    total : Cardinal;                 { total number of entries to save }  
  end;

  CURLSTScode = (
    CURLSTS_OK,
    CURLSTS_DONE,
    CURLSTS_FAIL
  );

  curl_hstsread_callback = function (easy : CURL; e : curl_hstsentry; userp :
    Pointer) : CURLSTScode of object;
  curl_hstswrite_callback = function (easy : CURL; e : curl_hstsentry; i :
    curl_index; userp : Pointer) : CURLSTScode of object;

  { enum for the different supported SSL backends }
  curl_sslbackend = (
    CURLSSLBACKEND_NONE                                             = 0,
    CURLSSLBACKEND_OPENSSL                                          = 1,
    { aliases for library clones and renames }
    CURLSSLBACKEND_LIBRESSL = CURLSSLBACKEND_OPENSSL{%H-},
    CURLSSLBACKEND_BORINGSSL = CURLSSLBACKEND_OPENSSL{%H-},
    { end aliases }
    CURLSSLBACKEND_GNUTLS                                           = 2,
    CURLSSLBACKEND_NSS                                              = 3,
    CURLSSLBACKEND_OBSOLETE4 { Was QSOSSL. }                        = 4,
    CURLSSLBACKEND_GSKIT                                            = 5,
    CURLSSLBACKEND_POLARSSL                                         = 6,
    CURLSSLBACKEND_WOLFSSL                                          = 7,
    { deprecated names: }
    CURLSSLBACKEND_CYASSL = CURLSSLBACKEND_WOLFSSL{%H-},
    { end deprecated }
    CURLSSLBACKEND_SCHANNEL                                         = 8,
    CURLSSLBACKEND_SECURETRANSPORT                                  = 9,
    { deprecated names: }
    CURLSSLBACKEND_DARWINSSL = CURLSSLBACKEND_SECURETRANSPORT{%H-},
    { end deprecated }
    CURLSSLBACKEND_AXTLS { never used since 7.63.0 }                = 10,
    CURLSSLBACKEND_MBEDTLS                                          = 11,
    CURLSSLBACKEND_MESALINK                                         = 12,
    CURLSSLBACKEND_BEARSSL                                          = 13
  );

  { linked-list structure for the CURLOPT_QUOTE option (and other) }
  pcurl_slist = ^curl_slist;
  ppcurl_slist = ^pcurl_slist;
  curl_slist = record
    data : PChar;
    next : pcurl_slist;
  end;

  pcurl_httppost = ^curl_httppost;
  ppcurl_httppost = ^pcurl_httppost;
  curl_httppost = record
    next : ^curl_httppost;      { next entry in the list }
    name : PChar;               { pointer to allocated name }
    namelength : Longint;       { length of name length }
    contents : PChar;           { pointer to allocated data contents }
    contentslength : Longint;   { length of contents field, see also
                                   CURL_HTTPPOST_LARGE }
    buffer : PChar;             { pointer to allocated buffer contents }
    bufferLength : Longint;     { length of buffer field }
    contenttype : PChar;        { Content-Type }
    contentheader : ^curl_slist;{ list of extra headers for this form }
    more : ^curl_httppost;      { if one field name has more than one
                                   file, this link should link to following
                                   files }
    flags : Longint;            { as defined below }
    showfilename : PChar;       { The file name to show. If not set, the
                                   actual file name will be used (if this
                                   is a file part) }
    userp : Pointer;            { custom pointer used for
                                   HTTPPOST_CALLBACK posts }
    contentlen : curl_off_t;    { alternative length of contents
                                   field. Used if CURL_HTTPPOST_LARGE is
                                   set. Added in 7.46.0 }
  end;

  { This is the CURLOPT_PROGRESSFUNCTION callback proto. It is now considered
    deprecated but was the only choice up until 7.31.0 }
  curl_progress_callback = function (clientp : Pointer; dltotal : Double;
    dlnow : Double; ultotal : Double; ulnow : Double) : Integer of object;

  { This is the CURLOPT_XFERINFOFUNCTION callback proto. It was introduced in
    7.32.0, it avoids floating point and provides more detailed information. }
  curl_xferinfo_callback = function (clientp : Pointer; dltotal : curl_off_t;
    dlnow : curl_off_t; ultotal : curl_off_t; ulnow : curl_off_t) : Integer
    of object;

  curl_write_callback = function (buffer : PChar; size : LongWord; nitems :
    LongWord; outstream : Pointer) : LongWord of object;

  { This callback will be called when a new resolver request is made }
  curl_resolver_start_callback = function (resolver_state : Pointer; reserved :
    Pointer; userdata : Pointer) : Integer of object;

  { enumeration of file types }
  curlfiletype = (
    CURLFILETYPE_FILE                                               = 0,
    CURLFILETYPE_DIRECTORY,
    CURLFILETYPE_SYMLINK,
    CURLFILETYPE_DEVICE_BLOCK,
    CURLFILETYPE_DEVICE_CHAR,
    CURLFILETYPE_NAMEDPIPE,
    CURLFILETYPE_SOCKET,
    CURLFILETYPE_DOOR,          { is possible only on Sun Solaris now }
    CURLFILETYPE_UNKNOWN        { should never occur }
  );

  { Information about a single file, used when doing FTP wildcard matching }
  curl_fileinfo = record
    filename : PChar;
    filetype : curlfiletype;
    time : Int64;               { always zero! }
    perm : Cardinal;
    uid : Integer;
    gid : Integer;
    size : curl_off_t;
    hardlinks : Longint;
    strings : record
      { If some of these fields is not NULL, it is a pointer to b_data. }
      time : PChar;
      perm : PChar;
      user : PChar;
      group : PChar;
      target : PChar;           { pointer to the target filename of a symlink }
    end;
    flags : Cardinal;
    { used internally }
    b_data : PChar;
    b_size : LongWord;
    b_used : LongWord;
  end;

  { if splitting of data transfer is enabled, this callback is called before
    download of an individual chunk started. Note that parameter "remains" works
    only for FTP wildcard downloading (for now), otherwise is not used }
  curl_chunk_bgn_callback = function (const transfer_info : Pointer;
    ptr : Pointer; remains : Integer) : Longint of object;

  { If splitting of data transfer is enabled this callback is called after
    download of an individual chunk finished.
    Note! After this callback was set then it have to be called FOR ALL chunks.
    Even if downloading of this chunk was skipped in CHUNK_BGN_FUNC.
    This is the reason why we don't need "transfer_info" parameter in this
    callback and we are not interested in "remains" parameter too. }
  curl_chunk_end_callback = function (ptr : Pointer) : Longint of object;

  { callback type for wildcard downloading pattern matching. If the
    string matches the pattern, return CURL_FNMATCHFUNC_MATCH value, etc. }
  curl_fnmatch_callback = function (ptr : Pointer; const pattern : PChar;
    const str : PChar) : Integer of object;

  curl_seek_callback = function (instream : Pointer; offset : curl_off_t;
    origin : Integer) : Integer of object;

  curl_read_callback = function (buffer : PChar; size : LongWord;
    nitems : LongWord; instream : Pointer) : LongWord of object;

  curl_trailer_callback = function (list : ppcurl_slist; userdata : Pointer) :
    Integer of object;

  curlsocktype = (
    CURLSOCKTYPE_IPCXN,         { socket created for a specific IP connection }
    CURLSOCKTYPE_ACCEPT,        { socket created by accept() call }
    CURLSOCKTYPE_LAST           { never use }
  );

  curl_sockopt_callback = function (clientp : Pointer; curlfd : curl_socket_t;
    purpose : curlsocktype) : Integer of object;

  pcurl_sockaddr = ^curl_sockaddr;
  curl_sockaddr = record
    family : Integer;
    socktype : Integer;
    protocol : Integer;
    addrlen : Cardinal;         { addrlen was a socklen_t type before 7.18.0 }
                                { but it turned really ugly and painful on the }
                                { systems that lack this type }
    addr : {$IFDEF FPC}sockaddr
      {$ELSE IFDEF DCC32 OR IFDEF DCC64}Pointer{$ENDIF};
  end;

  curl_opensocket_callback = function (clientp : Pointer;
    purpose : curlsocktype; address : pcurl_sockaddr) : curl_socket_t of object;

  curl_closesocket_callback = function (clientp : Pointer;
    item : curl_socket_t) : Integer of object;

  curlioerr = (
   CURLEIOE_OK,                 { I/O operation successful }
   CURLIOE_UNKNOWNCMD,          { command was unknown to callback }
   CURLIOE_FAILRESTART,         { failed to restart the read }
   CURLIOE_LAST                 { never use }
  );

  curliocmd = (
   CURLIOCMD_NOP,               { no operation }
   CURLIOCMD_RESTARTREAD,       { restart the read stream from start }
   CURLIOCMD_LAST               { never use }
  );

  curl_ioctrl_callback = function (handle : CURL; cmd : Integer;
    clientp : Pointer) : curlioerr of object;

  { The following typedef's are signatures of malloc, free, realloc, strdup and
    calloc respectively.  Function pointers of these types can be passed to the
    curl_global_init_mem() function to set user defined memory management
    callback routines. }

  curl_malloc_callback = function (size : Cardinal) : Pointer of object;
  curl_free_callback = procedure (ptr : Pointer) of object;
  curl_realloc_callback = function (ptr : Pointer; size : Cardinal) : Pointer of
    object;
  curl_strdup_callback = function (const str : PChar) : PChar of object;
  curl_calloc_callback = function (nmemb : Cardinal; size : Cardinal) : Pointer
    of object;

  { the kind of data that is passed to information_callback }
  curl_infotype = (
    CURLINFO_TEXT                                                   = 0,
    CURLINFO_HEADER_IN,         { 1 }
    CURLINFO_HEADER_OUT,        { 2 }
    CURLINFO_DATA_IN,           { 3 }
    CURLINFO_DATA_OUT,          { 4 }
    CURLINFO_SSL_DATA_IN,       { 5 }
    CURLINFO_SSL_DATA_OUT,      { 6 }
    CURLINFO_END
  );

  curl_debug_callback = function (
    handle : CURL;              { the handle/transfer this concerns }
    type_ : curl_infotype;      { what kind of data }
    data : PChar;               { points to the data }
    size: Cardinal;             { size of the data pointed to }
    userptr : Pointer)          { whatever the user please }
  : Integer of object;

  (* All possible error codes from all sorts of curl functions. Future versions
   may return other values, stay prepared.

   Always add new return codes last. Never *EVER* remove any. The return
   codes must remain the same! *)

  CURLcode = (
    CURLE_OK                                                        = 0,
    CURLE_UNSUPPORTED_PROTOCOL, { 1 }
    CURLE_FAILED_INIT,          { 2 }
    CURLE_URL_MALFORMAT,        { 3 }
    CURLE_NOT_BUILT_IN,         { 4 - [was obsoleted in August 2007 for
                                    7.17.0, reused in April 2011 for 7.21.5] }
    CURLE_COULDNT_RESOLVE_PROXY,{ 5 }
    CURLE_COULDNT_RESOLVE_HOST, { 6 }
    CURLE_COULDNT_CONNECT,      { 7 }
    CURLE_WEIRD_SERVER_REPLY,   { 8 }
    CURLE_REMOTE_ACCESS_DENIED, { 9 a service was denied by the server
                                    due to lack of access - when login fails
                                    this is not returned. }
    CURLE_FTP_ACCEPT_FAILED,    { 10 - [was obsoleted in April 2006 for
                                     7.15.4, reused in Dec 2011 for 7.24.0] }
    CURLE_FTP_WEIRD_PASS_REPLY, { 11 }
    CURLE_FTP_ACCEPT_TIMEOUT,   { 12 - timeout occurred accepting server
                                     [was obsoleted in August 2007 for 7.17.0,
                                     reused in Dec 2011 for 7.24.0] }
    CURLE_FTP_WEIRD_PASV_REPLY, { 13 }
    CURLE_FTP_WEIRD_227_FORMAT, { 14 }
    CURLE_FTP_CANT_GET_HOST,    { 15 }
    CURLE_HTTP2,                { 16 - A problem in the http2 framing layer.
                                     [was obsoleted in August 2007 for 7.17.0,
                                     reused in July 2014 for 7.38.0] }
    CURLE_FTP_COULDNT_SET_TYPE, { 17 }
    CURLE_PARTIAL_FILE,         { 18 }
    CURLE_FTP_COULDNT_RETR_FILE,{ 19 }
    CURLE_OBSOLETE20,           { 20 - NOT USED }
    CURLE_QUOTE_ERROR,          { 21 - quote command failure }
    CURLE_HTTP_RETURNED_ERROR,  { 22 }
    CURLE_WRITE_ERROR,          { 23 }
    CURLE_OBSOLETE24,           { 24 - NOT USED }
    CURLE_UPLOAD_FAILED,        { 25 - failed upload "command" }
    CURLE_READ_ERROR,           { 26 - couldn't open/read from file }
    CURLE_OUT_OF_MEMORY,        { 27 }
    { Note: CURLE_OUT_OF_MEMORY may sometimes indicate a conversion error
            instead of a memory allocation error if CURL_DOES_CONVERSIONS
            is defined }
    CURLE_OPERATION_TIMEDOUT,   { 28 - the timeout time was reached }
    CURLE_OBSOLETE29,           { 29 - NOT USED }
    CURLE_FTP_PORT_FAILED,      { 30 - FTP PORT operation failed }
    CURLE_FTP_COULDNT_USE_REST, { 31 - the REST command failed }
    CURLE_OBSOLETE32,           { 32 - NOT USED }
    CURLE_RANGE_ERROR,          { 33 - RANGE "command" didn't work }
    CURLE_HTTP_POST_ERROR,      { 34 }
    CURLE_SSL_CONNECT_ERROR,    { 35 - wrong when connecting with SSL }
    CURLE_BAD_DOWNLOAD_RESUME,  { 36 - couldn't resume download }
    CURLE_FILE_COULDNT_READ_FILE,{ 37 }
    CURLE_LDAP_CANNOT_BIND,     { 38 }
    CURLE_LDAP_SEARCH_FAILED,   { 39 }
    CURLE_OBSOLETE40,           { 40 - NOT USED }
    CURLE_FUNCTION_NOT_FOUND,   { 41 - NOT USED starting with 7.53.0 }
    CURLE_ABORTED_BY_CALLBACK,  { 42 }
    CURLE_BAD_FUNCTION_ARGUMENT,{ 43 }
    CURLE_OBSOLETE44,           { 44 - NOT USED }
    CURLE_INTERFACE_FAILED,     { 45 - CURLOPT_INTERFACE failed }
    CURLE_OBSOLETE46,           { 46 - NOT USED }
    CURLE_TOO_MANY_REDIRECTS,   { 47 - catch endless re-direct loops }
    CURLE_UNKNOWN_OPTION,       { 48 - User specified an unknown option }
    CURLE_TELNET_OPTION_SYNTAX, { 49 - Malformed telnet option }
    CURLE_OBSOLETE50,           { 50 - NOT USED }
    CURLE_OBSOLETE51,           { 51 - NOT USED }
    CURLE_GOT_NOTHING,          { 52 - when this is a specific error }
    CURLE_SSL_ENGINE_NOTFOUND,  { 53 - SSL crypto engine not found }
    CURLE_SSL_ENGINE_SETFAILED, { 54 - can not set SSL crypto engine as
                                     default }
    CURLE_SEND_ERROR,           { 55 - failed sending network data }
    CURLE_RECV_ERROR,           { 56 - failure in receiving network data }
    CURLE_OBSOLETE57,           { 57 - NOT IN USE }
    CURLE_SSL_CERTPROBLEM,      { 58 - problem with the local certificate }
    CURLE_SSL_CIPHER,           { 59 - couldn't use specified cipher }
    CURLE_SSL_CACERT,           { 60 - problem with the CA cert (path?) }
    CURLE_PEER_FAILED_VERIFICATION = CURLE_SSL_CACERT{%H-},
    CURLE_BAD_CONTENT_ENCODING, { 61 - Unrecognized/bad encoding }
    CURLE_LDAP_INVALID_URL,     { 62 - Invalid LDAP URL }
    CURLE_FILESIZE_EXCEEDED,    { 63 - Maximum file size exceeded }
    CURLE_USE_SSL_FAILED,       { 64 - Requested FTP SSL level failed }
    CURLE_SEND_FAIL_REWIND,     { 65 - Sending the data requires a rewind
                                     that failed }
    CURLE_SSL_ENGINE_INITFAILED,{ 66 - failed to initialise ENGINE }
    CURLE_LOGIN_DENIED,         { 67 - user, password or similar was not
                                     accepted and we failed to login }
    CURLE_TFTP_NOTFOUND,        { 68 - file not found on server }
    CURLE_TFTP_PERM,            { 69 - permission problem on server }
    CURLE_REMOTE_DISK_FULL,     { 70 - out of disk space on server }
    CURLE_TFTP_ILLEGAL,         { 71 - Illegal TFTP operation }
    CURLE_TFTP_UNKNOWNID,       { 72 - Unknown transfer ID }
    CURLE_REMOTE_FILE_EXISTS,   { 73 - File already exists }
    CURLE_TFTP_NOSUCHUSER,      { 74 - No such user }
    CURLE_CONV_FAILED,          { 75 - conversion failed }
    CURLE_CONV_REQD,            { 76 - caller must register conversion
                                     callbacks using curl_easy_setopt options
                                     CURLOPT_CONV_FROM_NETWORK_FUNCTION,
                                     CURLOPT_CONV_TO_NETWORK_FUNCTION, and
                                     CURLOPT_CONV_FROM_UTF8_FUNCTION }
    CURLE_SSL_CACERT_BADFILE,   { 77 - could not load CACERT file, missing
                                    or wrong format }
    CURLE_REMOTE_FILE_NOT_FOUND,{ 78 - remote file not found }
    CURLE_SSH,                  { 79 - error from the SSH layer, somewhat
                                     generic so the error message will be of
                                     interest when this has happened }
    CURLE_SSL_SHUTDOWN_FAILED,  { 80 - Failed to shut down the SSL
                                     connection }
    CURLE_AGAIN,                { 81 - socket is not ready for send/recv,
                                     wait till it's ready and try again (Added
                                     in 7.18.2) }
    CURLE_SSL_CRL_BADFILE,      { 82 - could not load CRL file, missing or
                                     wrong format (Added in 7.19.0) }
    CURLE_SSL_ISSUER_ERROR,     { 83 - Issuer check failed.  (Added in
                                     7.19.0) }
    CURLE_FTP_PRET_FAILED,      { 84 - a PRET command failed }
    CURLE_RTSP_CSEQ_ERROR,      { 85 - mismatch of RTSP CSeq numbers }
    CURLE_RTSP_SESSION_ERROR,   { 86 - mismatch of RTSP Session Ids }
    CURLE_FTP_BAD_FILE_LIST,    { 87 - unable to parse FTP file list }
    CURLE_CHUNK_FAILED,         { 88 - chunk callback reported error }
    CURLE_NO_CONNECTION_AVAILABLE,{ 89 - No connection available, the
                                       session will be queued }
    CURLE_SSL_PINNEDPUBKEYNOTMATCH,{ 90 - specified pinned public key did not
                                        match }
    CURLE_SSL_INVALIDCERTSTATUS,{ 91 - invalid certificate status }
    CURLE_HTTP2_STREAM,         { 92 - stream error in HTTP/2 framing layer }
    CURLE_RECURSIVE_API_CALL,   { 93 - an api function was called from
                                     inside a callback }
    CURLE_AUTH_ERROR,           { 94 - an authentication function returned an
                                     error }
    CURLE_HTTP3,                { 95 - An HTTP/3 layer problem }
    CURLE_QUIC_CONNECT_ERROR,   { 96 - QUIC connection error }
    CURLE_PROXY,                { 97 - proxy handshake error }
    CURL_LAST                   { never use! }
  );

  { Proxy error codes. Returned in CURLINFO_PROXY_ERROR if CURLE_PROXY was
    return for the transfers. }
  CURLproxycode = (
    CURLPX_OK,
    CURLPX_BAD_ADDRESS_TYPE,
    CURLPX_BAD_VERSION,
    CURLPX_CLOSED,
    CURLPX_GSSAPI,
    CURLPX_GSSAPI_PERMSG,
    CURLPX_GSSAPI_PROTECTION,
    CURLPX_IDENTD,
    CURLPX_IDENTD_DIFFER,
    CURLPX_LONG_HOSTNAME,
    CURLPX_LONG_PASSWD,
    CURLPX_LONG_USER,
    CURLPX_NO_AUTH,
    CURLPX_RECV_ADDRESS,
    CURLPX_RECV_AUTH,
    CURLPX_RECV_CONNECT,
    CURLPX_RECV_REQACK,
    CURLPX_REPLY_ADDRESS_TYPE_NOT_SUPPORTED,
    CURLPX_REPLY_COMMAND_NOT_SUPPORTED,
    CURLPX_REPLY_CONNECTION_REFUSED,
    CURLPX_REPLY_GENERAL_SERVER_FAILURE,
    CURLPX_REPLY_HOST_UNREACHABLE,
    CURLPX_REPLY_NETWORK_UNREACHABLE,
    CURLPX_REPLY_NOT_ALLOWED,
    CURLPX_REPLY_TTL_EXPIRED,
    CURLPX_REPLY_UNASSIGNED,
    CURLPX_REQUEST_FAILED,
    CURLPX_RESOLVE_HOST,
    CURLPX_SEND_AUTH,
    CURLPX_SEND_CONNECT,
    CURLPX_SEND_REQUEST,
    CURLPX_UNKNOWN_FAIL,
    CURLPX_UNKNOWN_MODE,
    CURLPX_USER_REJECTED,
    CURLPX_LAST                 { never use }
  );

  curl_conv_callback = function (buffer : PChar; length : Cardinal) : CURLcode
    of object;

  curl_ssl_ctx_callback = function (curl : CURL; { easy handle }
    ssl_ctx : Pointer; { actually an OpenSSL SSL_CTX or an mbedTLS
    mbedtls_ssl_config } userptr : Pointer) : CURLcode of object;

  curl_proxytype = (
    CURLPROXY_HTTP              { added in 7.10, new in 7.19.4 }    = 0,
                                { default is to use CONNECT HTTP/1.1 }
    CURLPROXY_HTTP_1_0          { added in 7.19.4, force to use }   = 1,
                                { CONNECT HTTP/1.0 }
    CURLPROXY_HTTPS             { added in 7.52.0 }                 = 2,
    CURLPROXY_SOCKS4            { support added in 7.15.2, enum }   = 4,
                                { existed already in 7.10 }
    CURLPROXY_SOCKS5            { added in 7.10 }                   = 5,
    CURLPROXY_SOCKS4A           { added in 7.18.0 }                 = 6,
    CURLPROXY_SOCKS5_HOSTNAME   { Use the SOCKS5 protocol but }     = 7
                                { pass along the host name rather }
                                { than the IP address. added in 7.18.0 }
  );

  curl_khtype = (
    CURLKHTYPE_UNKNOWN,
    CURLKHTYPE_RSA1,
    CURLKHTYPE_RSA,
    CURLKHTYPE_DSS,
    CURLKHTYPE_ECDSA,
    CURLKHTYPE_ED25519
  );

  pcurl_khkey = ^curl_khkey;
  curl_khkey = record
    key : PChar;                { points to a zero-terminated string encoded }
                                { with base64 if len is zero, otherwise to the }
                                { "raw" data }
    len : Cardinal;
    keytype : curl_khtype;
  end;

  { this is the set of return values expected from the curl_sshkeycallback
    callback }
  curl_khstat = (
    CURLKHSTAT_FINE_ADD_TO_FILE,
    CURLKHSTAT_FINE,
    CURLKHSTAT_REJECT,         { reject the connection, return an error }
    CURLKHSTAT_DEFER,          { do not accept it, but we can't answer right }
                               { now so this causes a CURLE_DEFER error but }
                               { otherwise the connection will be left intact }
                               { etc }
    CURLKHSTAT_FINE_REPLACE,   { accept and replace the wrong key }
    CURLKHSTAT_LAST            { not for use, only a marker for last-in-list }
   );

  { this is the set of status codes pass in to the callback }
  curl_khmatch = (
    CURLKHMATCH_OK,            { match }
    CURLKHMATCH_MISMATCH,      { host found, key mismatch! }
    CURLKHMATCH_MISSING,       { no matching host/key found }
    CURLKHMATCH_LAST           { not for use, only a marker for last-in-list }
  );

  curl_sshkeycallback = function (
    easy : CURL;               { easy handle }
    knownkey : pcurl_khkey;    { known }
    foundkey : pcurl_khkey;    { found }
    keys : curl_khmatch;       { libcurl's view on the keys }
    clientp : Pointer          { custom pointer passed from app }
  ) : Integer of object;

  { parameter for the CURLOPT_USE_SSL option }
  curl_usessl = (
    CURLUSESSL_NONE,           { do not attempt to use SSL }
    CURLUSESSL_TRY,            { try using SSL, proceed anyway otherwise }
    CURLUSESSL_CONTROL,        { SSL for the control connection or fail }
    CURLUSESSL_ALL,            { SSL for all communication or fail }
    CURLUSESSL_LAST            { not an option, never use }
  );

  { parameter for the CURLOPT_FTP_SSL_CCC option }
  curl_ftpccc = (
    CURLFTPSSL_CCC_NONE,       { do not send CCC }
    CURLFTPSSL_CCC_PASSIVE,    { Let the server initiate the shutdown }
    CURLFTPSSL_CCC_ACTIVE,     { Initiate the shutdown }
    CURLFTPSSL_CCC_LAST        { not an option, never use }
  );

  { parameter for the CURLOPT_FTPSSLAUTH option }
  curl_ftpauth = (
    CURLFTPAUTH_DEFAULT,       { let libcurl decide }
    CURLFTPAUTH_SSL,           { use "AUTH SSL" }
    CURLFTPAUTH_TLS,           { use "AUTH TLS" }
    CURLFTPAUTH_LAST           { not an option, never use }
  );

  { parameter for the CURLOPT_FTP_CREATE_MISSING_DIRS option }
  curl_ftpcreatedir = (
    CURLFTP_CREATE_DIR_NONE,   { do NOT create missing dirs! }
    CURLFTP_CREATE_DIR,        { (FTP/SFTP) if CWD fails, try MKD and then CWD
                                 again if MKD succeeded, for SFTP this does
                                 similar magic }
    CURLFTP_CREATE_DIR_RETRY,  { (FTP only) if CWD fails, try MKD and then CWD
                                 again even if MKD failed! }
    CURLFTP_CREATE_DIR_LAST    { not an option, never use }
  );

  { parameter for the CURLOPT_FTP_FILEMETHOD option }
  curl_ftpmethod = (
    CURLFTPMETHOD_DEFAULT,     { let libcurl pick }
    CURLFTPMETHOD_MULTICWD,    { single CWD operation for each path part }
    CURLFTPMETHOD_NOCWD,       { no CWD at all }
    CURLFTPMETHOD_SINGLECWD,   { one CWD to full dir, then work on file }
    CURLFTPMETHOD_LAST         { not an option, never use }
  );

  CURLoption = (
    { This is the FILE * or void * the regular output should be written to. }
    CURLOPT_WRITEDATA                         = CURLOPTTYPE_CBPOINT       + 1,

    { The full URL to get/put }
    CURLOPT_URL                               = CURLOPTTYPE_STRINGPOINT   + 2,

    { Port number to connect to, if other than default. }
    CURLOPT_PORT                              = CURLOPTTYPE_LONG + 3{%H-},

    { Name of proxy to use. }
    CURLOPT_PROXY                             = CURLOPTTYPE_STRINGPOINT   + 4,

    { "user:password;options" to use when fetching. }
    CURLOPT_USERPWD                           = CURLOPTTYPE_STRINGPOINT   + 5,

    { "user:password" to use with proxy. }
    CURLOPT_PROXYUSERPWD                      = CURLOPTTYPE_STRINGPOINT   + 6,

    { Range to get, specified as an ASCII string. }
    CURLOPT_RANGE                             = CURLOPTTYPE_STRINGPOINT   + 7,

    { not used }

    { Specified file stream to upload from (use as input): }
    CURLOPT_READDATA                          = CURLOPTTYPE_CBPOINT       + 9,

    { Buffer to receive error messages in, must be at least CURL_ERROR_SIZE
      bytes big. }
    CURLOPT_ERRORBUFFER                       = CURLOPTTYPE_OBJECTPOINT   + 10,

    { Function that will be called to store the output (instead of fwrite). The
      parameters will use fwrite() syntax, make sure to follow them. }
    CURLOPT_WRITEFUNCTION                     = CURLOPTTYPE_FUNCTIONPOINT + 11,

    { Function that will be called to read the input (instead of fread). The
      parameters will use fread() syntax, make sure to follow them. }
    CURLOPT_READFUNCTION                      = CURLOPTTYPE_FUNCTIONPOINT + 12,

    { Time-out the read operation after this amount of seconds }
    CURLOPT_TIMEOUT                           = CURLOPTTYPE_LONG          + 13,

    { If the CURLOPT_INFILE is used, this can be used to inform libcurl about
      how large the file being sent really is. That allows better error
      checking and better verifies that the upload was successful. -1 means
      unknown size.

      For large file support, there is also a _LARGE version of the key
      which takes an off_t type, allowing platforms with larger off_t
      sizes to handle larger files.  See below for INFILESIZE_LARGE. }
    CURLOPT_INFILESIZE                        = CURLOPTTYPE_LONG          + 14,

    { POST static input fields. }
    CURLOPT_POSTFIELDS                        = CURLOPTTYPE_OBJECTPOINT   + 15,

    { Set the referrer page (needed by some CGIs) }
    CURLOPT_REFERER                           = CURLOPTTYPE_STRINGPOINT   + 16,

    { Set the FTP PORT string (interface name, named or numerical IP address)
      Use i.e '-' to use default address. }
    CURLOPT_FTPPORT                           = CURLOPTTYPE_STRINGPOINT   + 17,

    { Set the User-Agent string (examined by some CGIs) }
    CURLOPT_USERAGENT                         = CURLOPTTYPE_STRINGPOINT   + 18,

    { If the download receives less than "low speed limit" bytes/second
      during "low speed time" seconds, the operations is aborted.
      You could i.e if you have a pretty high speed connection, abort if
      it is less than 2000 bytes/sec during 20 seconds. }
    { Set the "low speed limit" }
    CURLOPT_LOW_SPEED_LIMIT                  = CURLOPTTYPE_LONG           + 19,

    { Set the "low speed time" }
    CURLOPT_LOW_SPEED_TIME                   = CURLOPTTYPE_LONG           + 20,

    { Set the continuation offset.

      Note there is also a _LARGE version of this key which uses
      off_t types, allowing for large file offsets on platforms which
      use larger-than-32-bit off_t's.  Look below for RESUME_FROM_LARGE. }
    CURLOPT_RESUME_FROM                      = CURLOPTTYPE_LONG           + 21,

    { Set cookie in request: }
    CURLOPT_COOKIE                           = CURLOPTTYPE_STRINGPOINT    + 22,

    { This points to a linked list of headers, struct curl_slist kind. This
      list is also used for RTSP (in spite of its name) }
    CURLOPT_HTTPHEADER                       = CURLOPTTYPE_SLISTPOINT     + 23,
    { three convenient "aliases" that follow the name scheme better }
    CURLOPT_RTSPHEADER                       = CURLOPT_HTTPHEADER,

    { This points to a linked list of post entries, struct curl_httppost }
    CURLOPT_HTTPPOST                         = CURLOPTTYPE_OBJECTPOINT    + 24,

    { name of the file keeping your private SSL-certificate }
    CURLOPT_SSLCERT                          = CURLOPTTYPE_STRINGPOINT    + 25,

    { password for the SSL or SSH private key }
    CURLOPT_KEYPASSWD                        = CURLOPTTYPE_STRINGPOINT    + 26,
    CURLOPT_SSLKEYPASSWD                     = CURLOPT_KEYPASSWD,
    CURLOPT_SSLCERTPASSWD                    = CURLOPT_KEYPASSWD,

    { send TYPE parameter? }
    CURLOPT_CRLF                             = CURLOPTTYPE_LONG           + 27,

    { send linked-list of QUOTE commands }
    CURLOPT_QUOTE                            = CURLOPTTYPE_SLISTPOINT     + 28,

    { send FILE * or void * to store headers to, if you use a callback it
      is simply passed to the callback unmodified }
    CURLOPT_HEADERDATA                       = CURLOPTTYPE_CBPOINT        + 29,

    { point to a file to read the initial cookies from, also enables
     "cookie awareness" }
    CURLOPT_COOKIEFILE                       = CURLOPTTYPE_STRINGPOINT    + 31,

    { What version to specifically try to use.
      See CURL_SSLVERSION defines below. }
    CURLOPT_SSLVERSION                       = CURLOPTTYPE_VALUES         + 32,

    { What kind of HTTP time condition to use, see defines }
    CURLOPT_TIMECONDITION                    = CURLOPTTYPE_VALUES         + 33,

    { Time to use with the above condition. Specified in number of seconds
      since 1 Jan 1970 }
    CURLOPT_TIMEVALUE                        = CURLOPTTYPE_LONG           + 34,

    { 35 = OBSOLETE }

    { Custom request, for customizing the get command like
      HTTP: DELETE, TRACE and others
      FTP: to use a different list command }
    CURLOPT_CUSTOMREQUEST                    = CURLOPTTYPE_STRINGPOINT    + 36,

    { FILE handle to use instead of stderr }
    CURLOPT_STDERR                           = CURLOPTTYPE_OBJECTPOINT    + 37,

    { 38 is not used }

    { send linked-list of post-transfer QUOTE commands }
    CURLOPT_POSTQUOTE                        = CURLOPTTYPE_SLISTPOINT     + 39,

    { OBSOLETE, do not use! }
    CURLOPT_OBSOLETE40                       = CURLOPTTYPE_OBJECTPOINT    + 40,

    { talk a lot }
    CURLOPT_VERBOSE                          = CURLOPTTYPE_LONG           + 41,

    { throw the header out too }
    CURLOPT_HEADER                           = CURLOPTTYPE_LONG           + 42,

    { shut off the progress meter }
    CURLOPT_NOPROGRESS                       = CURLOPTTYPE_LONG           + 43,

    { use HEAD to get http document }
    CURLOPT_NOBODY                           = CURLOPTTYPE_LONG           + 44,

    { no output on http error codes >= 400 }
    CURLOPT_FAILONERROR                      = CURLOPTTYPE_LONG           + 45,

    { this is an upload }
    CURLOPT_UPLOAD                           = CURLOPTTYPE_LONG           + 46,

    { HTTP POST method }
    CURLOPT_POST                             = CURLOPTTYPE_LONG           + 47,

    { bare names when listing directories }
    CURLOPT_DIRLISTONLY                      = CURLOPTTYPE_LONG           + 48,
    CURLOPT_FTPLISTONLY                      = CURLOPT_DIRLISTONLY,

    { Append instead of overwrite on upload! }
    CURLOPT_APPEND                           = CURLOPTTYPE_LONG           + 50,
    CURLOPT_FTPAPPEND                        = CURLOPT_APPEND,

    { Specify whether to read the user+password from the .netrc or the URL.
      This must be one of the CURL_NETRC_* enums below. }
    CURLOPT_NETRC                            = CURLOPTTYPE_VALUES         + 51,

    { use Location: Luke! }
    CURLOPT_FOLLOWLOCATION                   = CURLOPTTYPE_LONG           + 52,

    { transfer data in text/ASCII format }
    CURLOPT_TRANSFERTEXT                     = CURLOPTTYPE_LONG           + 53,

    { HTTP PUT }
    CURLOPT_PUT                              = CURLOPTTYPE_LONG           + 54,

    { 55 = OBSOLETE }
    { 56 DEPRECATED }
    { Function that will be called instead of the internal progress display
      function. This function should be defined as the curl_progress_callback
      prototype defines. }
    CURLOPT_PROGRESSFUNCTION                  = CURLOPTTYPE_FUNCTIONPOINT + 56 ,

    { Data passed to the CURLOPT_PROGRESSFUNCTION and CURLOPT_XFERINFOFUNCTION
      callbacks }
    CURLOPT_PROGRESSDATA                     = CURLOPTTYPE_CBPOINT        + 57,
    CURLOPT_XFERINFODATA                     = CURLOPT_PROGRESSDATA,

    { We want the referrer field set automatically when following locations }
    CURLOPT_AUTOREFERER                      = CURLOPTTYPE_LONG           + 58,

    { Port of the proxy, can be set in the proxy string as well with:
      "[host]:[port]" }
    CURLOPT_PROXYPORT                        = CURLOPTTYPE_LONG           + 59,

    { size of the POST input data, if strlen() is not good to use }
    CURLOPT_POSTFIELDSIZE                    = CURLOPTTYPE_LONG           + 60,

    { tunnel non-http operations through a HTTP proxy }
    CURLOPT_HTTPPROXYTUNNEL                  = CURLOPTTYPE_LONG           + 61,

    { Set the interface string to use as outgoing network interface }
    CURLOPT_INTERFACE                        = CURLOPTTYPE_STRINGPOINT    + 62,

    { Set the krb4/5 security level, this also enables krb4/5 awareness.  This
      is a string, 'clear', 'safe', 'confidential' or 'private'.  If the string
      is set but doesn't match one of these, 'private' will be used. }
    CURLOPT_KRBLEVEL                         = CURLOPTTYPE_STRINGPOINT    + 63,
    CURLOPT_KRB4LEVEL                        = CURLOPT_KRBLEVEL,

    { Set if we should verify the peer in ssl handshake, set 1 to verify. }
    CURLOPT_SSL_VERIFYPEER                   = CURLOPTTYPE_LONG           + 64,

    { The CApath or CAfile used to validate the peer certificate
      this option is used only if SSL_VERIFYPEER is true }
    CURLOPT_CAINFO                           = CURLOPTTYPE_STRINGPOINT    + 65,

    { 66 = OBSOLETE }
    { 67 = OBSOLETE }

    { Maximum number of http redirects to follow }
    CURLOPT_MAXREDIRS                        = CURLOPTTYPE_LONG           + 68,

    { Pass a long set to 1 to get the date of the requested document (if
      possible)! Pass a zero to shut it off. }
    CURLOPT_FILETIME                         = CURLOPTTYPE_LONG           + 69,

    { This points to a linked list of telnet options }
    CURLOPT_TELNETOPTIONS                    = CURLOPTTYPE_SLISTPOINT     + 70,

    { Max amount of cached alive connections }
    CURLOPT_MAXCONNECTS                      = CURLOPTTYPE_LONG           + 71,

    { OBSOLETE, do not use! }
    CURLOPT_OBSOLETE72                       = CURLOPTTYPE_LONG           + 72,

    { 73 = OBSOLETE }

    { Set to explicitly use a new connection for the upcoming transfer.
      Do not use this unless you're absolutely sure of this, as it makes the
      operation slower and is less friendly for the network. }
    CURLOPT_FRESH_CONNECT                    = CURLOPTTYPE_LONG           + 74,

    { Set to explicitly forbid the upcoming transfer's connection to be re-used
      when done. Do not use this unless you're absolutely sure of this, as it
      makes the operation slower and is less friendly for the network. }
    CURLOPT_FORBID_REUSE                     = CURLOPTTYPE_LONG           + 75,

    { Set to a file name that contains random data for libcurl to use to
      seed the random engine when doing SSL connects. }
    CURLOPT_RANDOM_FILE                      = CURLOPTTYPE_STRINGPOINT    + 76,

    { Set to the Entropy Gathering Daemon socket pathname }
    CURLOPT_EGDSOCKET                        = CURLOPTTYPE_STRINGPOINT    + 77,

    { Time-out connect operations after this amount of seconds, if connects are
      OK within this time, then fine... This only aborts the connect phase. }
    CURLOPT_CONNECTTIMEOUT                   = CURLOPTTYPE_LONG           + 78,

    { Function that will be called to store headers (instead of fwrite). The
      parameters will use fwrite() syntax, make sure to follow them. }
    CURLOPT_HEADERFUNCTION                   = CURLOPTTYPE_FUNCTIONPOINT  + 79,

    { Set this to force the HTTP request to get back to GET. Only really usable
      if POST, PUT or a custom request have been used first. }
    CURLOPT_HTTPGET                          = CURLOPTTYPE_LONG           + 80,

    { Set if we should verify the Common name from the peer certificate in ssl
      handshake, set 1 to check existence, 2 to ensure that it matches the
      provided hostname. }
    CURLOPT_SSL_VERIFYHOST                   = CURLOPTTYPE_LONG           + 81,

    { Specify which file name to write all known cookies in after completed
      operation. Set file name to "-" (dash) to make it go to stdout. }
    CURLOPT_COOKIEJAR                        = CURLOPTTYPE_STRINGPOINT    + 82,

    { Specify which SSL ciphers to use }
    CURLOPT_SSL_CIPHER_LIST                  = CURLOPTTYPE_STRINGPOINT    + 83,

    { Specify which HTTP version to use! This must be set to one of the
      CURL_HTTP_VERSION* enums set below. }
    CURLOPT_HTTP_VERSION                     = CURLOPTTYPE_VALUES         + 84,

    { Specifically switch on or off the FTP engine's use of the EPSV command.
      By default, that one will always be attempted before the more traditional
      PASV command. }
    CURLOPT_FTP_USE_EPSV                     = CURLOPTTYPE_LONG           + 85,

    { type of the file keeping your SSL-certificate ("DER", "PEM", "ENG") }
    CURLOPT_SSLCERTTYPE                      = CURLOPTTYPE_STRINGPOINT    + 86,

    { name of the file keeping your private SSL-key }
    CURLOPT_SSLKEY                           = CURLOPTTYPE_STRINGPOINT    + 87,

    { type of the file keeping your private SSL-key ("DER", "PEM", "ENG") }
    CURLOPT_SSLKEYTYPE                       = CURLOPTTYPE_STRINGPOINT    + 88,

    { crypto engine for the SSL-sub system }
    CURLOPT_SSLENGINE                        = CURLOPTTYPE_STRINGPOINT    + 89,

    { set the crypto engine for the SSL-sub system as default
      the param has no meaning... }
    CURLOPT_SSLENGINE_DEFAULT                = CURLOPTTYPE_LONG           + 90,

    { Non-zero value means to use the global dns cache }
    { DEPRECATED, do not use! }
    CURLOPT_DNS_USE_GLOBAL_CACHE             = CURLOPTTYPE_LONG           + 91,

    { DNS cache timeout }
    CURLOPT_DNS_CACHE_TIMEOUT                = CURLOPTTYPE_LONG           + 92,

    { send linked-list of pre-transfer QUOTE commands }
    CURLOPT_PREQUOTE                         = CURLOPTTYPE_SLISTPOINT     + 93,

    { set the debug function }
    CURLOPT_DEBUGFUNCTION                    = CURLOPTTYPE_FUNCTIONPOINT  + 94,

    { set the data for the debug function }
    CURLOPT_DEBUGDATA                        = CURLOPTTYPE_CBPOINT        + 95,

    { mark this as start of a cookie session }
    CURLOPT_COOKIESESSION                    = CURLOPTTYPE_LONG           + 96,

    { The CApath directory used to validate the peer certificate
      this option is used only if SSL_VERIFYPEER is true }
    CURLOPT_CAPATH                           = CURLOPTTYPE_STRINGPOINT    + 97,

    { Instruct libcurl to use a smaller receive buffer }
    CURLOPT_BUFFERSIZE                       = CURLOPTTYPE_LONG           + 98,

    { Instruct libcurl to not use any signal/alarm handlers, even when using
      timeouts. This option is useful for multi-threaded applications.
      See libcurl-the-guide for more background information. }
    CURLOPT_NOSIGNAL                         = CURLOPTTYPE_LONG           + 99,

    { Provide a CURLShare for mutexing non-ts data }
    CURLOPT_SHARE                            = CURLOPTTYPE_OBJECTPOINT    + 100,

    { Indicates type of proxy. accepted values are CURLPROXY_HTTP (default),
      CURLPROXY_HTTPS, CURLPROXY_SOCKS4, CURLPROXY_SOCKS4A and
      CURLPROXY_SOCKS5. }
    CURLOPT_PROXYTYPE                        = CURLOPTTYPE_VALUES         + 101,

    { Set the Accept-Encoding string. Use this to tell a server you would like
      the response to be compressed. Before 7.21.6, this was known as
      CURLOPT_ENCODING }
    CURLOPT_ACCEPT_ENCODING                  = CURLOPTTYPE_STRINGPOINT    + 102,

    { Set pointer to private data }
    CURLOPT_PRIVATE                          = CURLOPTTYPE_OBJECTPOINT    + 103,

    { Set aliases for HTTP 200 in the HTTP Response header }
    CURLOPT_HTTP200ALIASES                   = CURLOPTTYPE_SLISTPOINT     + 104,

    { Continue to send authentication (user+password) when following locations,
      even when hostname changed. This can potentially send off the name
      and password to whatever host the server decides. }
    CURLOPT_UNRESTRICTED_AUTH                = CURLOPTTYPE_LONG           + 105,

    { Specifically switch on or off the FTP engine's use of the EPRT command (
      it also disables the LPRT attempt). By default, those ones will always be
      attempted before the good old traditional PORT command. }
    CURLOPT_FTP_USE_EPRT                     = CURLOPTTYPE_LONG           + 106,

    { Set this to a bitmask value to enable the particular authentications
      methods you like. Use this in combination with CURLOPT_USERPWD.
      Note that setting multiple bits may cause extra network round-trips. }
    CURLOPT_HTTPAUTH                         = CURLOPTTYPE_VALUES         + 107,

    { Set the ssl context callback function, currently only for OpenSSL ssl_ctx
      in second argument. The function must be matching the
      curl_ssl_ctx_callback proto. }
    CURLOPT_SSL_CTX_FUNCTION                 = CURLOPTTYPE_FUNCTIONPOINT  + 108,

    { Set the userdata for the ssl context callback function's third
      argument }
    CURLOPT_SSL_CTX_DATA                     = CURLOPTTYPE_CBPOINT        + 109,

    { FTP Option that causes missing dirs to be created on the remote server.
      In 7.19.4 we introduced the convenience enums for this option using the
      CURLFTP_CREATE_DIR prefix. }
    CURLOPT_FTP_CREATE_MISSING_DIRS          = CURLOPTTYPE_LONG           + 110,

    { Set this to a bitmask value to enable the particular authentications
      methods you like. Use this in combination with CURLOPT_PROXYUSERPWD.
      Note that setting multiple bits may cause extra network round-trips. }
    CURLOPT_PROXYAUTH                        = CURLOPTTYPE_VALUES         + 111,

    { FTP option that changes the timeout, in seconds, associated with
      getting a response.  This is different from transfer timeout time and
      essentially places a demand on the FTP server to acknowledge commands
      in a timely manner. }
    CURLOPT_FTP_RESPONSE_TIMEOUT             = CURLOPTTYPE_LONG           + 112,
    CURLOPT_SERVER_RESPONSE_TIMEOUT          = CURLOPT_FTP_RESPONSE_TIMEOUT,

    { Set this option to one of the CURL_IPRESOLVE_* defines (see below) to
      tell libcurl to resolve names to those IP versions only. This only has
      affect on systems with support for more than one, i.e IPv4 _and_ IPv6. }
    CURLOPT_IPRESOLVE                        = CURLOPTTYPE_LONG           + 113,

    { Set this option to limit the size of a file that will be downloaded from
      an HTTP or FTP server.

      Note there is also _LARGE version which adds large file support for
      platforms which have larger off_t sizes.  See MAXFILESIZE_LARGE below. }
    CURLOPT_MAXFILESIZE                      = CURLOPTTYPE_LONG           + 114,

    { See the comment for INFILESIZE above, but in short, specifies
      the size of the file being uploaded.  -1 means unknown. }
    CURLOPT_INFILESIZE_LARGE                 = CURLOPTTYPE_OFF_T          + 115,

    { Sets the continuation offset.  There is also a LONG version of this;
      look above for RESUME_FROM. }
    CURLOPT_RESUME_FROM_LARGE                = CURLOPTTYPE_OFF_T          + 116,

    { Sets the maximum size of data that will be downloaded from
      an HTTP or FTP server.  See MAXFILESIZE above for the LONG version. }
    CURLOPT_MAXFILESIZE_LARGE                = CURLOPTTYPE_OFF_T          + 117,

    { Set this option to the file name of your .netrc file you want libcurl
      to parse (using the CURLOPT_NETRC option). If not set, libcurl will do
      a poor attempt to find the user's home directory and check for a .netrc
      file in there. }
    CURLOPT_NETRC_FILE                       = CURLOPTTYPE_STRINGPOINT    + 118,

    { Enable SSL/TLS for FTP, pick one of:
      CURLUSESSL_TRY     - try using SSL, proceed anyway otherwise
      CURLUSESSL_CONTROL - SSL for the control connection or fail
      CURLUSESSL_ALL     - SSL for all communication or fail }
    CURLOPT_USE_SSL                          = CURLOPTTYPE_VALUES         + 119,
    CURLOPT_FTP_SSL                          = CURLOPT_USE_SSL,

    { The _LARGE version of the standard POSTFIELDSIZE option }
    CURLOPT_POSTFIELDSIZE_LARGE              = CURLOPTTYPE_OFF_T          + 120,

    { Enable/disable the TCP Nagle algorithm }
    CURLOPT_TCP_NODELAY                      = CURLOPTTYPE_LONG           + 121,

    { 122 OBSOLETE, used in 7.12.3. Gone in 7.13.0 }
    { 123 OBSOLETE. Gone in 7.16.0 }
    { 124 OBSOLETE, used in 7.12.3. Gone in 7.13.0 }
    { 125 OBSOLETE, used in 7.12.3. Gone in 7.13.0 }
    { 126 OBSOLETE, used in 7.12.3. Gone in 7.13.0 }
    { 127 OBSOLETE. Gone in 7.16.0 }
    { 128 OBSOLETE. Gone in 7.16.0 }

    { When FTP over SSL/TLS is selected (with CURLOPT_USE_SSL), this option
      can be used to change libcurl's default action which is to first try
      "AUTH SSL" and then "AUTH TLS" in this order, and proceed when a OK
      response has been received.

      Available parameters are:
      CURLFTPAUTH_DEFAULT - let libcurl decide
      CURLFTPAUTH_SSL     - try "AUTH SSL" first, then TLS
      CURLFTPAUTH_TLS     - try "AUTH TLS" first, then SSL }
    CURLOPT_FTPSSLAUTH                       = CURLOPTTYPE_VALUES         + 129,

    CURLOPT_IOCTLFUNCTION                    = CURLOPTTYPE_FUNCTIONPOINT  + 130,

    CURLOPT_IOCTLDATA                        = CURLOPTTYPE_CBPOINT        + 131,

    { 132 OBSOLETE. Gone in 7.16.0 }
    { 133 OBSOLETE. Gone in 7.16.0 }

    { Zero terminated string for pass on to the FTP server when asked for
      "account" info }
    CURLOPT_FTP_ACCOUNT                      = CURLOPTTYPE_STRINGPOINT    + 134,

    { feed cookie into cookie engine }
    CURLOPT_COOKIELIST                       = CURLOPTTYPE_STRINGPOINT    + 135,

    { ignore Content-Length }
    CURLOPT_IGNORE_CONTENT_LENGTH            = CURLOPTTYPE_LONG           + 136,

    { Set to non-zero to skip the IP address received in a 227 PASV FTP server
      response. Typically used for FTP-SSL purposes but is not restricted to
      that. libcurl will then instead use the same IP address it used for the
      control connection. }
    CURLOPT_FTP_SKIP_PASV_IP                 = CURLOPTTYPE_LONG           + 137,

    { Select "file method" to use when doing FTP, see the curl_ftpmethod
      above. }
    CURLOPT_FTP_FILEMETHOD                   = CURLOPTTYPE_VALUES         + 138,

    { Local port number to bind the socket to }
    CURLOPT_LOCALPORT                        = CURLOPTTYPE_LONG           + 139,

    { Number of ports to try, including the first one set with LOCALPORT.
      Thus, setting it to 1 will make no additional attempts but the first. }
    CURLOPT_LOCALPORTRANGE                   = CURLOPTTYPE_LONG           + 140,

    { no transfer, set up connection and let application use the socket by
      extracting it with CURLINFO_LASTSOCKET }
    CURLOPT_CONNECT_ONLY                     = CURLOPTTYPE_LONG           + 141,

    { Function that will be called to convert from the
      network encoding (instead of using the iconv calls in libcurl) }
    CURLOPT_CONV_FROM_NETWORK_FUNCTION       = CURLOPTTYPE_FUNCTIONPOINT  + 142,

    { Function that will be called to convert to the
      network encoding (instead of using the iconv calls in libcurl) }
    CURLOPT_CONV_TO_NETWORK_FUNCTION         = CURLOPTTYPE_FUNCTIONPOINT  + 143,

    { Function that will be called to convert from UTF8
      (instead of using the iconv calls in libcurl)
      Note that this is used only for SSL certificate processing }
    CURLOPT_CONV_FROM_UTF8_FUNCTION          = CURLOPTTYPE_FUNCTIONPOINT  + 144,

    { if the connection proceeds too quickly then need to slow it down
      limit-rate: maximum number of bytes per second to send or receive }
    CURLOPT_MAX_SEND_SPEED_LARGE             = CURLOPTTYPE_OFF_T          + 145,
    CURLOPT_MAX_RECV_SPEED_LARGE             = CURLOPTTYPE_OFF_T          + 146,

    { Pointer to command string to send if USER/PASS fails. }
    CURLOPT_FTP_ALTERNATIVE_TO_USER          = CURLOPTTYPE_STRINGPOINT    + 147,

    { callback function for setting socket options }
    CURLOPT_SOCKOPTFUNCTION                  = CURLOPTTYPE_FUNCTIONPOINT  + 148,
    CURLOPT_SOCKOPTDATA                      = CURLOPTTYPE_CBPOINT        + 149,

    { set to 0 to disable session ID re-use for this transfer, default is
      enabled (== 1) }
    CURLOPT_SSL_SESSIONID_CACHE              = CURLOPTTYPE_LONG           + 150,

    { allowed SSH authentication methods }
    CURLOPT_SSH_AUTH_TYPES                   = CURLOPTTYPE_LONG           + 151,

    { Used by scp/sftp to do public/private key authentication }
    CURLOPT_SSH_PUBLIC_KEYFILE               = CURLOPTTYPE_STRINGPOINT    + 152,
    CURLOPT_SSH_PRIVATE_KEYFILE              = CURLOPTTYPE_STRINGPOINT    + 153,

    { Send CCC (Clear Command Channel) after authentication }
    CURLOPT_FTP_SSL_CCC                      = CURLOPTTYPE_LONG           + 154,

    { Same as TIMEOUT and CONNECTTIMEOUT, but with ms resolution }
    CURLOPT_TIMEOUT_MS                       = CURLOPTTYPE_LONG           + 155,
    CURLOPT_CONNECTTIMEOUT_MS                = CURLOPTTYPE_LONG           + 156,

    { set to zero to disable the libcurl's decoding and thus pass the raw body
      data to the application even when it is encoded/compressed }
    CURLOPT_HTTP_TRANSFER_DECODING           = CURLOPTTYPE_LONG           + 157,
    CURLOPT_HTTP_CONTENT_DECODING            = CURLOPTTYPE_LONG           + 158,

    { Permission used when creating new files and directories on the remote
      server for protocols that support it, SFTP/SCP/FILE }
    CURLOPT_NEW_FILE_PERMS                   = CURLOPTTYPE_LONG           + 159,
    CURLOPT_NEW_DIRECTORY_PERMS              = CURLOPTTYPE_LONG           + 160,

    { Set the behaviour of POST when redirecting. Values must be set to one
      of CURL_REDIR* defines below. This used to be called CURLOPT_POST301 }
    CURLOPT_POSTREDIR                        = CURLOPTTYPE_VALUES         + 161,
    CURLOPT_POST301                          = CURLOPT_POSTREDIR,

    { used by scp/sftp to verify the host's public key }
    CURLOPT_SSH_HOST_PUBLIC_KEY_MD5          = CURLOPTTYPE_STRINGPOINT    + 162,

    { Callback function for opening socket (instead of socket(2)). Optionally,
      callback is able change the address or refuse to connect returning
      CURL_SOCKET_BAD.  The callback should have type
      curl_opensocket_callback }
    CURLOPT_OPENSOCKETFUNCTION               = CURLOPTTYPE_FUNCTIONPOINT  + 163,
    CURLOPT_OPENSOCKETDATA                   = CURLOPTTYPE_CBPOINT        + 164,

    { POST volatile input fields. }
    CURLOPT_COPYPOSTFIELDS                   = CURLOPTTYPE_OBJECTPOINT    + 165,

    { set transfer mode (;type=<a|i>) when doing FTP via an HTTP proxy }
    CURLOPT_PROXY_TRANSFER_MODE              = CURLOPTTYPE_LONG           + 166,

    { Callback function for seeking in the input stream }
    CURLOPT_SEEKFUNCTION                     = CURLOPTTYPE_FUNCTIONPOINT  + 167,
    CURLOPT_SEEKDATA                         = CURLOPTTYPE_CBPOINT        + 168,

    { CRL file }
    CURLOPT_CRLFILE                          = CURLOPTTYPE_STRINGPOINT    + 169,

    { Issuer certificate }
    CURLOPT_ISSUERCERT                       = CURLOPTTYPE_STRINGPOINT    + 170,

    { (IPv6) Address scope }
    CURLOPT_ADDRESS_SCOPE                    = CURLOPTTYPE_LONG           + 171,

    { Collect certificate chain info and allow it to get retrievable with
      CURLINFO_CERTINFO after the transfer is complete. }
    CURLOPT_CERTINFO                         = CURLOPTTYPE_LONG           + 172,

    { "name" and "pwd" to use when fetching. }
    CURLOPT_USERNAME                         = CURLOPTTYPE_STRINGPOINT    + 173,
    CURLOPT_PASSWORD                         = CURLOPTTYPE_STRINGPOINT    + 174,

    { "name" and "pwd" to use with Proxy when fetching. }
    CURLOPT_PROXYUSERNAME                    = CURLOPTTYPE_STRINGPOINT    + 175,
    CURLOPT_PROXYPASSWORD                    = CURLOPTTYPE_STRINGPOINT    + 176,

    { Comma separated list of hostnames defining no-proxy zones. These should
      match both hostnames directly, and hostnames within a domain. For
      example, local.com will match local.com and www.local.com, but NOT
      notlocal.com or www.notlocal.com. For compatibility with other
      implementations of this, .local.com will be considered to be the same as
      local.com. A single * is the only valid wildcard, and effectively
      disables the use of proxy. }
    CURLOPT_NOPROXY                          = CURLOPTTYPE_STRINGPOINT    + 177,

    { block size for TFTP transfers }
    CURLOPT_TFTP_BLKSIZE                     = CURLOPTTYPE_LONG           + 178,

    { Socks Service }
    CURLOPT_SOCKS5_GSSAPI_SERVICE            = CURLOPTTYPE_STRINGPOINT    + 179,

    { Socks Service }
    CURLOPT_SOCKS5_GSSAPI_NEC                = CURLOPTTYPE_LONG           + 180,

    { set the bitmask for the protocols that are allowed to be used for the
      transfer, which thus helps the app which takes URLs from users or other
      external inputs and want to restrict what protocol(s) to deal
      with. Defaults to CURLPROTO_ALL. }
    CURLOPT_PROTOCOLS                        = CURLOPTTYPE_LONG           + 181,

    { set the bitmask for the protocols that libcurl is allowed to follow to,
      as a subset of the CURLOPT_PROTOCOLS ones. That means the protocol needs
      to be set in both bitmasks to be allowed to get redirected to. Defaults
      to all protocols except FILE and SCP. }
    CURLOPT_REDIR_PROTOCOLS                  = CURLOPTTYPE_LONG           + 182,

    { set the SSH knownhost file name to use }
    CURLOPT_SSH_KNOWNHOSTS                   = CURLOPTTYPE_STRINGPOINT    + 183,

    { set the SSH host key callback, must point to a curl_sshkeycallback
      function }
    CURLOPT_SSH_KEYFUNCTION                  = CURLOPTTYPE_FUNCTIONPOINT  + 184,

    { set the SSH host key callback custom pointer }
    CURLOPT_SSH_KEYDATA                      = CURLOPTTYPE_CBPOINT        + 185,

    { set the SMTP mail originator }
    CURLOPT_MAIL_FROM                        = CURLOPTTYPE_STRINGPOINT    + 186,

    { set the list of SMTP mail receiver(s) }
    CURLOPT_MAIL_RCPT                        = CURLOPTTYPE_SLISTPOINT     + 187,

    { FTP: send PRET before PASV }
    CURLOPT_FTP_USE_PRET                     = CURLOPTTYPE_LONG           + 188,

    { RTSP request method (OPTIONS, SETUP, PLAY, etc...) }
    CURLOPT_RTSP_REQUEST                     = CURLOPTTYPE_VALUES         + 189,

    { The RTSP session identifier }
    CURLOPT_RTSP_SESSION_ID                  = CURLOPTTYPE_STRINGPOINT    + 190,

    { The RTSP stream URI }
    CURLOPT_RTSP_STREAM_URI                  = CURLOPTTYPE_STRINGPOINT    + 191,

    { The Transport: header to use in RTSP requests }
    CURLOPT_RTSP_TRANSPORT                   = CURLOPTTYPE_STRINGPOINT    + 192,

    { Manually initialize the client RTSP CSeq for this handle }
    CURLOPT_RTSP_CLIENT_CSEQ                 = CURLOPTTYPE_LONG           + 193,

    { Manually initialize the server RTSP CSeq for this handle }
    CURLOPT_RTSP_SERVER_CSEQ                 = CURLOPTTYPE_LONG           + 194,

    { The stream to pass to INTERLEAVEFUNCTION. }
    CURLOPT_INTERLEAVEDATA                   = CURLOPTTYPE_CBPOINT        + 195,

    { Let the application define a custom write method for RTP data }
    CURLOPT_INTERLEAVEFUNCTION               = CURLOPTTYPE_FUNCTIONPOINT  + 196,

    { Turn on wildcard matching }
    CURLOPT_WILDCARDMATCH                    = CURLOPTTYPE_LONG           + 197,

    { Directory matching callback called before downloading of an
      individual file (chunk) started }
    CURLOPT_CHUNK_BGN_FUNCTION               = CURLOPTTYPE_FUNCTIONPOINT  + 198,

    { Directory matching callback called after the file (chunk)
      was downloaded, or skipped }
    CURLOPT_CHUNK_END_FUNCTION               = CURLOPTTYPE_FUNCTIONPOINT  + 199,

    { Change match (fnmatch-like) callback for wildcard matching }
    CURLOPT_FNMATCH_FUNCTION                 = CURLOPTTYPE_FUNCTIONPOINT  + 200,

    { Let the application define custom chunk data pointer }
    CURLOPT_CHUNK_DATA                       = CURLOPTTYPE_CBPOINT        + 201,

    { FNMATCH_FUNCTION user pointer }
    CURLOPT_FNMATCH_DATA                     = CURLOPTTYPE_CBPOINT        + 202,

    { send linked-list of name:port:address sets }
    CURLOPT_RESOLVE                          = CURLOPTTYPE_SLISTPOINT     + 203,

    { Set a username for authenticated TLS }
    CURLOPT_TLSAUTH_USERNAME                 = CURLOPTTYPE_STRINGPOINT    + 204,

    { Set a password for authenticated TLS }
    CURLOPT_TLSAUTH_PASSWORD                 = CURLOPTTYPE_STRINGPOINT    + 205,

    { Set authentication type for authenticated TLS }
    CURLOPT_TLSAUTH_TYPE                     = CURLOPTTYPE_STRINGPOINT    + 206,

    { Set to 1 to enable the "TE:" header in HTTP requests to ask for
      compressed transfer-encoded responses. Set to 0 to disable the use of TE:
      in outgoing requests. The current default is 0, but it might change in a
      future libcurl release.

      libcurl will ask for the compressed methods it knows of, and if that
      isn't any, it will not ask for transfer-encoding at all even if this
      option is set to 1. }
    CURLOPT_TRANSFER_ENCODING                = CURLOPTTYPE_LONG           + 207,

    { Callback function for closing socket (instead of close(2)). The callback
      should have type curl_closesocket_callback }
    CURLOPT_CLOSESOCKETFUNCTION              = CURLOPTTYPE_FUNCTIONPOINT  + 208,
    CURLOPT_CLOSESOCKETDATA                  = CURLOPTTYPE_CBPOINT        + 209,

    { allow GSSAPI credential delegation }
    CURLOPT_GSSAPI_DELEGATION                = CURLOPTTYPE_VALUES         + 210,

    { Set the name servers to use for DNS resolution }
    CURLOPT_DNS_SERVERS                      = CURLOPTTYPE_STRINGPOINT    + 211,

    { Time-out accept operations (currently for FTP only) after this amount
      of milliseconds. }
    CURLOPT_ACCEPTTIMEOUT_MS                 = CURLOPTTYPE_LONG           + 212,

    { Set TCP keepalive }
    CURLOPT_TCP_KEEPALIVE                    = CURLOPTTYPE_LONG           + 213,

    { non-universal keepalive knobs (Linux, AIX, HP-UX, more) }
    CURLOPT_TCP_KEEPIDLE                     = CURLOPTTYPE_LONG           + 214,
    CURLOPT_TCP_KEEPINTVL                    = CURLOPTTYPE_LONG           + 215,

    { Enable/disable specific SSL features with a bitmask, see CURLSSLOPT_* }
    CURLOPT_SSL_OPTIONS                      = CURLOPTTYPE_VALUES         + 216,

    { Set the SMTP auth originator }
    CURLOPT_MAIL_AUTH                        = CURLOPTTYPE_STRINGPOINT    + 217,

    { Enable/disable SASL initial response }
    CURLOPT_SASL_IR                          = CURLOPTTYPE_LONG           + 218,

    { Function that will be called instead of the internal progress display
      function. This function should be defined as the curl_xferinfo_callback
      prototype defines. (Deprecates CURLOPT_PROGRESSFUNCTION) }
    CURLOPT_XFERINFOFUNCTION                 = CURLOPTTYPE_FUNCTIONPOINT  + 219,

    { The XOAUTH2 bearer token }
    CURLOPT_XOAUTH2_BEARER                   = CURLOPTTYPE_STRINGPOINT    + 220,

    { Set the interface string to use as outgoing network
      interface for DNS requests.
      Only supported by the c-ares DNS backend }
    CURLOPT_DNS_INTERFACE                    = CURLOPTTYPE_STRINGPOINT    + 221,

    { Set the local IPv4 address to use for outgoing DNS requests.
      Only supported by the c-ares DNS backend }
    CURLOPT_DNS_LOCAL_IP4                    = CURLOPTTYPE_STRINGPOINT    + 222,

    { Set the local IPv4 address to use for outgoing DNS requests.
      Only supported by the c-ares DNS backend }
    CURLOPT_DNS_LOCAL_IP6                    = CURLOPTTYPE_STRINGPOINT    + 223,

    { Set authentication options directly }
    CURLOPT_LOGIN_OPTIONS                    = CURLOPTTYPE_STRINGPOINT    + 224,

    { Enable/disable TLS NPN extension (http2 over ssl might fail without) }
    CURLOPT_SSL_ENABLE_NPN                   = CURLOPTTYPE_LONG           + 225,

    { Enable/disable TLS ALPN extension (http2 over ssl might fail without) }
    CURLOPT_SSL_ENABLE_ALPN                  = CURLOPTTYPE_LONG           + 226,

    { Time to wait for a response to a HTTP request containing an
      Expect: 100-continue header before sending the data anyway. }
    CURLOPT_EXPECT_100_TIMEOUT_MS            = CURLOPTTYPE_LONG           + 227,

    { This points to a linked list of headers used for proxy requests only,
      struct curl_slist kind }
    CURLOPT_PROXYHEADER                      = CURLOPTTYPE_SLISTPOINT     + 228,

    { Pass in a bitmask of "header options" }
    CURLOPT_HEADEROPT                        = CURLOPTTYPE_VALUES         + 229,

    { The public key in DER form used to validate the peer public key
      this option is used only if SSL_VERIFYPEER is true }
    CURLOPT_PINNEDPUBLICKEY                  = CURLOPTTYPE_STRINGPOINT    + 230,

    { Path to Unix domain socket }
    CURLOPT_UNIX_SOCKET_PATH                 = CURLOPTTYPE_STRINGPOINT    + 231,

    { Set if we should verify the certificate status. }
    CURLOPT_SSL_VERIFYSTATUS                 = CURLOPTTYPE_LONG           + 232,

    { Set if we should enable TLS false start. }
    CURLOPT_SSL_FALSESTART                   = CURLOPTTYPE_LONG           + 233,

    { Do not squash dot-dot sequences }
    CURLOPT_PATH_AS_IS                       = CURLOPTTYPE_LONG           + 234,

    { Proxy Service Name }
    CURLOPT_PROXY_SERVICE_NAME               = CURLOPTTYPE_STRINGPOINT    + 235,

    { Service Name }
    CURLOPT_SERVICE_NAME                     = CURLOPTTYPE_STRINGPOINT    + 236,

    { Wait/don't wait for pipe/mutex to clarify }
    CURLOPT_PIPEWAIT                         = CURLOPTTYPE_LONG           + 237,

    { Set the protocol used when curl is given a URL without a protocol }
    CURLOPT_DEFAULT_PROTOCOL                 = CURLOPTTYPE_STRINGPOINT    + 238,

    { Set stream weight, 1 - 256 (default is 16) }
    CURLOPT_STREAM_WEIGHT                    = CURLOPTTYPE_LONG           + 239,

    { Set stream dependency on another CURL handle }
    CURLOPT_STREAM_DEPENDS                   = CURLOPTTYPE_OBJECTPOINT    + 240,

    { Set E-xclusive stream dependency on another CURL handle }
    CURLOPT_STREAM_DEPENDS_E                 = CURLOPTTYPE_OBJECTPOINT    + 241,

    { Do not send any tftp option requests to the server }
    CURLOPT_TFTP_NO_OPTIONS                  = CURLOPTTYPE_LONG           + 242,

    { Linked-list of host:port:connect-to-host:connect-to-port,
      overrides the URL's host:port (only for the network layer) }
    CURLOPT_CONNECT_TO                       = CURLOPTTYPE_SLISTPOINT     + 243,

    { Set TCP Fast Open }
    CURLOPT_TCP_FASTOPEN                     = CURLOPTTYPE_LONG           + 244,

    { Continue to send data if the server responds early with an
      HTTP status code >= 300 }
    CURLOPT_KEEP_SENDING_ON_ERROR            = CURLOPTTYPE_LONG           + 245,

    { The CApath or CAfile used to validate the proxy certificate
      this option is used only if PROXY_SSL_VERIFYPEER is true }
    CURLOPT_PROXY_CAINFO                     = CURLOPTTYPE_STRINGPOINT    + 246,

    { The CApath directory used to validate the proxy certificate
      this option is used only if PROXY_SSL_VERIFYPEER is true }
    CURLOPT_PROXY_CAPATH                     = CURLOPTTYPE_STRINGPOINT    + 247,

    { Set if we should verify the proxy in ssl handshake,
      set 1 to verify. }
    CURLOPT_PROXY_SSL_VERIFYPEER             = CURLOPTTYPE_LONG           + 248,

    { Set if we should verify the Common name from the proxy certificate in ssl
      handshake, set 1 to check existence, 2 to ensure that it matches
      the provided hostname. }
    CURLOPT_PROXY_SSL_VERIFYHOST             = CURLOPTTYPE_LONG           + 249,

    { What version to specifically try to use for proxy.
      See CURL_SSLVERSION defines below. }
    CURLOPT_PROXY_SSLVERSION                 = CURLOPTTYPE_LONG           + 250,

    { Set a username for authenticated TLS for proxy }
    CURLOPT_PROXY_TLSAUTH_USERNAME           = CURLOPTTYPE_STRINGPOINT    + 251,

    { Set a password for authenticated TLS for proxy }
    CURLOPT_PROXY_TLSAUTH_PASSWORD           = CURLOPTTYPE_STRINGPOINT    + 252,

    { Set authentication type for authenticated TLS for proxy }
    CURLOPT_PROXY_TLSAUTH_TYPE               = CURLOPTTYPE_STRINGPOINT    + 253,

    { name of the file keeping your private SSL-certificate for proxy }
    CURLOPT_PROXY_SSLCERT                    = CURLOPTTYPE_STRINGPOINT    + 254,

    { type of the file keeping your SSL-certificate ("DER", "PEM", "ENG") for
      proxy }
    CURLOPT_PROXY_SSLCERTTYPE                = CURLOPTTYPE_STRINGPOINT    + 255,

    { name of the file keeping your private SSL-key for proxy }
    CURLOPT_PROXY_SSLKEY                     = CURLOPTTYPE_STRINGPOINT    + 256,

    { type of the file keeping your private SSL-key ("DER", "PEM", "ENG") for
      proxy }
    CURLOPT_PROXY_SSLKEYTYPE                 = CURLOPTTYPE_STRINGPOINT    + 257,

    { password for the SSL private key for proxy }
    CURLOPT_PROXY_KEYPASSWD                  = CURLOPTTYPE_STRINGPOINT    + 258,

    { Specify which SSL ciphers to use for proxy }
    CURLOPT_PROXY_SSL_CIPHER_LIST            = CURLOPTTYPE_STRINGPOINT    + 259,

    { CRL file for proxy }
    CURLOPT_PROXY_CRLFILE                    = CURLOPTTYPE_STRINGPOINT    + 260,

    { Enable/disable specific SSL features with a bitmask for proxy, see
      CURLSSLOPT_* }
    CURLOPT_PROXY_SSL_OPTIONS                = CURLOPTTYPE_LONG           + 261,

    { Name of pre proxy to use. }
    CURLOPT_PRE_PROXY                        = CURLOPTTYPE_STRINGPOINT    + 262,

    { The public key in DER form used to validate the proxy public key
      this option is used only if PROXY_SSL_VERIFYPEER is true }
    CURLOPT_PROXY_PINNEDPUBLICKEY            = CURLOPTTYPE_STRINGPOINT    + 263,

    { Path to an abstract Unix domain socket }
    CURLOPT_ABSTRACT_UNIX_SOCKET             = CURLOPTTYPE_STRINGPOINT    + 264,

    { Suppress proxy CONNECT response headers from user callbacks }
    CURLOPT_SUPPRESS_CONNECT_HEADERS         = CURLOPTTYPE_LONG           + 265,

    { The request target, instead of extracted from the URL }
    CURLOPT_REQUEST_TARGET                   = CURLOPTTYPE_STRINGPOINT    + 266,

    { bitmask of allowed auth methods for connections to SOCKS5 proxies }
    CURLOPT_SOCKS5_AUTH                      = CURLOPTTYPE_LONG           + 267,

    { Enable/disable SSH compression }
    CURLOPT_SSH_COMPRESSION                  = CURLOPTTYPE_LONG           + 268,

    { Post MIME data. }
    CURLOPT_MIMEPOST                         = CURLOPTTYPE_OBJECTPOINT    + 269,

    { Time to use with the CURLOPT_TIMECONDITION. Specified in number of
      seconds since 1 Jan 1970. }
    CURLOPT_TIMEVALUE_LARGE                  = CURLOPTTYPE_OFF_T          + 270,

    { Head start in milliseconds to give happy eyeballs. }
    CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS        = CURLOPTTYPE_LONG           + 271,

    { Function that will be called before a resolver request is made }
    CURLOPT_RESOLVER_START_FUNCTION          = CURLOPTTYPE_FUNCTIONPOINT  + 272,

    { User data to pass to the resolver start callback. }
    CURLOPT_RESOLVER_START_DATA              = CURLOPTTYPE_OBJECTPOINT    + 273,

    { send HAProxy PROXY protocol header? }
    CURLOPT_HAPROXYPROTOCOL                  = CURLOPTTYPE_LONG           + 274,

    { shuffle addresses before use when DNS returns multiple }
    CURLOPT_DNS_SHUFFLE_ADDRESSES            = CURLOPTTYPE_LONG           + 275,

    { Specify which TLS 1.3 ciphers suites to use }
    CURLOPT_TLS13_CIPHERS                    = CURLOPTTYPE_STRINGPOINT    + 276,
    CURLOPT_PROXY_TLS13_CIPHERS              = CURLOPTTYPE_STRINGPOINT    + 277,

    { Disallow specifying username/login in URL. }
    CURLOPT_DISALLOW_USERNAME_IN_URL         = CURLOPTTYPE_LONG           + 278,

    { DNS-over-HTTPS URL }
    CURLOPT_DOH_URL                          = CURLOPTTYPE_STRINGPOINT    + 279,

    { Preferred buffer size to use for uploads }
    CURLOPT_UPLOAD_BUFFERSIZE                = CURLOPTTYPE_LONG           + 280,

    { Time in ms between connection upkeep calls for long-lived connections. }
    CURLOPT_UPKEEP_INTERVAL_MS               = CURLOPTTYPE_LONG           + 281,

    { Specify URL using CURL URL API. }
    CURLOPT_CURLU                            = CURLOPTTYPE_OBJECTPOINT    + 282,

    { add trailing data just after no more data is available }
    CURLOPT_TRAILERFUNCTION                  = CURLOPTTYPE_FUNCTIONPOINT  + 283,

    { pointer to be passed to HTTP_TRAILER_FUNCTION }
    CURLOPT_TRAILERDATA                      = CURLOPTTYPE_CBPOINT        + 284,

    { set this to 1L to allow HTTP/0.9 responses or 0L to disallow }
    CURLOPT_HTTP09_ALLOWED                   = CURLOPTTYPE_LONG           + 285,

    { alt-svc control bitmask }
    CURLOPT_ALTSVC_CTRL                      = CURLOPTTYPE_LONG           + 286,

    { alt-svc cache file name to possibly read from/write to }
    CURLOPT_ALTSVC                           = CURLOPTTYPE_STRINGPOINT    + 287,

    { maximum age of a connection to consider it for reuse (in seconds) }
    CURLOPT_MAXAGE_CONN                      = CURLOPTTYPE_LONG           + 288,

    { SASL authorisation identity }
    CURLOPT_SASL_AUTHZID                     = CURLOPTTYPE_STRINGPOINT    + 289,

    { allow RCPT TO command to fail for some recipients }
    CURLOPT_MAIL_RCPT_ALLLOWFAILS            = CURLOPTTYPE_LONG           + 290,

    { the private SSL-certificate as a "blob" }
    CURLOPT_SSLCERT_BLOB                     = CURLOPTTYPE_BLOB           + 291,
    CURLOPT_SSLKEY_BLOB                      = CURLOPTTYPE_BLOB           + 292,
    CURLOPT_PROXY_SSLCERT_BLOB               = CURLOPTTYPE_BLOB           + 293,
    CURLOPT_PROXY_SSLKEY_BLOB                = CURLOPTTYPE_BLOB           + 294,
    CURLOPT_ISSUERCERT_BLOB                  = CURLOPTTYPE_BLOB           + 295,

    { Issuer certificate for proxy }
    CURLOPT_PROXY_ISSUERCERT                 = CURLOPTTYPE_STRINGPOINT    + 296,
    CURLOPT_PROXY_ISSUERCERT_BLOB            = CURLOPTTYPE_BLOB           + 297,

    { the EC curves requested by the TLS client (RFC 8422, 5.1);
      OpenSSL support via 'set_groups'/'set_curves':
      https://www.openssl.org/docs/manmaster/man3/SSL_CTX_set1_groups.html }
    CURLOPT_SSL_EC_CURVES                    = CURLOPTTYPE_STRINGPOINT    + 298,

    { HSTS bitmask }
    CURLOPT_HSTS_CTRL                        = CURLOPTTYPE_LONG           + 299,

    { HSTS file name }
    CURLOPT_HSTS                             = CURLOPTTYPE_STRINGPOINT    + 300,

    { HSTS read callback }
    CURLOPT_HSTSREADFUNCTION                 = CURLOPTTYPE_FUNCTIONPOINT  + 301,
    CURLOPT_HSTSREADDATA                     = CURLOPTTYPE_CBPOINT        + 302,

    { HSTS write callback }
    CURLOPT_HSTSWRITEFUNCTION                = CURLOPTTYPE_FUNCTIONPOINT  + 303,
    CURLOPT_HSTSWRITEDATA                    = CURLOPTTYPE_CBPOINT        + 304,

    { the last unused }
    CURLOPT_LASTENTRY
  );

  { These enums are for use with the CURLOPT_HTTP_VERSION option. }
  curl_http_version = (
    CURL_HTTP_VERSION_NONE,    { setting this means we don't care, and that we'd
                                 like the library to choose the best possible
                                 for us! }
    CURL_HTTP_VERSION_1_0,     { please use HTTP 1.0 in the request }
    CURL_HTTP_VERSION_1_1,     { please use HTTP 1.1 in the request }
    CURL_HTTP_VERSION_2_0,     { please use HTTP 2 in the request }

    { Convenience definition simple because the name of the version is HTTP/2
      and not 2.0. The 2_0 version of the enum name was set while the version
      was still planned to be 2.0 and we stick to it for compatibility. }
    CURL_HTTP_VERSION_2 = CURL_HTTP_VERSION_2_0{%H-},

    CURL_HTTP_VERSION_2TLS, { use version 2 for HTTPS, version 1.1 for HTTP }
    CURL_HTTP_VERSION_2_PRIOR_KNOWLEDGE, { please use HTTP 2 without HTTP/1.1
                                           Upgrade }
    CURL_HTTP_VERSION_3 = 30,  { Makes use of explicit HTTP/3 without fallback.
                                 Use CURLOPT_ALTSVC to enable HTTP/3 upgrade }

    CURL_HTTP_VERSION_LAST     { *ILLEGAL* http version }
  );

  { Public API enums for RTSP requests }
  curl_rtsp_requests = (
    CURL_RTSPREQ_NONE,
    CURL_RTSPREQ_OPTIONS,
    CURL_RTSPREQ_DESCRIBE,
    CURL_RTSPREQ_ANNOUNCE,
    CURL_RTSPREQ_SETUP,
    CURL_RTSPREQ_PLAY,
    CURL_RTSPREQ_PAUSE,
    CURL_RTSPREQ_TEARDOWN,
    CURL_RTSPREQ_GET_PARAMETER,
    CURL_RTSPREQ_SET_PARAMETER,
    CURL_RTSPREQ_RECORD,
    CURL_RTSPREQ_RECEIVE,
    CURL_RTSPREQ_LAST
  );

  { These enums are for use with the CURLOPT_NETRC option. }
  CURL_NETRC_OPTION = (
    CURL_NETRC_IGNORED,        { The .netrc will never be read.
                                 This is the default. }
    CURL_NETRC_OPTIONAL,       { A user:password in the URL will be preferred
                                 to one in the .netrc. }
    CURL_NETRC_REQUIRED,       { A user:password in the URL will be ignored.
                                 Unless one is set programmatically, the .netrc
                                 will be queried. }
    CURL_NETRC_LAST
  );

  curl_ssl_version = (
    CURL_SSLVERSION_DEFAULT,
    CURL_SSLVERSION_TLSv1,     { TLS 1.x }
    CURL_SSLVERSION_SSLv2,
    CURL_SSLVERSION_SSLv3,
    CURL_SSLVERSION_TLSv1_0,
    CURL_SSLVERSION_TLSv1_1,
    CURL_SSLVERSION_TLSv1_2,
    CURL_SSLVERSION_TLSv1_3,

    CURL_SSLVERSION_LAST       { never use, keep last }
  );

  CURL_TLSAUTH = (
    CURL_TLSAUTH_NONE,
    CURL_TLSAUTH_SRP,
    CURL_TLSAUTH_LAST          { never use, keep last }
  );

  curl_TimeCond = (
    CURL_TIMECOND_NONE,

    CURL_TIMECOND_IFMODSINCE,
    CURL_TIMECOND_IFUNMODSINCE,
    CURL_TIMECOND_LASTMOD,

    CURL_TIMECOND_LAST
  );

  { Mime/form handling support. }
  curl_mime_s = Pointer;       { Mime context. }

  pcurl_mime = ^curl_mime;
  curl_mime = type curl_mime_s;

  curl_mimepart_s = Pointer;   { Mime part context. }

  pcurl_mimepart = ^curl_mimepart;
  curl_mimepart = type curl_mimepart_s;

  CURLformoption = (
    CURLFORM_NOTHING,

    CURLFORM_COPYNAME,
    CURLFORM_PTRNAME,
    CURLFORM_NAMELENGTH,
    CURLFORM_COPYCONTENTS,
    CURLFORM_PTRCONTENTS,
    CURLFORM_CONTENTSLENGTH,
    CURLFORM_FILECONTENT,
    CURLFORM_ARRAY,
    CURLFORM_OBSOLETE,
    CURLFORM_FILE,

    CURLFORM_BUFFER,
    CURLFORM_BUFFERPTR,
    CURLFORM_BUFFERLENGTH,

    CURLFORM_CONTENTTYPE,
    CURLFORM_CONTENTHEADER,
    CURLFORM_FILENAME,
    CURLFORM_END,
    CURLFORM_OBSOLETE2,

    CURLFORM_STREAM,
    CURLFORM_CONTENTLEN,       { added in 7.46.0, provide a curl_off_t length }

    CURLFORM_LASTENTRY
  );

  { structure to be used as parameter for CURLFORM_ARRAY }
  curl_forms = record
    option : CURLformoption;
    value : PChar;
  end;

  { use this for multipart formpost building }
  { Returns code for curl_formadd()

    Returns:
    CURL_FORMADD_OK             on success
    CURL_FORMADD_MEMORY         if the FormInfo allocation fails
    CURL_FORMADD_OPTION_TWICE   if one option is given twice for one Form
    CURL_FORMADD_NULL           if a null pointer was given for a char
    CURL_FORMADD_MEMORY         if the allocation of a FormInfo struct failed
    CURL_FORMADD_UNKNOWN_OPTION if an unknown option was used
    CURL_FORMADD_INCOMPLETE     if the some FormInfo is not complete (or error)
    CURL_FORMADD_MEMORY         if a curl_httppost struct cannot be allocated
    CURL_FORMADD_MEMORY         if some allocation for string copying failed.
    CURL_FORMADD_ILLEGAL_ARRAY  if an illegal option is used in an array
  }

  CURLFORMcode = (
    CURL_FORMADD_OK,           { first, no error }
    CURL_FORMADD_MEMORY,
    CURL_FORMADD_OPTION_TWICE,
    CURL_FORMADD_NULL,
    CURL_FORMADD_UNKNOWN_OPTION,
    CURL_FORMADD_INCOMPLETE,
    CURL_FORMADD_ILLEGAL_ARRAY,
    CURL_FORMADD_DISABLED,     { libcurl was built with this disabled }

    CURL_FORMADD_LAST          { last }
  );

  { callback function for curl_formget()
    The void *arg pointer will be the one passed as second argument to
    curl_formget().
    The character buffer passed to it must not be freed.
    Should return the buffer length passed to it as the argument "len" on
    success. }
  curl_formget_callback = function (arg : Pointer; const buf : PChar;
     len : Cardinal) : Cardinal of object;

  pcurl_ssl_backend = ^curl_ssl_backend;
  ppcurl_ssl_backend = ^pcurl_ssl_backend;
  pppcurl_ssl_backend = ^ppcurl_ssl_backend;
  curl_ssl_backend = record
    id : curl_sslbackend;
    name : PChar;
  end;

  CURLsslset = (
    CURLSSLSET_OK = 0,
    CURLSSLSET_UNKNOWN_BACKEND,
    CURLSSLSET_TOO_LATE,
    CURLSSLSET_NO_BACKENDS     { libcurl was built without any SSL support }
  );

  { info about the certificate chain, only for OpenSSL builds. Asked
    for with CURLOPT_CERTINFO / CURLINFO_CERTINFO }
  curl_certinfo = record
    num_of_certs : Integer;    { number of certificates with information }
    certinfo : ppcurl_slist;   { for each index in this array, there's a
                                 linked list with textual information in the
                                 format "name: value" }
  end;

  { Information about the SSL library used and the respective internal SSL
    handle, which can be used to obtain further information regarding the
    connection. Asked for with CURLINFO_TLS_SSL_PTR or CURLINFO_TLS_SESSION. }
  curl_tlssessioninfo = record
    backend : curl_sslbackend;
    internals : Pointer;
  end;

  CURLINFO = (
    CURLINFO_NONE,             { first, never use this }
    CURLINFO_EFFECTIVE_URL                   = CURLINFO_STRING            + 1,
    CURLINFO_RESPONSE_CODE                   = CURLINFO_LONG              + 2,

    { CURLINFO_RESPONSE_CODE is the new name for the option previously known as
      CURLINFO_HTTP_CODE }
    CURLINFO_HTTP_CODE                       = CURLINFO_LONG              + 2{%H-},
    CURLINFO_TOTAL_TIME                      = CURLINFO_DOUBLE            + 3,
    CURLINFO_NAMELOOKUP_TIME                 = CURLINFO_DOUBLE            + 4,
    CURLINFO_CONNECT_TIME                    = CURLINFO_DOUBLE            + 5,
    CURLINFO_PRETRANSFER_TIME                = CURLINFO_DOUBLE            + 6,
    CURLINFO_SIZE_UPLOAD                     = CURLINFO_DOUBLE            + 7,
    CURLINFO_SIZE_UPLOAD_T                   = CURLINFO_OFF_T             + 7,
    CURLINFO_SIZE_DOWNLOAD                   = CURLINFO_DOUBLE            + 8,
    CURLINFO_SIZE_DOWNLOAD_T                 = CURLINFO_OFF_T             + 8,
    CURLINFO_SPEED_DOWNLOAD                  = CURLINFO_DOUBLE            + 9,
    CURLINFO_SPEED_DOWNLOAD_T                = CURLINFO_OFF_T             + 9,
    CURLINFO_SPEED_UPLOAD                    = CURLINFO_DOUBLE            + 10,
    CURLINFO_SPEED_UPLOAD_T                  = CURLINFO_OFF_T             + 10,
    CURLINFO_HEADER_SIZE                     = CURLINFO_LONG              + 11,
    CURLINFO_REQUEST_SIZE                    = CURLINFO_LONG              + 12,
    CURLINFO_SSL_VERIFYRESULT                = CURLINFO_LONG              + 13,
    CURLINFO_FILETIME                        = CURLINFO_LONG              + 14,
    CURLINFO_FILETIME_T                      = CURLINFO_OFF_T             + 14,
    CURLINFO_CONTENT_LENGTH_DOWNLOAD         = CURLINFO_DOUBLE            + 15,
    CURLINFO_CONTENT_LENGTH_DOWNLOAD_T       = CURLINFO_OFF_T             + 15,
    CURLINFO_CONTENT_LENGTH_UPLOAD           = CURLINFO_DOUBLE            + 16,
    CURLINFO_CONTENT_LENGTH_UPLOAD_T         = CURLINFO_OFF_T             + 16,
    CURLINFO_STARTTRANSFER_TIME              = CURLINFO_DOUBLE            + 17,
    CURLINFO_CONTENT_TYPE                    = CURLINFO_STRING            + 18,
    CURLINFO_REDIRECT_TIME                   = CURLINFO_DOUBLE            + 19,
    CURLINFO_REDIRECT_COUNT                  = CURLINFO_LONG              + 20,
    CURLINFO_PRIVATE                         = CURLINFO_STRING            + 21,
    CURLINFO_HTTP_CONNECTCODE                = CURLINFO_LONG              + 22,
    CURLINFO_HTTPAUTH_AVAIL                  = CURLINFO_LONG              + 23,
    CURLINFO_PROXYAUTH_AVAIL                 = CURLINFO_LONG              + 24,
    CURLINFO_OS_ERRNO                        = CURLINFO_LONG              + 25,
    CURLINFO_NUM_CONNECTS                    = CURLINFO_LONG              + 26,
    CURLINFO_SSL_ENGINES                     = CURLINFO_SLIST             + 27,
    CURLINFO_COOKIELIST                      = CURLINFO_SLIST             + 28,
    CURLINFO_LASTSOCKET                      = CURLINFO_LONG              + 29,
    CURLINFO_FTP_ENTRY_PATH                  = CURLINFO_STRING            + 30,
    CURLINFO_REDIRECT_URL                    = CURLINFO_STRING            + 31,
    CURLINFO_PRIMARY_IP                      = CURLINFO_STRING            + 32,
    CURLINFO_APPCONNECT_TIME                 = CURLINFO_DOUBLE            + 33,
    CURLINFO_CERTINFO                        = CURLINFO_PTR               + 34,
    CURLINFO_CONDITION_UNMET                 = CURLINFO_LONG              + 35,
    CURLINFO_RTSP_SESSION_ID                 = CURLINFO_STRING            + 36,
    CURLINFO_RTSP_CLIENT_CSEQ                = CURLINFO_LONG              + 37,
    CURLINFO_RTSP_SERVER_CSEQ                = CURLINFO_LONG              + 38,
    CURLINFO_RTSP_CSEQ_RECV                  = CURLINFO_LONG              + 39,
    CURLINFO_PRIMARY_PORT                    = CURLINFO_LONG              + 40,
    CURLINFO_LOCAL_IP                        = CURLINFO_STRING            + 41,
    CURLINFO_LOCAL_PORT                      = CURLINFO_LONG              + 42,
    CURLINFO_TLS_SESSION                     = CURLINFO_PTR               + 43,
    CURLINFO_ACTIVESOCKET                    = CURLINFO_SOCKET            + 44,
    CURLINFO_TLS_SSL_PTR                     = CURLINFO_PTR               + 45,
    CURLINFO_HTTP_VERSION                    = CURLINFO_LONG              + 46,
    CURLINFO_PROXY_SSL_VERIFYRESULT          = CURLINFO_LONG              + 47,
    CURLINFO_PROTOCOL                        = CURLINFO_LONG              + 48,
    CURLINFO_SCHEME                          = CURLINFO_STRING            + 49,

    { Preferably these would be defined conditionally based on the
      sizeof curl_off_t being 64-bits }
    CURLINFO_TOTAL_TIME_T                    = CURLINFO_OFF_T             + 50,
    CURLINFO_NAMELOOKUP_TIME_T               = CURLINFO_OFF_T             + 51,
    CURLINFO_CONNECT_TIME_T                  = CURLINFO_OFF_T             + 52,
    CURLINFO_PRETRANSFER_TIME_T              = CURLINFO_OFF_T             + 53,
    CURLINFO_STARTTRANSFER_TIME_T            = CURLINFO_OFF_T             + 54,
    CURLINFO_REDIRECT_TIME_T                 = CURLINFO_OFF_T             + 55,
    CURLINFO_APPCONNECT_TIME_T               = CURLINFO_OFF_T             + 56,
    CURLINFO_RETRY_AFTER                     = CURLINFO_OFF_T             + 57,
    CURLINFO_EFFECTIVE_METHOD                = CURLINFO_STRING            + 58,
    CURLINFO_PROXY_ERROR                     = CURLINFO_LONG              + 59,

    CURLINFO_LASTONE                                                      = 59
  );

  curl_closepolicy = (
    CURLCLOSEPOLICY_NONE,      { first, never use this }

    CURLCLOSEPOLICY_OLDEST,
    CURLCLOSEPOLICY_LEAST_RECENTLY_USED,
    CURLCLOSEPOLICY_LEAST_TRAFFIC,
    CURLCLOSEPOLICY_SLOWEST,
    CURLCLOSEPOLICY_CALLBACK,

    CURLCLOSEPOLICY_LAST       { last, never use this }
  );

  curl_lock_data = (
    CURL_LOCK_DATA_NONE = 0,
    {  CURL_LOCK_DATA_SHARE is used internally to say that
       the locking is just made to change the internal state of the share
       itself. }
    CURL_LOCK_DATA_SHARE,
    CURL_LOCK_DATA_COOKIE,
    CURL_LOCK_DATA_DNS,
    CURL_LOCK_DATA_SSL_SESSION,
    CURL_LOCK_DATA_CONNECT,
    CURL_LOCK_DATA_PSL,
    CURL_LOCK_DATA_LAST
  );

  { Different lock access types }
  curl_lock_access = (
    CURL_LOCK_ACCESS_NONE      { unspecified action }                     = 0,
    CURL_LOCK_ACCESS_SHARED    { for read perhaps }                       = 1,
    CURL_LOCK_ACCESS_SINGLE    { for write perhaps }                      = 2,
    CURL_LOCK_ACCESS_LAST      { never use }
  );

  curl_lock_function = procedure (handle : CURL; data : curl_lock_data;
    locktype : curl_lock_access; userptr : Pointer) of object;

  curl_unlock_function = procedure (handle : CURL; data : curl_lock_data;
    userptr : Pointer) of object;

  CURLSHcode = (
    CURLSHE_OK,                { all is fine }
    CURLSHE_BAD_OPTION,        { 1 }
    CURLSHE_IN_USE,            { 2 }
    CURLSHE_INVALID,           { 3 }
    CURLSHE_NOMEM,             { 4 out of memory }
    CURLSHE_NOT_BUILT_IN,      { 5 feature not present in lib }
    CURLSHE_LAST               { never use }
  );

  CURLSHoption = (
    CURLSHOPT_NONE,            { don't use }
    CURLSHOPT_SHARE,           { specify a data type to share }
    CURLSHOPT_UNSHARE,         { specify which data type to stop sharing }
    CURLSHOPT_LOCKFUNC,        { pass in a 'curl_lock_function' pointer }
    CURLSHOPT_UNLOCKFUNC,      { pass in a 'curl_unlock_function' pointer }
    CURLSHOPT_USERDATA,        { pass in a user data pointer used in the
                                 lock/unlock callback functions }
    CURLSHOPT_LAST             { never use }
  );

  { Structures for querying information about the curl library at runtime. }
  CURLversion = (
    CURLVERSION_FIRST,
    CURLVERSION_SECOND,
    CURLVERSION_THIRD,
    CURLVERSION_FOURTH,
    CURLVERSION_FIFTH,
    CURLVERSION_SIXTH,
    CURLVERSION_SEVENTH,
    CURLVERSION_EIGHTH,
    CURLVERSION_LAST           { never actually use this }
  );

  pcurl_version_info_data = ^curl_version_info_data;
  curl_version_info_data = record
    age : CURLversion;         { age of the returned struct }
    version : PChar;           { LIBCURL_VERSION }
    version_num : Cardinal;    { LIBCURL_VERSION_NUM }
    host : PChar;              { OS/host/cpu/machine when configured }
    features : Integer;        { bitmask, see defines below }
    ssl_version : PChar;       { human readable string }
    ssl_version_num : Longint; { not used anymore, always 0 }
    libz_version : PChar;      { human readable string }
    protocols : PChar;         { protocols is terminated by an entry with a NULL
                                 protoname }
    ares : PChar;              { The fields below this were added in
                                 CURLVERSION_SECOND }
    ares_num : Integer;
    { This field was added in CURLVERSION_THIRD }
    libidn : PChar;

    { These field were added in CURLVERSION_FOURTH }
    iconv_ver_num : Integer;   { Same as '_libiconv_version' if built with
                                 HAVE_ICONV }
    libssh_version : PChar;    { human readable string }

    { These fields were added in CURLVERSION_FIFTH }
    brotli_ver_num : Cardinal; { Numeric Brotli version
                                 (MAJOR << 24) | (MINOR << 12) | PATCH }
    brotli_version : PChar;    { human readable string. }

    { These fields were added in CURLVERSION_SIXTH }
    nghttp2_ver_num : Cardinal;{ Numeric nghttp2 version
                                 (MAJOR << 16) | (MINOR << 8) | PATCH }
    nghttp2_version : PChar;   { human readable string. }
    quic_version : PChar;      { human readable quic (+ HTTP/3) library +
                                 version or NULL }
    
    { These fields were added in CURLVERSION_SEVENTH }
    cainfo : PChar;             { the built-in default CURLOPT_CAINFO, might
                                  be NULL }
    capath : PChar;             { the built-in default CURLOPT_CAPATH, might
                                  be NULL }

    { These fields were added in CURLVERSION_EIGHTH }
    zstd_ver_num : Cardinal;    { Numeric Zstd version
                                  (MAJOR << 24) | (MINOR << 12) | PATCH }
    zstd_version : PChar;       { human readable string. }
  end;

  CURLMcode = (
    CURLM_CALL_MULTI_PERFORM = -1, { please call curl_multi_perform() or
                                     curl_multi_socket*() soon }
    { just to make code nicer when using curl_multi_socket() you can now check
      for CURLM_CALL_MULTI_SOCKET too in the same style it works for
      curl_multi_perform() and CURLM_CALL_MULTI_PERFORM }
    CURLM_CALL_MULTI_SOCKET = CURLM_CALL_MULTI_PERFORM{%H-},

    CURLM_OK,
    CURLM_BAD_HANDLE,          { the passed-in handle is not a valid CURLM
                                 handle }
    CURLM_BAD_EASY_HANDLE,     { an easy handle was not good/valid }
    CURLM_OUT_OF_MEMORY,       { if you ever get this, you're in deep sh*t }
    CURLM_INTERNAL_ERROR,      { this is a libcurl bug }
    CURLM_BAD_SOCKET,          { the passed in socket argument did not match }
    CURLM_UNKNOWN_OPTION,      { curl_multi_setopt() with unsupported option }
    CURLM_ADDED_ALREADY,       { an easy handle already added to a multi handle
                                 was attempted to get added - again }
    CURLM_RECURSIVE_API_CALL,  { an api function was called from inside a
                                 callback }
    CURLM_WAKEUP_FAILURE,      { wakeup is unavailable or failed }
    CURLM_BAD_FUNCTION_ARGUMENT, { function called with a bad parameter }
    CURLM_LAST
  );

  CURLMSG = (
    CURLMSG_NONE,              { first, not used }
    CURLMSG_DONE,              { This easy handle has completed. 'result'
                                 contains the CURLcode of the transfer }
    CURLMSG_LAST               { last, not used }
  );

  CURLMsg_rec = record
    msg : CURLMSG;             { what this message means }
    easy_handle : CURL;        { the handle it concerns }
    case data : Integer of
      1 : (whatever : Pointer);
      2 : (result : CURLcode);
  end;

  curl_waitfd = record
    fd : curl_socket_t;
    events : ShortInt;
    revents : ShortInt;        { not supported yet }
  end;

  curl_socket_callback = function (
    easy : CURL;               { easy handle }
    s : curl_socket_t;         { socket }
    what : Integer;            { see above }
    userp : Pointer;           { private callback pointer }
    socketp : Pointer          { private socket pointer }
  ) : Integer of object;

  { Name:    curl_multi_timer_callback

    Desc:    Called by libcurl whenever the library detects a change in the
             maximum number of milliseconds the app is allowed to wait before
             curl_multi_socket() or curl_multi_perform() must be called
             (to allow libcurl's timed events to take place).

    Returns: The callback should return zero. }
  curl_multi_timer_callback = function (
    multi : CURLM;             { multi handle }
    timeout_ms : Longint;      { see above }
    userp : Pointer            { private callback pointer }
  ) : Integer of object;

  CURLMoption = (
    { This is the socket callback function pointer }
    CURLMOPT_SOCKETFUNCTION                  = CURLOPTTYPE_FUNCTIONPOINT  + 1,

    { This is the argument passed to the socket callback }
    CURLMOPT_SOCKETDATA                      = CURLOPTTYPE_OBJECTPOINT    + 2{%H-},

    { set to 1 to enable pipelining for this multi handle }
    CURLMOPT_PIPELINING                      = CURLOPTTYPE_LONG           + 3,

    { This is the timer callback function pointer }
    CURLMOPT_TIMERFUNCTION                   = CURLOPTTYPE_FUNCTIONPOINT  + 4,

    { This is the argument passed to the timer callback }
    CURLMOPT_TIMERDATA                       = CURLOPTTYPE_OBJECTPOINT    + 5,

    { maximum number of entries in the connection cache }
    CURLMOPT_MAXCONNECTS                     = CURLOPTTYPE_LONG           + 6,

    { maximum number of (pipelining) connections to one host }
    CURLMOPT_MAX_HOST_CONNECTIONS            = CURLOPTTYPE_LONG           + 7,

    { maximum number of requests in a pipeline }
    CURLMOPT_MAX_PIPELINE_LENGTH             = CURLOPTTYPE_LONG           + 8,

    { a connection with a content-length longer than this
      will not be considered for pipelining }
    CURLMOPT_CONTENT_LENGTH_PENALTY_SIZE     = CURLOPTTYPE_OFF_T          + 9,

    { a connection with a chunk length longer than this
      will not be considered for pipelining }
    CURLMOPT_CHUNK_LENGTH_PENALTY_SIZE       = CURLOPTTYPE_OFF_T          + 10,

    { a list of site names(+port) that are blacklisted from
      pipelining }
    CURLMOPT_PIPELINING_SITE_BL              = CURLOPTTYPE_OBJECTPOINT    + 11,

    { a list of server types that are blacklisted from
      pipelining }
    CURLMOPT_PIPELINING_SERVER_BL            = CURLOPTTYPE_OBJECTPOINT    + 12,

    { maximum number of open connections in total }
    CURLMOPT_MAX_TOTAL_CONNECTIONS           = CURLOPTTYPE_LONG           + 13,

    { This is the server push callback function pointer }
    CURLMOPT_PUSHFUNCTION                    = CURLOPTTYPE_FUNCTIONPOINT  + 14,

    { This is the argument passed to the server push callback }
    CURLMOPT_PUSHDATA                        = CURLOPTTYPE_OBJECTPOINT    + 15,

    { maximum number of concurrent streams to support on a connection }
    CURLMOPT_MAX_CONCURRENT_STREAMS          = CURLOPTTYPE_LONG           + 16,

    { the last unused }
    CURLMOPT_LASTENTRY
  );

  { Name: curl_push_callback

    Desc: This callback gets called when a new stream is being pushed by the
          server. It approves or denies the new stream.

    Returns: CURL_PUSH_OK or CURL_PUSH_DENY. }
  pcurl_pushheaders = ^curl_pushheaders;
  curl_pushheaders = Pointer;

  curl_push_callback = function (parent : CURL; easy : CURL; num_header :
    Cardinal; headers : pcurl_pushheaders; userp : Pointer) : Integer of object;

  { the error codes for the URL API }
  CURLUcode = (
    CURLUE_OK,
    CURLUE_BAD_HANDLE,         { 1 }
    CURLUE_BAD_PARTPOINTER,    { 2 }
    CURLUE_MALFORMED_INPUT,    { 3 }
    CURLUE_BAD_PORT_NUMBER,    { 4 }
    CURLUE_UNSUPPORTED_SCHEME, { 5 }
    CURLUE_URLDECODE,          { 6 }
    CURLUE_OUT_OF_MEMORY,      { 7 }
    CURLUE_USER_NOT_ALLOWED,   { 8 }
    CURLUE_UNKNOWN_PART,       { 9 }
    CURLUE_NO_SCHEME,          { 10 }
    CURLUE_NO_USER,            { 11 }
    CURLUE_NO_PASSWORD,        { 12 }
    CURLUE_NO_OPTIONS,         { 13 }
    CURLUE_NO_HOST,            { 14 }
    CURLUE_NO_PORT,            { 15 }
    CURLUE_NO_QUERY,           { 16 }
    CURLUE_NO_FRAGMENT         { 17 }
  );

  CURLUPart = (
    CURLUPART_URL,
    CURLUPART_SCHEME,
    CURLUPART_USER,
    CURLUPART_PASSWORD,
    CURLUPART_OPTIONS,
    CURLUPART_HOST,
    CURLUPART_PORT,
    CURLUPART_PATH,
    CURLUPART_QUERY,
    CURLUPART_FRAGMENT,
    CURLUPART_ZONEID           { added in 7.65.0 }
  );

  PCURLU = ^CURLU;
  CURLU = Pointer;

  PPChar = ^PChar;

  curl_easytype = (
    CURLOT_LONG,    { long (a range of values) }
    CURLOT_VALUES,  {      (a defined set or bitmask) }
    CURLOT_OFF_T,   { curl_off_t (a range of values) }
    CURLOT_OBJECT,  { pointer (void *) }
    CURLOT_STRING,  {         (char * to zero terminated buffer) }
    CURLOT_SLIST,   {         (struct curl_slist *) }
    CURLOT_CBPTR,   {         (void * passed as-is to a callback) }
    CURLOT_BLOB,    { blob (struct curl_blob *) }
    CURLOT_FUNCTION { function pointer }
  );

  { The CURLOPTTYPE_* id ranges can still be used to figure out what type/size
    to use for curl_easy_setopt() for the given id }
  pcurl_easyoption = ^curl_easyoption;
  curl_easyoption = record
    name : PChar;
    id : CURLoption;
    easytype : curl_easytype;
    flags : Cardinal;
  end;

const
  CURL_SSLVERSION_MAX_NONE    = 0;
  CURL_SSLVERSION_MAX_DEFAULT = Longint(CURL_SSLVERSION_TLSv1)   shl 16;
  CURL_SSLVERSION_MAX_TLSv1_0 = Longint(CURL_SSLVERSION_TLSv1_0) shl 16;
  CURL_SSLVERSION_MAX_TLSv1_1 = Longint(CURL_SSLVERSION_TLSv1_1) shl 16;
  CURL_SSLVERSION_MAX_TLSv1_2 = Longint(CURL_SSLVERSION_TLSv1_2) shl 16;
  CURL_SSLVERSION_MAX_TLSv1_3 = Longint(CURL_SSLVERSION_TLSv1_3) shl 16;

  { never use, keep last }
  CURL_SSLVERSION_MAX_LAST = Longint(CURL_SSLVERSION_LAST) shl 16;

  { "alias" means it is provided for old programs to remain functional,
     we prefer another name }
  CURLOT_FLAG_ALIAS                                                   = 1 shl 0;

  { curl_strequal() and curl_strnequal() are subject for removal in a future
    release }
  function curl_strequal (const s1 : PChar; const s2 : PChar) : Integer; cdecl;
    external CurlLib;
  function curl_strnequal (const s1 : PChar; const s2 : PChar; n : Cardinal) :
    Integer; cdecl; external CurlLib;

  { NAME curl_mime_init()

   DESCRIPTION
   Create a mime context and return its handle. The easy parameter is the
   target handle. }
  function curl_mime_init (easy : CURL) : pcurl_mime; cdecl; external CurlLib;

  { NAME curl_mime_free()

    DESCRIPTION
    release a mime handle and its substructures. }
  procedure curl_mime_free (mime : pcurl_mime); cdecl; external CurlLib;

  { NAME curl_mime_addpart()

    DESCRIPTION
    Append a new empty part to the given mime context and return a handle to
    the created part. }
  function curl_mime_addpart (mime : pcurl_mime) : pcurl_mimepart; cdecl;
    external CurlLib;

  { NAME curl_mime_name()

    DESCRIPTION
    Set mime/form part name. }
  function curl_mime_name (part : pcurl_mimepart; const name : PChar) :
    CURLcode; cdecl; external CurlLib;

  { NAME curl_mime_filename()

    DESCRIPTION
    Set mime part remote file name. }
  function curl_mime_filename (part : pcurl_mimepart; const filename : PChar) :
    CURLcode; cdecl; external CurlLib;

  { NAME curl_mime_type()

    DESCRIPTION
    Set mime part type. }
  function curl_mime_type (part : pcurl_mimepart; const mimetype : PChar) :
    CURLcode; cdecl; external CurlLib;

  { NAME curl_mime_encoder()

    DESCRIPTION
    Set mime data transfer encoder. }
  function curl_mime_encoder (part : pcurl_mimepart; const encoding : PChar) :
    CURLcode; cdecl; external CurlLib;

  { NAME curl_mime_data()

    DESCRIPTION
    Set mime part data source from memory data, }
  function curl_mime_data (part : pcurl_mimepart; const data : PChar;
    datasize : Cardinal) : CURLcode; cdecl; external CurlLib;

  { NAME curl_mime_filedata()

   DESCRIPTION
   Set mime part data source from named file. }
  function curl_mime_filedata (part : pcurl_mimepart; const filename : PChar) :
    CURLcode; cdecl; external CurlLib;

  { NAME curl_mime_data_cb()

    DESCRIPTION
    Set mime part data source from callback function. }
  function curl_mime_data_cb (part : pcurl_mimepart; datasize : curl_off_t;
    readfunc : curl_read_callback; seekfunc : curl_seek_callback;
    freefunc : curl_free_callback; arg : Pointer) : CURLcode; cdecl;
    external CurlLib;

  { NAME curl_mime_subparts()

    DESCRIPTION
    Set mime part data source from subparts. }
  function curl_mime_subparts (part : pcurl_mimepart; subparts : pcurl_mime) :
    CURLcode; cdecl; external CurlLib;

  { NAME curl_mime_headers()

    DESCRIPTION
    Set mime part headers. }
  function curl_mime_headers (part : pcurl_mimepart; headers : pcurl_slist;
    take_ownership : Integer) : CURLcode; cdecl; external CurlLib;

  { NAME curl_formadd()

    DESCRIPTION
    Pretty advanced function for building multi-part formposts. Each invoke
    adds one part that together construct a full post. Then use
    CURLOPT_HTTPPOST to send it off to libcurl. }
  function curl_formadd (httppost : ppcurl_httppost;
    lastpost : ppcurl_httppost) : CURLFORMcode; cdecl; varargs;
    external CurlLib;

  { NAME curl_formget()

    DESCRIPTION
    Serialize a curl_httppost struct built with curl_formadd().
    Accepts a void pointer as second argument which will be passed to
    the curl_formget_callback function.
    Returns 0 on success. }
  function curl_formget (form : pcurl_httppost; arg : Pointer;
    append : curl_formget_callback) : Integer; cdecl; external CurlLib;

  { NAME curl_formfree()

    DESCRIPTION
    Free a multipart formpost previously built with curl_formadd(). }
  procedure curl_formfree (form : pcurl_httppost); cdecl; external CurlLib;

  { NAME curl_getenv()

    DESCRIPTION
    Returns a malloc()'ed string that MUST be curl_free()ed after usage is
    complete. DEPRECATED - see lib/README.curlx }
  function curl_getenv (const variable : PChar) : PChar; cdecl;
    external CurlLib;

  { NAME curl_version()

    DESCRIPTION
    Returns a static ascii string of the libcurl version. }
  function curl_version : PChar; cdecl; external CurlLib;

  { NAME curl_easy_escape()

    DESCRIPTION
    Escapes URL strings (converts all letters consider illegal in URLs to their
    %XX versions). This function returns a new allocated string or NULL if an
    error occurred. }
  function curl_easy_escape (handle : CURL; const str : PChar;
    length : Integer) : PChar; cdecl; external CurlLib;

  { the previous version: }
  function curl_escape (const std : PChar; length : Integer) : PChar; cdecl;
    external CurlLib;

  { NAME curl_easy_unescape()

    DESCRIPTION
    Unescapes URL encoding in strings (converts all %XX codes to their 8bit
    versions). This function returns a new allocated string or NULL if an error
    occurred.
    Conversion Note: On non-ASCII platforms the ASCII %XX codes are
    converted into the host encoding. }
  function curl_easy_unescape (handle : CURL; const str : PChar;
    length : Integer; outlength : PInteger) : PChar; cdecl; external CurlLib;

  { the previous version  }
  function curl_unescape (const str : PChar; length : Integer) : PChar; cdecl;
    external CurlLib;

  { NAME curl_free()

    DESCRIPTION
    Provided for de-allocation in the same translation unit that did the
    allocation. Added in libcurl 7.10 }
  procedure curl_free (p : Pointer); cdecl; external CurlLib;

  { NAME curl_global_init()

    DESCRIPTION
    curl_global_init() should be invoked exactly once for each application that
    uses libcurl and before any call of other libcurl functions.

    This function is not thread-safe! }
  function curl_global_init (flags : Longint) : CURLcode; cdecl;
    external CurlLib;

  { NAME curl_global_init_mem()

    DESCRIPTION
    curl_global_init() or curl_global_init_mem() should be invoked exactly once
    for each application that uses libcurl.  This function can be used to
    initialize libcurl and set user defined memory management callback
    functions.  Users can implement memory management routines to check for
    memory leaks, check for mis-use of the curl library etc.  User registered
    callback routines with be invoked by this library instead of the system
    memory management routines like malloc, free etc. }
  function curl_global_init_mem (flags : Longint; m : curl_malloc_callback;
    f : curl_free_callback; r : curl_realloc_callback;
    s : curl_strdup_callback; c : curl_calloc_callback) : CURLcode; cdecl;
    external CurlLib;

  { NAME curl_global_cleanup()

    DESCRIPTION
    curl_global_cleanup() should be invoked exactly once for each application
    that uses libcurl }
  procedure curl_global_cleanup; cdecl; external CurlLib;

  { NAME curl_global_sslset()

    DESCRIPTION
    When built with multiple SSL backends, curl_global_sslset() allows to
    choose one. This function can only be called once, and it must be called
    *before* curl_global_init().

    The backend can be identified by the id (e.g. CURLSSLBACKEND_OPENSSL). The
    backend can also be specified via the name parameter (passing -1 as id).
    If both id and name are specified, the name will be ignored. If neither id
    nor name are specified, the function will fail with
    CURLSSLSET_UNKNOWN_BACKEND and set the "avail" pointer to the
    NULL-terminated list of available backends.

    Upon success, the function returns CURLSSLSET_OK.

    If the specified SSL backend is not available, the function returns
    CURLSSLSET_UNKNOWN_BACKEND and sets the "avail" pointer to a NULL-
    terminated list of available SSL backends.

    The SSL backend can be set only once. If it has already been set, a
    subsequent attempt to change it will result in a CURLSSLSET_TOO_LATE. }
  function curl_global_sslset (id : curl_sslbackend; const name : PChar;
    const avail : pppcurl_ssl_backend) : CURLsslset; cdecl; external CurlLib;

  { NAME curl_slist_append()

    DESCRIPTION
    Appends a string to a linked list. If no list exists, it will be created
    first. Returns the new list, after appending. }
  function curl_slist_append (list : pcurl_slist; const str : PChar) :
    pcurl_slist; cdecl; external CurlLib;

  { NAME curl_slist_free_all()

    DESCRIPTION
    free a previously built curl_slist. }
  procedure curl_slist_free_all (list : pcurl_slist); cdecl; external CurlLib;

  { NAME curl_getdate()

    DESCRIPTION
    Returns the time, in seconds since 1 Jan 1970 of the time string given in
    the first argument. The time argument in the second parameter is unused
    and should be set to NULL. }
  function curl_getdate (const p : PChar; const unused : PInt64) : Int64;
    cdecl; external CurlLib;

  function curl_share_init : CURLSH; cdecl; external CurlLib;
  function curl_share_setopt (handle : CURLSH; option : CURLSHoption) :
    CURLSHcode; cdecl; varargs; external CurlLib;
  function curl_share_cleanup (handle : CURLSH) : CURLSHcode; cdecl;
    external CurlLib;

  { NAME curl_version_info()

    DESCRIPTION
    This function returns a pointer to a static copy of the version info
    struct. See above. }
  function curl_version_info (ver : CURLversion) : pcurl_version_info_data;
    cdecl; external CurlLib;

  { NAME curl_easy_strerror()

    DESCRIPTION
    The curl_easy_strerror function may be used to turn a CURLcode value
    into the equivalent human readable error string.  This is useful
    for printing meaningful error messages. }
  function curl_easy_strerror (code : CURLcode) : PChar; cdecl;
    external CurlLib;

  { NAME curl_share_strerror()

    DESCRIPTION
    The curl_share_strerror function may be used to turn a CURLSHcode value
    into the equivalent human readable error string.  This is useful
    for printing meaningful error messages. }
  function curl_share_strerror (code : CURLSHcode) : PChar; cdecl;
    external CurlLib;

  { NAME curl_easy_pause()

    DESCRIPTION
    The curl_easy_pause function pauses or unpauses transfers. Select the new
    state by setting the bitmask, use the convenience defines below. }
  function curl_easy_pause (handle : CURL; bitmask : Integer) : CURLcode;
    cdecl; external CurlLib;

  function curl_easy_init : CURL; cdecl; external CurlLib;
  function curl_easy_setopt (handle : CURL; option : CURLoption) : CURLcode;
    cdecl; varargs; external CurlLib;
  function curl_easy_perform (handle : CURL) : CURLcode; cdecl;
    external CurlLib;
  procedure curl_easy_cleanup (handle : CURL); cdecl; external CurlLib;

  { NAME curl_easy_getinfo()

    DESCRIPTION
    Request internal information from the curl session with this function.  The
    third argument MUST be a pointer to a long, a pointer to a char * or a
    pointer to a double (as the documentation describes elsewhere).  The data
    pointed to will be filled in accordingly and can be relied upon only if the
    function returns CURLE_OK.  This function is intended to get used *AFTER* a
    performed transfer, all results from this function are undefined until the
    transfer is completed. }
  function curl_easy_getinfo (handle : CURL; info : CURLINFO) : CURLcode;
    cdecl; varargs; external CurlLib;

  { NAME curl_easy_duphandle()

    DESCRIPTION
    Creates a new curl session handle with the same options set for the handle
    passed in. Duplicating a handle could only be a matter of cloning data and
    options, internal state info and things like persistent connections cannot
    be transferred. It is useful in multithreaded applications when you can run
    curl_easy_duphandle() for each new thread to avoid a series of identical
    curl_easy_setopt() invokes in every thread. }
  function curl_easy_duphandle (handle : CURL) : CURL; cdecl;
    external CurlLib;

  { NAME curl_easy_reset()

    DESCRIPTION
    Re-initializes a CURL handle to the default values. This puts back the
    handle to the same state as it was in when it was just created.

    It does keep: live connections, the Session ID cache, the DNS cache and the
    cookies. }
  procedure curl_easy_reset (handle : CURL); cdecl; external CurlLib;

  { NAME curl_easy_recv()

    DESCRIPTION
    Receives data from the connected socket. Use after successful
    curl_easy_perform() with CURLOPT_CONNECT_ONLY option. }
  function curl_easy_recv (handle : CURL; buffer : Pointer; buflen : Cardinal;
    n : PCardinal) : CURLcode; cdecl; external CurlLib;

  { NAME curl_easy_send()

    DESCRIPTION
    Sends data over the connected socket. Use after successful
    curl_easy_perform() with CURLOPT_CONNECT_ONLY option. }
  function curl_easy_send (handle : CURL; const buffer : Pointer;
    buflen : Cardinal; n : PCardinal) : CURLcode; cdecl; external CurlLib;

  { NAME curl_easy_upkeep()

    DESCRIPTION
    Performs connection upkeep for the given session handle. }
  function curl_aesy_upkeep (handle : CURL) : CURLcode; cdecl; 
    external CurlLib;

  { Name:    curl_multi_init()

    Desc:    inititalize multi-style curl usage
    Returns: a new CURLM handle to use in all 'curl_multi' functions. }
  function curl_multi_init : CURLM; cdecl; external CurlLib;

  { Name:    curl_multi_add_handle()

    Desc:    add a standard curl handle to the multi stack
    Returns: CURLMcode type, general multi error code. }
  function curl_multi_add_handle (multi_handle : CURLM; curl_handle : CURL) :
    CURLMcode; cdecl; external CurlLib;

  { Name:    curl_multi_remove_handle()

    Desc:    removes a curl handle from the multi stack again
    Returns: CURLMcode type, general multi error code. }
  function curl_multi_remove_handle (multi_handle : CURLM; curl_handle : CURL) :
    CURLMcode; cdecl; external CurlLib;

  { Name:    curl_multi_fdset()

    Desc:    Ask curl for its fd_set sets. The app can use these to select() or
             poll() on. We want curl_multi_perform() called as soon as one of
             them are ready.
    Returns: CURLMcode type, general multi error code. }
  function curl_multi_fdset (multi_handle : CURLM; read_fd_set : pFDSet;
    write_fd_set : pFDSet; exc_fd_set : pFDSet; max_fd : PInteger) :
    CURLMcode; cdecl; external CurlLib;

  { Name:     curl_multi_wait()

    Desc:     Poll on all fds within a CURLM set as well as any
              additional fds passed to the function.
    Returns:  CURLMcode type, general multi error code. }
  function curl_multi_wait (multi_handle : CURLM;
    extra_fds : array of curl_waitfd; extra_nfds : Cardinal;
    timeout_ms : Integer; ret : PInteger) : CURLMcode; cdecl;
    external CurlLib;

  { Name:     curl_multi_poll()
  
    Desc:     Poll on all fds within a CURLM set as well as any
              additional fds passed to the function.
 
    Returns:  CURLMcode type, general multi error code. }
  function curl_multi_poll (multi_handle : CURLM; extra_fds : 
    array of curl_waitfd; extra_nfds : Cardinal; timeout_ms : Integer; ret :
    PInteger) : CURLMcode; cdecl; external CurlLib; 

  { Name:     curl_multi_wakeup()
 
    Desc:     wakes up a sleeping curl_multi_poll call.
 
    Returns:  CURLMcode type, general multi error code. }
  function curl_multi_wakeup (multi_handle : CURLM) : CURLMcode; cdecl;
    external CurlLib;

  { Name:    curl_multi_perform()

    Desc:    When the app thinks there's data available for curl it calls this
             function to read/write whatever there is right now. This returns
             as soon as the reads and writes are done. This function does not
             require that there actually is data available for reading or that
             data can be written, it can be called just in case. It returns
             the number of handles that still transfer data in the second
             argument's integer-pointer.

    Returns: CURLMcode type, general multi error code. *NOTE* that this only
             returns errors etc regarding the whole multi stack. There might
             still have occurred problems on invidual transfers even when this
             returns OK. }
  function curl_multi_perform (multi_handle : CURLM; running_handles :
    PInteger) : CURLMcode; cdecl; external CurlLib;

  { Name:    curl_multi_cleanup()

    Desc:    Cleans up and removes a whole multi stack. It does not free or
             touch any individual easy handles in any way. We need to define
             in what state those handles will be if this function is called
             in the middle of a transfer.

    Returns: CURLMcode type, general multi error code. }
  function curl_multi_cleanup (multi_handle : CURLM) : CURLMcode; cdecl;
    external CurlLib;

  { Name:    curl_multi_info_read()

    Desc:    Ask the multi handle if there's any messages/informationals from
             the individual transfers. Messages include informationals such as
             error code from the transfer or just the fact that a transfer is
             completed. More details on these should be written down as well.

             Repeated calls to this function will return a new struct each
             time, until a special "end of msgs" struct is returned as a signal
             that there is no more to get at this point.

             The data the returned pointer points to will not survive calling
             curl_multi_cleanup().

             The 'CURLMsg' struct is meant to be very simple and only contain
             very basic information. If more involved information is wanted,
             we will provide the particular "transfer handle" in that struct
             and that should/could/would be used in subsequent
             curl_easy_getinfo() calls (or similar). The point being that we
             must never expose complex structs to applications, as then we'll
             undoubtably get backwards compatibility problems in the future.

    Returns: A pointer to a filled-in struct, or NULL if it failed or ran out
             of structs. It also writes the number of messages left in the
             queue (after this read) in the integer the second argument points
             to. }
  function curl_multi_info_read (multi_handle : CURLM;
    mdgs_in_queue : PInteger) : CURLMsg_rec; cdecl; external CurlLib;


  { Name:    curl_multi_strerror()

    Desc:    The curl_multi_strerror function may be used to turn a CURLMcode
             value into the equivalent human readable error string.  This is
             useful for printing meaningful error messages.

    Returns: A pointer to a zero-terminated error message. }
  function curl_multi_strerror (code : CURLMcode) : PChar; cdecl;
    external CurlLib;

  { Name:    curl_multi_socket() and
             curl_multi_socket_all()

    Desc:    An alternative version of curl_multi_perform() that allows the
             application to pass in one of the file descriptors that have been
             detected to have "action" on them and let libcurl perform.
             See man page for details. }
  function curl_multi_socket (multi_handle : CURLM; s : curl_socket_t;
    running_handles : PInteger) : CURLMcode; cdecl; external CurlLib;

  function curl_multi_socket_action (multi_handle : CURLM; s : curl_socket_t;
    ev_bitmask : Integer; running_handles : PInteger) : CURLMcode; cdecl;
    external CurlLib;

  function curl_multi_socket_all (multi_handle : CURLM;
    running_handles : PInteger) : CURLMcode; cdecl; external CurlLib;

  { Name:    curl_multi_timeout()

    Desc:    Returns the maximum number of milliseconds the app is allowed to
             wait before curl_multi_socket() or curl_multi_perform() must be
             called (to allow libcurl's timed events to take place).

    Returns: CURLM error code. }
  function curl_multi_timeout (multi_handle : CURLM; miliseconds : PLongint) :
    CURLMcode; cdecl; external CurlLib;

  { Name:    curl_multi_setopt()

    Desc:    Sets options for the multi handle.

    Returns: CURLM error code. }
  function curl_multi_setopt (multi_handle : CURLM; option : CURLMoption) :
    CURLMcode; cdecl; varargs; external CurlLib;

  { Name:    curl_multi_assign()

    Desc:    This function sets an association in the multi handle between the
             given socket and a private pointer of the application. This is
             (only) useful for curl_multi_socket uses.

    Returns: CURLM error code. }
  function curl_multi_assign (multi_handle : CURLM; sockfd : curl_socket_t;
    sockp : Pointer) : CURLMcode; cdecl; external CurlLib;

  { Name: curl_push_callback

    Desc: This callback gets called when a new stream is being pushed by the
          server. It approves or denies the new stream.

    Returns: CURL_PUSH_OK or CURL_PUSH_DENY. }
  function curl_pushheader_bynum (h : pcurl_pushheaders; num : Cardinal) :
    PChar; cdecl; external CurlLib;

  function curl_pushheader_byname (h : pcurl_pushheaders; const name : PChar) :
    PChar; cdecl; external CurlLib;

  { curl_url() creates a new CURLU handle and returns a pointer to it.
    Must be freed with curl_url_cleanup(). }
  function curl_url : PCURLU; cdecl; external CurlLib;

  { curl_url_cleanup() frees the CURLU handle and related resources used for
    the URL parsing. It will not free strings previously returned with the URL
    API. }
  procedure curl_url_cleanup (handle : PCURLU); cdecl; external CurlLib;

  { curl_url_dup() duplicates a CURLU handle and returns a new copy. The new
    handle must also be freed with curl_url_cleanup(). }
  function curl_url_dup (in_handle : PCURLU) : PCURLU; cdecl; external CurlLib;

  { curl_url_get() extracts a specific part of the URL from a CURLU
    handle. Returns error code. The returned pointer MUST be freed with
    curl_free() afterwards. }
  function curl_url_get (handle : PCURLU; what : CURLUPart; part : PPChar;
    flags : Cardinal) : CURLUcode; cdecl; external CurlLib;

  { curl_url_set() sets a specific part of the URL in a CURLU handle. Returns
    error code. The passed in string will be copied. Passing a NULL instead of
    a part string, clears that part. }
  function curl_url_set (handle : PCURLU; what : CURLUPart; const part : PChar;
    flags : Cardinal) : CURLUcode; cdecl; external CurlLib;

  function curl_easy_option_by_name (const name : PChar) : pcurl_easyoption;
    cdecl; external CurlLib;
  function curl_easy_option_by_id (id : CURLoption) : pcurl_easyoption; cdecl;
    external CurlLib;
  function curl_easy_option_next (const prev : pcurl_easyoption) : 
    pcurl_easyoption; cdecl; external CurlLib;

implementation

end.


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

unit curl.utils.errors_stack;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, utils.errorsstack;

type
  { Collect CURL errors. }
  PErrorsStack = ^TErrorsStack;
  TErrorsStack = class({$IFDEF FPC}specialize{$ENDIF} TListErrorsStack<String>)
  private
    FErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
  
    { Error buffer pointer }
    function GetErrorBuffer : PChar;
  public
    { Add error to stack }
    procedure Push (AError : CURLcode); overload;
  
    { Get internal additional error buffer }
    property ErrorBuffer : PChar read GetErrorBuffer;
  end;

const
  ErrorsMessages : array [CURLE_OK .. CURL_LAST] of String = (
    { CURLE_OK }
    '',

    { CURLE_UNSUPPORTED_PROTOCOL }
    'The URL you passed to libcurl used a protocol that this libcurl does not '+
    'support.',

    { CURLE_FAILED_INIT }
    'Very early initialization code failed.',

    { CURLE_URL_MALFORMAT }
    'The URL was not properly formatted.',

    { CURLE_NOT_BUILT_IN }
    'A requested feature, protocol or option was not found built-in in this '  +
    'libcurl due to a build-time decision.',

    { CURLE_COULDNT_RESOLVE_PROXY }
    'Couldn''t resolve proxy. The given proxy host could not be resolved.',

    { CURLE_COULDNT_RESOLVE_HOST }
    'Couldn''t resolve host. The given remote host was not resolved.',

    { CURLE_COULDNT_CONNECT }
    'Failed to connect to host or proxy.',

    { CURLE_WEIRD_SERVER_REPLY }
    'The server sent data libcurl couldn''t parse.',

    { CURLE_REMOTE_ACCESS_DENIED }
    'We were denied access to the resource given in the URL.',

    { CURLE_FTP_ACCEPT_FAILED }
    'While waiting for the server to connect back when an active FTP session ' +
    'is used, an error code was sent over the control connection or similar. ',

    { CURLE_FTP_WEIRD_PASS_REPLY }
    'After having sent the FTP password to the server, libcurl expects a '     +
    'proper reply.',

    { CURLE_FTP_ACCEPT_TIMEOUT }
    'During an active FTP session while waiting for the server to connect, '   +
    'the timeout expired.',

    { CURLE_FTP_WEIRD_PASV_REPLY }
    'libcurl failed to get a sensible result back from the server as a '       +
    'response to either a PASV or a EPSV command. The server is flawed.',

    { CURLE_FTP_WEIRD_227_FORMAT }
    'FTP servers return a 227-line as a response to a PASV command.',

    { CURLE_FTP_CANT_GET_HOST }
    'An internal failure to lookup the host used for the new connection.',

    { CURLE_HTTP2 }
    'A problem was detected in the HTTP2 framing layer.',

    { CURLE_FTP_COULDNT_SET_TYPE }
    'Received an error when trying to set the transfer mode to binary or ASCII.',

    { CURLE_PARTIAL_FILE }
    'A file transfer was shorter or larger than expected.',

    { CURLE_FTP_COULDNT_RETR_FILE }
    'This was either a weird reply to a ''RETR'' command or a zero byte '      +
    'transfer complete.',

    { CURLE_OBSOLETE20 }
    'These error codes will never be returned.',

    { CURLE_QUOTE_ERROR }
    'When sending custom "QUOTE" commands to the remote server, one of the '   +
    'commands returned an error code that was 400 or higher (for FTP) or '     +
    'otherwise indicated unsuccessful completion of the command.',

    { CURLE_HTTP_RETURNED_ERROR }
    'This is returned if the HTTP server returns an error code that is >= 400.',

    { CURLE_WRITE_ERROR }
    'An error occurred when writing received data error.',

    { CURLE_OBSOLETE24 }
    'These error codes will never be returned.',

    { CURLE_UPLOAD_FAILED }
    'Failed starting the upload.',

    { CURLE_READ_ERROR }
    'There was a problem reading.',

    { CURLE_OUT_OF_MEMORY }
    'A memory allocation request failed.',

    { CURLE_OPERATION_TIMEDOUT }
    'Operation timeout.',

    { CURLE_OBSOLETE29 }
    'These error codes will never be returned.',

    { CURLE_FTP_PORT_FAILED }
    'The FTP PORT command returned error.',

    { CURLE_FTP_COULDNT_USE_REST }
    'The FTP REST command returned error.',

    { CURLE_OBSOLETE32 }
    'These error codes will never be returned.',

    { CURLE_RANGE_ERROR }
    'The server does not support or accept range requests.',

    { CURLE_HTTP_POST_ERROR }
    'Mainly occurs due to internal confusion.',

    { CURLE_SSL_CONNECT_ERROR }
    'A problem occurred somewhere in the SSL/TLS handshake.',

    { CURLE_BAD_DOWNLOAD_RESUME }
    'The download could not be resumed because the specified offset was out of'+
    ' the file boundary.',

    { CURLE_FILE_COULDNT_READ_FILE }
    'A file given with FILE:// couldn''t be opened.',

    { CURLE_LDAP_CANNOT_BIND }
    'LDAP cannot bind. LDAP bind operation failed.',

    { CURLE_LDAP_SEARCH_FAILED }
    'LDAP search failed.',

    { CURLE_OBSOLETE40 }
    'These error codes will never be returned.',

    { CURLE_FUNCTION_NOT_FOUND }
    'Function not found.',

    { CURLE_ABORTED_BY_CALLBACK }
    'Aborted by callback.',

    { CURLE_BAD_FUNCTION_ARGUMENT }
    'A function was called with a bad parameter.',

    { CURLE_OBSOLETE44 }
    'These error codes will never be returned.',

    { CURLE_INTERFACE_FAILED }
    'Interface error. A specified outgoing interface could not be used.',

    { CURLE_OBSOLETE46 }
    'These error codes will never be returned.',

    { CURLE_TOO_MANY_REDIRECTS }
    'Too many redirects.',

    { CURLE_UNKNOWN_OPTION }
    'An option passed to libcurl is not recognized/known.',

    { CURLE_TELNET_OPTION_SYNTAX }
    'A telnet option string was Illegally formatted.',

    { CURLE_OBSOLETE50 }
    'These error codes will never be returned.',

    { CURLE_OBSOLETE51 }
    'These error codes will never be returned.',

    { CURLE_GOT_NOTHING }
    'Nothing was returned from the server, and under the circumstances, '      +
    'getting nothing is considered an error.',

    { CURLE_SSL_ENGINE_NOTFOUND }
    'The specified crypto engine wasn''t found.',

    { CURLE_SSL_ENGINE_SETFAILED }
    'Failed setting the selected SSL crypto engine as default!',

    { CURLE_SEND_ERROR }
    'Failed sending network data.',

    { CURLE_RECV_ERROR }
    'Failure with receiving network data.',

    { CURLE_OBSOLETE57 }
    'These error codes will never be returned.',

    { CURLE_SSL_CERTPROBLEM }
    'Problem with the local client certificate.',

    { CURLE_SSL_CIPHER }
    'Couldn''t use specified cipher.',

    { CURLE_SSL_CACERT }
    { CURLE_PEER_FAILED_VERIFICATION }
    'The remote server''s SSL certificate or SSH md5 fingerprint was deemed '  +
    'not OK.',

    { CURLE_BAD_CONTENT_ENCODING }
    'Unrecognized transfer encoding.',

    { CURLE_LDAP_INVALID_URL }
    'Invalid LDAP URL.',

    { CURLE_FILESIZE_EXCEEDED }
    'Maximum file size exceeded.',

    { CURLE_USE_SSL_FAILED }
    'Requested FTP SSL level failed.',

    { CURLE_SEND_FAIL_REWIND }
    'When doing a send operation curl had to rewind the data to retransmit, '  +
    'but the rewinding operation failed.',

    { CURLE_SSL_ENGINE_INITFAILED }
    'Initiating the SSL Engine failed.',

    { CURLE_LOGIN_DENIED }
    'The remote server denied curl to login.',

    { CURLE_TFTP_NOTFOUND }
    'File not found on TFTP server.',

    { CURLE_TFTP_PERM }
    'Permission problem on TFTP server.',

    { CURLE_REMOTE_DISK_FULL }
    'Out of disk space on the server.',

    { CURLE_TFTP_ILLEGAL }
    'Illegal TFTP operation.',

    { CURLE_TFTP_UNKNOWNID }
    'Unknown TFTP transfer ID.',

    { CURLE_REMOTE_FILE_EXISTS }
    'File already exists and will not be overwritten.',

    { CURLE_TFTP_NOSUCHUSER }
    'This error should never be returned by a properly functioning TFTP server.',

    { CURLE_CONV_FAILED }
    'Character conversion failed.',

    { CURLE_CONV_REQD }
    'Caller must register conversion callbacks.',

    { CURLE_SSL_CACERT_BADFILE }
    'Problem with reading the SSL CA cert.',

    { CURLE_REMOTE_FILE_NOT_FOUND }
    'The resource referenced in the URL does not exist.',

    { CURLE_SSH }
    'An unspecified error occurred during the SSH session.',

    { CURLE_SSL_SHUTDOWN_FAILED }
    'Failed to shut down the SSL connection.',

    { CURLE_AGAIN }
    'Socket is not ready for send/recv wait till it''s ready and try again.',

    { CURLE_SSL_CRL_BADFILE }
    'Failed to load CRL file.',

    { CURLE_SSL_ISSUER_ERROR }
    'Issuer check failed.',

    { CURLE_FTP_PRET_FAILED }
    'The FTP server does not understand the PRET command at all or does not '  +
    'support the given argument.',

    { CURLE_RTSP_CSEQ_ERROR }
    'Mismatch of RTSP CSeq numbers.',

    { CURLE_RTSP_SESSION_ERROR }
    'Mismatch of RTSP Session Identifiers.',

    { CURLE_FTP_BAD_FILE_LIST }
    'Unable to parse FTP file list (during FTP wildcard downloading).',

    { CURLE_CHUNK_FAILED }
    'Chunk callback reported error.',

    { CURLE_NO_CONNECTION_AVAILABLE }
    'No connection available, the session will be queued.',

    { CURLE_SSL_PINNEDPUBKEYNOTMATCH }
    'Failed to match the pinned key.',

    { CURLE_SSL_INVALIDCERTSTATUS }
    'Status returned failure.',

    { CURLE_HTTP2_STREAM }
    'Stream error in the HTTP/2 framing layer.',

    { CURLE_RECURSIVE_API_CALL }
    'An API function was called from inside a callback.',

    { CURLE_AUTH_ERROR }
    'An authentication function returned an error.',

    { CURLE_HTTP3 }
    'A problem was detected in the HTTP/3 layer.',

    { CURLE_QUIC_CONNECT_ERROR }
    'QUIC connection error. This error may be caused by an SSL library error. '+
    'QUIC is the protocol used for HTTP/3 transfers.',

    { CURLE_PROXY }
    'Proxy handshake error.',

    { CURL_LAST }
    ''
  );

implementation

{ TErrorsStack }

function TErrorsStack.GetErrorBuffer : PChar;
begin
  Result := @FErrorBuffer[0];
end;

procedure TErrorsStack.Push(AError: CURLcode);
begin
  if AError <> CURLE_OK then
  begin
    Push(ErrorsMessages[AError]);
  end;

  if FErrorBuffer[0] <> #0 then
  begin
    Push(FErrorBuffer);
    FillChar(FErrorBuffer, CURL_ERROR_SIZE, #0);
  end;
end;

end.


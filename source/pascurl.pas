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
(*                  assign  request  params.  And TResponse  class to getting *)
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

  THTTPStatusCode = (
    HTTP_CONTINUE                                        = 100,
    HTTP_SWITCHING_PROTOCOL                              = 101,
    HTTP_CHECKPOINT                                      = 103, { UNOFFICIAL CODE }

    HTTP_OK                                              = 200,
    HTTP_CREATED                                         = 201,
    HTTP_ACCEPTED                                        = 202,
    HTTP_NON_AUTHORITATIVE_INFORMATION                   = 203,
    HTTP_NO_CONTENT                                      = 204,
    HTTP_RESET_CONTENT                                   = 205,
    HTTP_PARTIAL_CONTENT                                 = 206,
    HTTP_MULTI_STATUS                                    = 207,
    HTTP_ALREADY_REPORTED                                = 208,
    HTTP_THIS_IS_FINE__APACHE_WEB_SERVER                 = 218, { UNOFFICIAL CODE }
    HTTP_IM_USED                                         = 226,

    HTTP_MULTIPLE_CHOICES                                = 300,
    HTTP_MOVED_PERMANENTLY                               = 301,
    HTTP_FOUND                                           = 302,
    HTTP_SEE_OTHER                                       = 303,
    HTTP_NOT_MODIFIED                                    = 304,
    HTTP_USE_PROXY                                       = 305,
    HTTP_TEMPORARY_REDIRECT                              = 307,
    HTTP_PERMANENT_REDIRECT                              = 308,

    HTTP_BAD_REQUEST                                     = 400,
    HTTP_UNAUTHORIZED                                    = 401,
    HTTP_FORBIDDEN                                       = 403,
    HTTP_NOT_FOUND                                       = 404,
    HTTP_METHOD_NOT_ALLOWED                              = 405,
    HTTP_NOT_ACCEPTABLE                                  = 406,
    HTTP_PROXY_AUTHENTIFICATION_REQUIRED                 = 407,
    HTTP_REQUEST_TIMEOUT                                 = 408,
    HTTP_CONFLICT                                        = 409,
    HTTP_GONE                                            = 410,
    HTTP_LENGTH_REQUIRED                                 = 411,
    HTTP_PRECONDITION_FAILED                             = 412,
    HTTP_REQUEST_ENTITY_TOO_LARGE                        = 413,
    HTTP_REQUEST_URL_TOO_LONG                            = 414,
    HTTP_UNSUPPORTED_MEDIA_TYPE                          = 415,
    HTTP_REQUESTED_RANGE_NOT_SATISFIABLE                 = 416,
    HTTP_EXPECTATION_FAILED                              = 417,
    HTTP_IM_A_TEAPOT                                     = 418,
    HTTP_PAGE_EXPIRED__LARAVEL_FRAMEWORK                 = 419, { UNOFFICIAL CODE }
    HTTP_METHOD_FAILURE__SPRING_FRAMEWORK                = 420, { UNOFFICIAL CODE }
    HTTP_MISDIRECTED_REQUEST                             = 421,
    HTTP_UNPROCESSABLE_ENTITY                            = 422,
    HTTP_LOCKED                                          = 423,
    HTTP_FAILED_DEPENDENCY                               = 424,
    HTTP_TOO_EARLY                                       = 425,
    HTTP_UPGRADE_REQUIRED                                = 426,
    HTTP_PRECONDITION_REQUIRED                           = 428,
    HTTP_TOO_MANY_REQUESTS                               = 429,
    HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE__SHOPIFY        = 430, { UNOFFICIAL CODE }
    HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE                 = 431,
    HTTP_BLOCKED_BY_WINDOWS_PARENTAL_CONTROLS__MICROSOFT = 450, { UNOFFICIAL CODE }
    HTTP_UNAVAILABLE_FOR_LEGAL_REASONS                   = 451,
    HTTP_INVALID_TOKEN__ESRI                             = 498, { UNOFFICIAL CODE }
    HTTP_TOKEN_REQUIRED__ESRI                            = 499, { UNOFFICIAL CODE }

    HTTP_INTERNAL_SERVER_ERROR                           = 500,
    HTTP_NOT_IMPLEMENTED                                 = 501,
    HTTP_BAD_GETEWAY                                     = 502,
    HTTP_SERVICE_UNAVAIBLE                               = 503,
    HTTP_GATEWAY_TIMEOUT                                 = 504,
    HTTP_VERSION_NOT_SUPPORTED                           = 505,
    HTTP_VARIANT_ALSO_NEGOTIATES                         = 506,
    HTTP_INSUFFICIENT_STORAGE                            = 507,
    HTTP_LOOP_DETECTED                                   = 508,
    HTTP_BANDWIDTH_LIMIT_EXCEEDED__APACHE_WEB_SERVER     = 509, { UNOFFICIAL CODE }
    HTTP_NOT_EXTENDED                                    = 510,
    HTTP_NETWORK_AUTHENTICATION_REQUIRED                 = 511,
    HTTP_INVALID_SSL_CERTIFICATE__CLOUDFLARE             = 526, { UNOFFICIAL CODE }
    HTTP_SITE_IS_FROZEN__PATHEON                         = 530, { UNOFFICIAL CODE }
    HTTP_NETWORK_READ_TIMEOUT_ERROR                      = 598  { UNOFFICIAL CODE }
  );

  HTTPVersionCode = (
    HTTP_VERSION_UNKNOWN              = 0,
    HTTP_VERSION_1_0                  = Longint(CURL_HTTP_VERSION_1_0),
    HTTP_VERSION_1_1                  = Longint(CURL_HTTP_VERSION_1_1),
    HTTP_VERSION_2_0                  = Longint(CURL_HTTP_VERSION_2_0),
    HTTP_VERSION_3_0                  = Longint(CURL_HTTP_VERSION_3)
  );

  TFTPStatusCode = (
    (**
     * 1xx Positive Preliminary reply
     *
     * The requested action is being initiated; expect another reply before
     * proceeding with a new command. (The user-process sending another command
     * before the completion reply would be in violation of protocol; but
     * server-FTP processes should queue any commands that arrive while a
     * preceding command is in progress.) This type of reply can be used to
     * indicate that the command was accepted and the user-process may now pay
     * attention to the data connections, for implementations where simultaneous
     * monitoring is difficult. The server-FTP process may send at most, one 1xx
     * reply per command.
     *)

     (**
      * The requested action is being initiated, expect another reply before
      * proceeding with a new command.
      *)
    FTP_REQUEST_INIT_WAIT_NEXT_RESPONSE                  = 100,

    (**
     * Restart marker replay. In this case, the text is exact and not left to
     * the particular implementation; it must read: MARK yyyy = mmmm where yyyy
     * is User-process data stream marker, and mmmm server's equivalent marker
     * (note the spaces between markers and "=").
     *)
    FTP_RESTART_MARKER_REPLY                             = 110,

    (**
     * Service ready in nnn minutes.
     *
     * This indicates that the server accepted and has begun the processing of
     * the command. The client should wait until another response is received
     * before proceeding.
     *)
    FTP_BEGUN_PROCESSING_WAIT_NEXT_RESPONSE              = 120,

    (**
     * Data connection already open; transfer starting.
     *
     * The server may use this code in response to a command initiating a file
     * transfer if the data connection is already established. After sending or
     * receiving this response, the server or client may begin sending data over
     * the data connection.
     *)
     FTP_DATA_CONNECTION_ALREADY_OPENED                  = 125,

    (**
     * File status okay; about to open data connection.
     *
     * The server may use this reply code in response to a command initiating a
     * file transfer before establishing the data connection over which the file
     * transfer will occur. A PASV or PORT command should have been issued prior
     * to the command receiving the 150 response code. After sending or
     * receiving this response, the server/client may begin sending data over
     * the data connection.
     *)
    FTP_INIT_FILE_TRANSFER                               = 150,

    (**
     * 2xx Positive Completion reply
     *
     * The requested action has been successfully completed. A new request may
     * be initiated.
     *)

    (**
     * The requested action has been successfully completed.
     *
     * A server issues a 200 response code if a command is accepted and
     * successfully processed.
     *)
    FTP_SUCCESS_COMPLETE                                 = 200,

    (**
     * Command not implemented, superfluous at this site.
     *
     * A 202 response is meant to indicate that the command is recognized but
     * not implemented by the server because it has no use or meaning to the
     * server. It is considered a successful reply because the client can
     * continue its FTP transaction as if the command was successfully completed
     * on the server.
     *)
    FTP_COMMAND_NOT_IMPLEMENTED                          = 202,

    (**
     * System status, or system help reply.
     *
     * A 211 code is given in response to commands asking for status or help
     * from the server. The information contained along with the response is
     * intended for user consumption and rarely has a meaning to the client
     * process itself.
     *)
    FTP_SYSTEM_STATUS                                    = 211,

    (**
     * Directory status.
     *
     * A 212 code is given in response to a command asking for directory status
     * information. A STAT command that includes a path parameter is one command
     * where a 212 response would be expected.
     *)
    FTP_DIRECTORY_STATUS_INFORMATION                     = 212,

    (**
     * File status.
     *
     * A 213 code may be given in response to a command asking for status
     * information on a file transfer. A STAT command issued during a file
     * transfer is one command where a 213 response code would be expected.
     *)
    FTP_FILE_TRANSFER_STATUS_INFORMATION                 = 213,

    (**
     * Help message. Explains how to use the server or the meaning of a
     * particular non-standard command. This reply is useful only to the human
     * user.
     *
     * A 214 code is used in response to the HELP command to provide information
     * on how to use the server or the meaning of a particular non-standard
     * command. This response is useful only to the human user and commonly
     * contains a list of commands recognized by the server.
     *)
    FTP_HELP_INFORMATION                                 = 214,

    (**
     * NAME system type. Where NAME is an official system name from the registry
     * kept by IANA.
     *
     * A 215 code is given in response to the SYST command. The response should
     * contain an official system name from the list in the Assigned Numbers
     *)
    FTP_SYSTEM_NAME                                      = 215,

    (**
     * Service ready for new user.
     *
     * A 220 code is sent in response to a new user connecting to the FTP server
     * to indicate that the server is ready for the new client. It can also be
     * sent in response to a REIN command, which is meant to reset the
     * connection to the moment the client first connected to the server.
     *)
    FTP_SERVER_READY                                     = 220,

    (**
     * Service closing control connection.
     *
     * A 221 code is sent over the control connection in response to the
     * client's QUIT command. It is sent immediately before the control
     * connection is closed by the server.
     *)
    FTP_DATA_CONNECTION_CLOSED                           = 221,

    (**
     * Data connection open; no transfer in progress.
     *
     * A 225 code is sent in response to the ABOR command when the data
     * connection is still open but there is no file transfer in progress to
     * abort.
     *)
    FTP_DATA_CONNECTION_OPEN_NO_TRANSFER                 = 225,

    (**
     * Closing data connection. Requested file action successful (for example,
     * file transfer or file abort).
     *
     * A 226 reply code is sent by the server before closing the data connection
     * after successfully processing the previous client command affecting the
     * data connection. In most cases, it signals the completion of a file
     * transfer. It can also be sent in response to the ABOR command, which
     * signals that the current file transfer was successfully terminated.
     *)
    FTP_DATA_CONNECTION_CLOSE                            = 226,

    (**
     * Entering Passive Mode (h1,h2,h3,h4,p1,p2).
     *
     * A 227 code is the response given by the server to the PASV command. It
     * indicates that the server is ready for the client to connect to it for
     * the purpose of establishing a data connection. The format of this
     * response is important because the client software must be capable of
     * parsing out the connection information it contains. The values h1 to h4
     * are the IP addresses that the server is listening on.
     *)
    FTP_ENTERING_PASSIVE_MODE                            = 227,

    (**
     * Entering Long Passive Mode (long address, port).
     *)
    FTP_ENTERING_LONG_PASSIVE_MODE                       = 228,

    (**
     * Entering Extended Passive Mode (|||port|).
     *)
    FTP_ENTERING_EXTENDED_PASSIVE_MODE                   = 229,

    (**
     * User logged in, proceed. Logged out if appropriate.
     *
     * The server sends a 230 code in response to a command that has provided
     * sufficient credentials to the server to grant the user access to the FTP
     * server.
     *)
    FTP_USER_LOGGEDIN                                    = 230,

    (**
     * User logged out; service terminated.
     *)
    FTP_USER_LOGGEDOUT                                   = 231,

    (**
     * Logout command noted, will complete when transfer done.
     *
     * A 232 code may be sent in response to a USER command if the server is
     * willing to allow the user access based on previously exchanged security
     * data.
     *)
    FTP_LOGOUT_NOTED                                     = 232,

    (**
     * Specifies that the server accepts the authentication mechanism specified
     * by the client, and the exchange of security data is complete. A higher
     * level nonstandard code created by Microsoft.
     *
     * A 234 code is sent in response to the AUTH command when the requested
     * security mechanism is accepted and negotiation of the secured connection
     * can begin.
     *)
    FTP_SECURITY_ACCEPTED_NEGOTIATION_CONNECTION_BEGIN   = 234,

    (**
     * Requested file action okay, completed.
     *
     * A 250 code is sent in response to the successful completion of a file
     * related command or user working directory related command.
     *)
    FTP_RESPONSE_SUCCESSFUL_COMPLETION                   = 250,

    (**
     * "PATHNAME" created.
     *
     * A 257 code is used as a successful response to the creation of a new
     * directory from the MKD command or in response to a PWD command.
     *)
    FTP_DIRECTORY_CREATE_SUCCESSFUL                      = 257,

    (**
     * 3xx Positive Intermediate reply
     *
     * The command has been accepted, but the requested action is being held in
     * abeyance, pending receipt of further information. The user should send
     * another command specifying this information. This reply is used in command
     * sequence groups.
     *)

    (**
     * The command has been accepted, but the requested action is on hold,
     * pending receipt of further information.
     *)
    FTP_ACCEPTED_REQUEST_HOLD                            = 300,

    (**
     * User name okay, need password.
     *
     * A 331 code is sent in response to the USER command when a password is
     * required for the login to continue. It is considered as a positive
     * intermediate response and the client should immediately respond with a
     * PASS command.
     *)
    FTP_USERNAME_OK                                      = 331,

    (**
     * Need account for login.
     *
     * A 332 code is sent in response to a login-related command where an
     * account is required to continue the login process. It is considered as a
     * positive intermediate response and the client should immediately follow
     * with an ACCT command.
     *)
    FTP_NEED_ACCOUNT_TO_LOGIN                            = 332,

    (**
     * Requested file action pending further information
     *
     * A 350 response code is sent by the server in response to a file-related
     * command that requires further commands in order for the operation to be
     * completed. The original FTP specification identifies two instances where
     * this reply code can be used. The first is in response to a REST command,
     * which would indicate that the server has received the restart marker and
     * is pending the initiation of file transfer to resume the transfer at the
     * marker point. The second is in positive response to the RNFR command
     * where the server is waiting for an RNTOcommand to complete the file
     * rename operation.
     *)
    FTP_NEED_INFORMATION                                 = 350,

    (**
     * 4xx Transient Negative Completion reply
     *
     * The command was not accepted and the requested action did not take place,
     * but the error condition is temporary and the action may be requested
     * again. The user should return to the beginning of the command sequence,
     * if any. It is difficult to assign a meaning to "transient", particularly
     * when two distinct sites (Server- and User-processes) have to agree on the
     * interpretation. Each reply in the 4xx category might have a slightly
     * different time value, but the intent is that the user-process is
     * encouraged to try again. A rule of thumb in determining if a reply fits
     * into the 4xx or the 5xx (Permanent Negative) category is that replies are
     * 4xx if the commands can be repeated without any change in command form or
     * in properties of the User or Server (e.g., the command is spelled the
     * same with the same arguments used; the user does not change his file
     * access or user name; the server does not put up a new implementation.)
     *)

    (**
     * The command was not accepted and the requested action did not take place,
     * but the error condition is temporary and the action may be requested
     * again.
     *)
    FTP_NOT_ACCEPTED                                     = 400,

    (**
     * Service not available, closing control connection. This may be a reply to
     * any command if the service knows it must shut down.
     *
     * A 421 response code indicates that while the service is still running,
     * the service is unavailable at the time of connection. It indicates that
     * the server will be restarting as soon as it finishes processing pending
     * operations (usually any file transfers currently in progress). It is
     * considered a transient negative response, which means the client is
     * encouraged to issue the same command again at a later time when it can be
     * accepted.
     *)
    FTP_SERVICE_NOT_AVAILABLE                            = 421,

    (**
     * Can't open data connection.
     *
     * A 425 response code may be sent in response to any command requiring the
     * usage of a data connection if the server is unable to open a data
     * connection. This is considered a transient negative reply as it is
     * considered to be a temporary condition. It may indicate that the server
     * does not immediately have the resources available to open a data
     * connection. In this case, the client is encouraged to restart the FTP
     * transaction and try again.
     *)
    FTP_DATA_CONNECTION_NOT_AVAILABLE                    = 425,

    (**
     * Connection closed; transfer aborted.
     *
     * A 426 response code may be sent in response to any command requiring the
     * usage of a data connection. It is considered a transient negative reply
     * as it is considered to be a temporary condition. It is usually sent when
     * the data connection is unexpectedly closed before the completion of a
     * data transfer. In this case, the client is encouraged to restart the FTP
     * transaction and try again.
     *)
    FTP_TRANSFER_ABORTED                                 = 426,

    (**
     * Invalid username or password
     *)
    FTP_INVALID_USERNAME_OR_PASSWORD                     = 430,

    (**
     * Requested host unavailable.
     *)
    FTP_HOST_UNAVAILABLE                                 = 434,

    (**
     * Requested file action not taken.
     *
     * A 450 response code may be sent in response to any command requiring the
     * server to access a local file. It is a transient negative response as the
     * error is considered a temporary one. It is usually sent when the server
     * is unable to gain access to a required file at the time the command is
     * received. In this case, the client is encouraged to restart the FTP
     * transaction and try again.
     *)
    FTP_FILE_ACTION_ABORTED                              = 450,

    (**
     * Requested action aborted. Local error in processing.
     *
     * A 451 response code may be sent in response to any command initiating a
     * file transfer. It is a transient negative response, which means the error
     * condition is a temporary one. It is usually sent in response to the
     * server encountering an unexpected local error when processing data it is
     * transferring or receiving. In this case, the client is encouraged to
     * restart the FTP transaction and try again.
     *)
    FTP_LOCAL_ERROR                                      = 451,

    (**
     * Requested action not taken. Insufficient storage space in system. File
     * unavailable (e.g., file busy).
     *
     * A 452 response code may be given in response to any command requiring the
     * server to store transferred data it receives from the client (a file
     * upload). It is a transient negative response as the error is considered a
     * temporary one. It is usually sent because the server does not have
     * storage space to save the received data. In this case, the client is
     * encouraged to restart the FTP transaction and try again.
     *)
    FTP_INSUFFICIENT_STORAGE_SPACE                       = 452,

    (**
     * 5xx Permanent Negative Completion reply
     *
     * The command was not accepted and the requested action did not take place.
     * The User-process is discouraged from repeating the exact request (in the
     * same sequence). Even some "permanent" error conditions can be corrected, so
     * the human user may want to direct his User-process to reinitiate the
     * command sequence by direct action at some point in the future (e.g., after
     * the spelling has been changed, or the user has altered his directory
     * status.)
     *)

    (**
     * Syntax error, command unrecognized and the requested action did not take
     * place. This may include errors such as command line too long.
     *
     * A 500 response code may be sent in response to any command that the
     * server is unable to recognize. It is a permanent negative response, which
     * means the client is discouraged from sending the command again since the
     * server will respond with the same reply code. It usually means that the
     * client has sent a command to the server that the server does not
     * recognize. This may be due to an error in the spelling or formatting of
     * the command itself or that the command is newer than the FTP
     * implementation in place on the server or is a proprietary command of
     * another server implementation.
     *)
    FTP_COMMAND_UNRECOGNIZED                             = 500,

    (**
     * Syntax error in parameters or arguments.
     *
     * A 501 response code may be sent in response to any command that requires
     * or supports the optional use of a parameter or argument. It is a
     * permanent negative response, which means the client is discouraged from
     * sending the exact command and parameter(s) again since the server will
     * respond with the same response code. It differs from the 500 response
     * code in that the server recognizes the command present but is unable to
     * take the requested action due to a syntax error in the parameter(s)
     * present with the command. Sending the same command to the server with a
     * corrected parameter may result in a different response code.
     *)
    FTP_PARAMETERS_SYNTAX_ERROR                          = 501,

    (**
     * Command not implemented.
     *
     * A 502 code may be sent in response to any FTP command that the server
     * does not support. It is a permanent negative reply, which means the
     * client is discouraged from sending the command again since the server
     * will respond with the same reply code. The original FTP specification
     * dictates a minimum implementation for all FTP servers with a list of
     * required commands. Because of this, a 502 reply code should not be sent
     * in response to a required command.
     *)
    FTP_COMMAND_NOT_SUPPORT                              = 502,

    (**
     * Bad sequence of commands.
     *
     * A 503 response code may be sent in response to any command that requires
     * the successful processing of previous commands first. It is a permanent
     * negative reply, which means the client is discouraged from immediately
     * sending the command again since the server will respond with the same
     * reply code. For example, a file rename requires a successful RNFR command
     * before the RNTO command can be sent. Sending the RNTOcommand first will
     * result in a 503 response code.
     *)
    FTP_COMMAND_BAD_SEQUENCE                             = 503,

    (**
     * Command not implemented for that parameter.
     *
     * A 504 response code can be sent in response to any command using a
     * parameter that is not supported by the server. It is a permanent negative
     * response, which means the client is discouraged from sending the command
     * again since the server will respond with the same response code. Issuing
     * the same command with a different parameter may result in a different
     * response code.
     *)
    FTP_PARAMETER_NOT_SUPPORTED_BY_SERVER                = 504,

    (**
     * Not logged in.
     *
     * A 530 response code may be sent in response to any command that requires
     * a user to log in before the command is processed. Some servers may reply
     * to all commands with a 530 response code until the client logs in.
     *)
    FTP_NEED_LOGIN                                       = 530,

    (**
     * Need account for storing files.
     *
     * A 532 response code may be sent in response to any command involving the
     * storage or manipulation of files on the server. It is a permanent
     * negative response, which means the client is discouraged from sending the
     * command again since the server will respond with the same response code.
     * Providing account information first and sending the command again can
     * result in a different response code.
     *)
    FTP_NEED_ACCOUNT_FOR_STORIG_FILES                    = 532,

    (**
     * Could Not Connect to Server - Policy Requires SSL
     *
     * A 534 response code can be issued in response to any command that the
     * server is unwilling to process due to its security policy. It is a
     * permanent negative response, which means the client is discouraged from
     * sending the command again since the server will respond with the same
     * response code. It usually means that the server requires a certain level
     * of security to exist on the connection before processing the command or
     * that it is unwilling to process a command that would provide for
     * decreased security.
     *)
    FTP_REQUIRE_HIGHER_SECURITY_LEVEL                    = 534,

    (**
     * Requested action not taken. File unavailable (e.g., file not found, no
     * access).
     *
     * A 550 response code may be sent in response to any command requiring the
     * server to access a local file. It is a permanent negative response, which
     * means the client is discouraged from sending the command again since the
     * server will respond with the same response code. It is usually due to a
     * command requiring access to a file that does not exist or that the user
     * does not have access rights to.
     *)
    FTP_FILE_UNAVAILABLE                                 = 550,

    (**
     * Requested action aborted. Page type unknown.
     *
     * A 551 response code may be sent in response to any command requiring the
     * server to store information locally. It is a permanent negative reply,
     * which means the client is discouraged from sending the command again
     * since the server will respond with the same response code. It is only
     * applicable when the page file structure is being used (through a STRU P
     * command).
     *)
    FTP_PAGE_FILE_STRUCTURE_UNKNOWN                      = 551,

    (**
     * Requested file action aborted. Exceeded storage allocation (for current
     * directory or dataset).
     *
     * A 552 response code may be sent in response to any command requiring the
     * server to store received information locally. It is a permanent negative
     * response, which means the client is discouraged from sending the command
     * again since the server will respond with the same reply code. It usually
     * indicates that the logged in user has exceeded the storage space
     * allocated to their user account by the administrator.
     *)
    FTP_STORAGE_SPACE_EXCEEDED                           = 552,

    (**
     * Requested action not taken. File name not allowed.
     *
     * A 553 response code may be given in response to any command requiring or
     * supporting the use of a file name as a parameter. It is a permanent
     * negative reply, which means the client is discouraged from sending the
     * command again since the server will respond with the same reply code. It
     * is usually due to the file name contained as a parameter violating the
     * file naming policies existing on the server. Issuing the command again
     * with a different file name may result in a different reply code.
     *)
    FTP_FILENAME_NOT_ALLOWED                             = 553,

    (**
     * 6xx Protected reply
     *
     * The RFC 2228 introduced the concept of protected replies to increase
     * security over the FTP communications. The 6xx replies are Base64 encoded
     * protected messages that serves as responses to secure commands. When
     * properly decoded, these replies fall into the above categories.
     *)

    (**
     * Replies regarding confidentiality and integrity
     *)
    FTP_CONFIDENTIALITY_AND_INTEGRITY                    = 600,

    (**
     * Integrity protected reply.
     *)
    FTP_INTEGRITY_PROTECTED_REPLY                        = 631,

    (**
     * Confidentiality and integrity protected reply.
     *)
    FTP_CONFIDENTIALITY_AND_INTEGRITY_REPLY              = 632,

    (**
     * Confidentiality protected reply.
     *)
    FTP_CONFIDENTIALITY_REPLY                            = 633
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

  TPostRedirect = (
    REDIRECT_POST_NONE,

    (**
     * Tells the library to respect RFC 7231 (section 6.4.2 to 6.4.4) and not
     * convert POST requests into GET requests when following a 301 redirection
     *)
    REDIRECT_POST_301,

    (**
     * Makes libcurl maintain the request method after a 302 redirect
     *)
    REDIRECT_POST_302,

    (**
     * Makes libcurl maintain the request method after a 303 redirect
     *)
    REDIRECT_POST_303,

    REDIRECT_POST_ALL
  );

  TPostRedirects = set of TPostRedirect;

  TNETRCOption = (
   (**
    * The use of the ~/.netrc file is optional, and information in the URL is to
    * be preferred. The file will be scanned for the host and user name (to find
    * the password only) or for the host only, to find the first user name and
    * password after that machine, which ever information is not specified.
    *)
    NETRC_OPTIONAL                    = Longint(CURL_NETRC_OPTIONAL),

   (**
    * The library will ignore the ~/.netrc file.
    *)
    NETRC_IGNORED                     = Longint(CURL_NETRC_IGNORED),

   (**
    * The use of the ~/.netrc file is required, and information in the URL is to
    * be ignored. The file will be scanned for the host and user name (to find
    * the password only) or for the host only, to find the first user name and
    * password after that machine, which ever information is not specified.
    *)
    NETRC_REQUIRED                    = Longint(CURL_NETRC_REQUIRED)
  );

  TEncoding = (
   (**
    * Non-compressed
    *)
    ENCODE_NONE,

    (**
     * Requests the server to compress its response using the zlib algorithm
     *)
    ENCODE_DEFLATE,

   (**
    * Requests the gzip algorithm
    *)
    ENCODE_GZIP,

   (**
    * Requests the brotli algorithm
    *)
    ENCODE_BR
  );

  TEncodings = set of TEncoding;

  { TAltSvc }
  (**
   * Setting for the alt-svc engine
   *)
  TAltSvc = (
    (**
     * No Alt-Svc treatment.
     *)
    ALTSVC_DISABLE                 = 0,

    (**
     * If an Alt-Svc: header is received, this instructs libcurl to switch to
     * one of those alternatives asap rather than to save it and use for the
     * next request. (Not currently supported).
     *)
    ALTSVC_IMMEDIATELY             = Longint(CURLALTSVC_IMMEDIATELY),

    (**
     * Do not write the alt-svc cache back to the file specified with
     * TSession.HTTP.AltSvcCacheFile even if it gets updated. By default a file
     * specified with that option will be read and written to as deemed
     * necessary.
     *)
    ALTSVC_READONLYFILE            = Longint(CURLALTSVC_READONLYFILE),

    (**
     * Accept alternative services offered over HTTP/1.1.
     *)
    ALTSVC_H1                      = Longint(CURLALTSVC_H1),

    (**
     * Accept alternative services offered over HTTP/2. This will only be used
     * if libcurl was also built to actually support HTTP/2, otherwise this bit
     * will be ignored.
     *)
    ALTSVC_H2                      = Longint(CURLALTSVC_H2),

    (**
     * Accept alternative services offered over HTTP/3. This will only be used
     * if libcurl was also built to actually support HTTP/3, otherwise this bit
     * will be ignored.
     *)
    ALTSVC_H3                      = Longint(CURLALTSVC_H3)
  );

  TAltSvcs = set of TAltSvc;

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
    procedure SetMicroseconds ( us : QWord); inline;
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
      const FormatType : string = '0.###') : string;

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
      const FormatType : string = '0.###') : string;

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
  public
    type

      { TOptionsProperty }

      TOptionsProperty = class
      private
        FHandle : CURL;

        procedure SetAddressScope (AScope : Longint);
        procedure SetInterface (AInterface : string);
        procedure SetUnixSocketPath (APath : string);
        procedure SetAbstractUnixSocketPath (APath : string);
        procedure SetBufferSize (ASize : TDataSize);
        procedure SetFailOnError (AFailOnError : Boolean);
        procedure SetPathAsIs (ALeaveIt : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

         (**
         * Set scope id for IPv6 addresses
         *
         * Pass a long specifying the scope id value to use when connecting to
         * IPv6 addresses.
         *)
         property AddressScope : Longint write SetAddressScope default 0;

         (**
         * Source interface for outgoing trafic
         *
         * This sets the interface name to use as outgoing network interface.
         * The name can be an interface name, an IP address, or a host name.
         * If the parameter starts with "if!" then it is treated as only as
         * interface name and no attempt will ever be named to do treat it as an
         * IP address or to do name resolution on it. If the parameter starts
         * with "host!" it is treated as either an IP address or a hostname.
         * Hostnames are resolved synchronously. Using the if! format is highly
         * recommended when using the multi interfaces to avoid allowing the
         * code to block.
         *)
         property InterfaceName : string write SetInterface;

        (**
         * Set UNIX domain socket
         *
         * Enables the use of Unix domain sockets as connection endpoint and
         * sets the path to path. If path is '' (empty string), then Unix domain
         * sockets are disabled.
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
         * Enables the use of an abstract Unix domain socket instead of
         * establishing a TCP connection to a host.
         *)
         property AbstractUnixSocketPath : string
           write SetAbstractUnixSocketPath;

        (**
         * Set preffered receive buffer size
         *
         * Specifying your preferred size for the receive buffer in libcurl. The
         * main point of this would be that the write callback gets called more
         * often and with smaller chunks. Secondly, for some protocols, there's
         * a benefit of having a larger buffer for performance.
         * The minimum buffer size allowed to be set is 1024 bytes.
         *)
         property BufferSize : TDataSize write SetBufferSize;

        (**
         * Request failure on HTTP response >= 400
         *
         * Tells the library to fail the request if the HTTP code returned is
         * equal to or larger than 400. The default action would be to return
         * the page normally, ignoring that code.
         *)
        property FailOnError : Boolean write SetFailOnError default False;

        (**
         * Do not handle dot dot sequences
         *
         * Tell libcurl to not alter the given path before passing it on to the
         * server.
         * This instructs libcurl to NOT squash sequences of "/../" or "/./"
         * that may exist in the URL's path part and that is supposed to be
         * removed according to RFC 3986 section 5.2.4.
         * Some server implementations are known to (erroneously) require the
         * dot dot sequences to remain in the path and some clients want to pass
         * these on in order to try out server implementations.
         * By default libcurl will merge such sequences before using the path.
         *)
        property PathAsIs : Boolean write SetPathAsIs default False;

      end;

      { TProtocolProperty }

      TProtocolProperty = class
      private
        FHandle : CURL;

        procedure SetAllowedProtocols (AProtocols : TProtocols);
        procedure SetAllowedRedirectProtocols (AProtocols : TProtocols);
        procedure SetDefaultProtocol (AProtocol : TProtocol);
        procedure SetFollowRedirect (AFollow : Boolean);
        procedure SetMaxRedirects (AAmount : Longint);
        procedure SetNoBody (ANoBody : Boolean);
        procedure SetVerbose (AEnable : Boolean);
        procedure SetIncludeHeader (AIncludeHeader : Boolean);
        procedure SetIgnoreContentLength (AIgnoreLength : Boolean);
        procedure SetTransferEncoding (AEncoding : Boolean);
        procedure SetWildcardMatch (AMatch : Boolean);
        procedure SetKeepSendingOnError (AKeepSending : Boolean);
        procedure SetRemotePort (APort : Word);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Set allowed protocols
         *
         * Limits what protocols libcurl may use in the transfer. This allows
         * you to have a libcurl built to support a wide range of protocols but
         * still limit specific transfers to only be allowed to use a subset of
         * them. By default libcurl will accept all protocols it supports
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
         * Limits what protocols libcurl may use in a transfer that it follows
         * to in a redirect when FollowRedirect is enabled. This allows you to
         * limit specific transfers to only be allowed to use a subset of
         * protocols in redirections.
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
         * Follow HTTP 3XXX redirects
         *
         * Tells the library to follow any Location: header that the server
         * sends as part of an HTTP header in a 3xx response. The Location:
         * header can specify a relative or an absolute URL to follow.
         * libcurl will issue another request for the new URL and follow new
         * Location: headers all the way until no more such headers are
         * returned. libcurl limits what protocols it automatically follows to.
         * By default libcurl will allow HTTP, HTTPS, FTP and FTPS on redirect.
         *)
        property FollowRedirect : Boolean write SetFollowRedirect default True;

        (**
         * Meximum numbers of redirects allowed
         *
         * Setting the limit to 0 will make libcurl refuse any redirect.
         * Set it to -1 for an infinite number of redirects.
         *)
        property MaxRedirects : Longint write SetMaxRedirects default -1;

        (**
         * Do the download request without getting the body
         *
         * Tells libcurl to not include the body-part in the output when doing
         * what would otherwise be a download. For HTTP(S), this makes libcurl
         * do a HEAD request. For most other protocols it means just not asking
         * to transfer the body data.
         *)
        property NoBody : Boolean write SetNoBody default False;

        (**
         * Set verbose mode on/off
         *
         * Make the library display a lot of verbose information about its
         * operations. Very useful for libcurl and/or protocol debugging and
         * understanding. The verbose information will be sent to stderr.
         *)
        property VerboseMode : Boolean write SetVerbose default False;

        (**
         * Pass headers to the data stream
         *
         * Ask libcurl to include the headers in the data stream.
         * When asking to get the headers passed to the body, it is not possible
         * to accurately separate them again without detailed knowledge about
         * the protocol in use.
         *)
        property IncludeHeader : Boolean write SetIncludeHeader default False;

        (**
         * Ignore content length
         *
         * Ignore the Content-Length header in the HTTP response and ignore
         * asking for or relying on it for FTP transfers.
         * This is useful for HTTP with Apache 1.x (and similar servers) which
         * will report incorrect content length for files over 2 gigabytes. If
         * this option is used, curl will not be able to accurately report
         * progress, and will simply stop the download when the server ends the
         * connection. It is also useful with FTP when for example the file is
         * growing while the transfer is in progress which otherwise will
         * unconditionally cause libcurl to report error.
         *)
        property IgnoreContentLength : Boolean write SetIgnoreContentLength
          default False;

        (**
         * Ask for HTTP Transfer Encoding
         *
         * Add a request for compressed Transfer Encoding in the outgoing HTTP
         * request. If the server supports this and so desires, it can respond
         * with the HTTP response sent using a compressed Transfer-Encoding that
         * will be automatically uncompressed by libcurl on reception.
         *)
        property TransferEncoding : Boolean write SetTransferEncoding
          default False;

        (**
         * Enable directory wildcard transfers
         * [This feature is only supported for FTP download]
         *
         * Transfer multiple files according to a file name pattern. The pattern
         * can be specified as part of the Url option, using an fnmatch-like
         * pattern (Shell Pattern Matching) in the last part of URL (file name).
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
         * asterisk have no special meaning in a bracket expression. Each
         * bracket expression ends by the right bracket and matches exactly one
         * character. Some examples follow:
         * [a-zA-Z0-9] or [f-gF-G] - character interval
         * [abc] - character enumeration
         * [^abc] or [!abc] - negation
         * [[:name:]] class expression. Supported classes are alnum,lower,
         * space, alpha, digit, print, upper, blank, graph, xdigit.
         * [][-!^] - special case - matches only '-', ']', '[', '!' or '^'.
         * These characters have no special purpose.
         * [\[\]\\] - escape syntax. Matches '[', ']' or '´.
         * Using the rules above, a file name pattern can be constructed:
         * ftp://example.com/some/path/[a-z[:upper:]\\].jpeg
         *)
        property WildcardMatch : Boolean write SetWildcardMatch;

        (**
         * Keep sending on early HTTP response >= 300
         *
         * Tells the library to keep sending the request body if the HTTP code
         * returned is equal to or larger than 300. The default action would be
         * to stop sending and close the stream or connection.
         *)
        property KeepSendingOnError : Boolean write SetKeepSendingOnError
          default False;

        (**
         * Set remote port number to work with
         *
         * This option sets number to be the remote port number to connect to,
         * instead of the one specified in the URL or the default port for the
         * used protocol.
         * Usually, you just let the URL decide which port to use but this
         * allows the application to override that.
         *)
        property Port : Word write SetRemotePort;
      end;

      { TTCPProperty }

      TTCPProperty = class
      private
        FHandle : CURL;

        procedure SetTCPFastOpen (AEnable : Boolean);
        procedure SetTCPNoDelay (AEnable : Boolean);
        procedure SetTCPKeepalive (ASendProbe : Boolean);
        procedure SetTCPKeepIdle (ATime : TTimeInterval);
        procedure SetTCPKeepInterval (ATime : TTimeInterval);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Enable/disable TCP Fast Open
         *
         * TCP Fast Open (RFC7413) is a mechanism that allows data to be carried
         * in the SYN and SYN-ACK packets and consumed by the receiving end
         * during the initial connection handshake, saving up to one full
         * round-trip time (RTT).
         *)
        property FastOpen : Boolean write SetTCPFastOpen default False;

        (**
         * Disable TCP's Nagle algorithm on this connection
         *
         * The purpose of this algorithm is to try to minimize the number of
         * small packets on the network (where "small packets" means TCP
         * segments less than the Maximum Segment Size (MSS) for the network).
         * Maximizing the amount of data sent per TCP segment is good because it
         * amortizes the overhead of the send. However, in some cases small
         * segments may need to be sent without delay. This is less efficient
         * than sending larger amounts of data at a time, and can contribute to
         * congestion on the network if overdone.
         *)
        property NoDelay : Boolean write SetTCPNoDelay default True;

        (**
         * Enable/disable TCP keep-alive probing
         *
         * If set, TCP keepalive probes will be sent. The delay and frequency of
         * these probes can be controlled by the TCPKeepIdle and TCPKeepInterval
         * options, provided the operating system supports them. Set to False
         * (default behavior) to disable keepalive probes
         *)
        property Keepalive : Boolean write SetTCPKeepalive default False;

        (**
         * Set TCP keep-alive idle time wait
         *
         * Sets the delay, that the operating system will wait while the
         * connection is idle before sending keepalive probes. Not all operating
         * systems support this option.
         *)
        property KeepIdle : TTimeInterval write SetTCPKeepIdle;

        (**
         * Set TCP keep-alive interval
         *
         * Sets the interval, that the operating system will wait between
         * sending keepalive probes. Not all operating systems support this
         * option.
         *)
         property KeepInterval : TTimeInterval write SetTCPKeepInterval;
      end;

      { TSOCKS5Property }

      TSOCKS5Property = class
      private
        FHandle : CURL;

        procedure SetSOCKS5Auth (AMethod : TAuthMethods);
        procedure SetSOCKS5GSSAPIServiceName (AName : string);
        procedure SetSOCKS5GSSAPINegotiation (AEnable : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Set allowed methods for SOCKS5 proxy authentication
         *
         * Tell libcurl which authentication method(s) are allowed for SOCKS5
         * proxy authentication. The only supported flags are AUTH_BASIC, which
         * allows username/password authentication, AUTH_GSSAPI, which allows
         * GSS-API authentication, and AUTH_NONE, which allows no
         * authentication.
         *)
        property Auth : TAuthMethods write SetSOCKS5Auth
          default [AUTH_BASIC, AUTH_GSSAPI];

        (**
         * SOCKS5 proxy authentication service name
         *
         * String holding the name of the service. The default service name for
         * a SOCKS5 server is "rcmd". This option allows you to change it.
         *)
        property GSSAPIServiceName : string write SetSOCKS5GSSAPIServiceName;

        (**
         * Set socks proxy gssapi nogotiation protection
         *
         * As part of the gssapi negotiation a protection mode is negotiated.
         * The RFC 1961 says in section 4.3/4.4 it should be protected, but the
         * NEC reference implementation does not. If enabled, this option allows
         * the unprotected exchange of the protection mode negotiation.
         *)
        property GSSAPINegotiation : Boolean write SetSOCKS5GSSAPINegotiation;
      end;

      { TProxyProperty }

      TProxyProperty = class
      private
        FHandle : CURL;
        FSOCKS5 : TSOCKS5Property;

        procedure SetPreProxy (APreProxy : string);
        procedure SetProxy (AProxy : string);
        procedure SetPort (APort : Longint);
        procedure SetProxyType (AType : TProxyType);
        procedure SetProxyServiceName (AName : string);
        procedure SetNoProxyHosts (AHosts : string);
        procedure SetHttpProxyTunnel (AEnable : Boolean);
        procedure SetProxyUserPassword (AUserpwd : string);
        procedure SetProxyUsername (AName : string);
        procedure SetProxyPassword (APassword : string);
        procedure SetProxyTLSUsername (AName : string);
        procedure SetProxyTLSPassword (APassword : string);
        procedure SetProxyTLSAuth (AMethod : TTLSAuthMethod);
        procedure SetProxyHTTPAuth (AMethod : TAuthMethods);
        procedure SetHAProxyHeader (ASend : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        property SOCKS5 : TSOCKS5Property read FSOCKS5 write FSOCKS5;

        (**
         * Set pre-proxy to use
         *
         * Set the preproxy to use for the upcoming request. The parameter
         * should be a string holding the host name or dotted numerical IP
         * address. A numerical IPv6 address must be written within [brackets].
         * To specify port number in this string, append :[port] to the end of
         * the host name. The proxy's port number may optionally be specified
         * with the separate option Proxy. If not specified, libcurl will
         * default to using port 1080 for proxies.
         * A pre proxy is a SOCKS proxy that curl connects to before it connects
         * to the HTTP(S) proxy specified in the CURLOPT_PROXY option. The pre
         * proxy can only be a SOCKS proxy.
         * The pre proxy string should be prefixed with [scheme]:// to specify
         * which kind of socks is used. Use socks4://, socks4a://, socks5:// or
         * socks5h:// (the last one to enable socks5 and asking the proxy to do
         * the resolving, also known as CURLPROXY_SOCKS5_HOSTNAME type) to
         * request the specific SOCKS version to be used. Otherwise SOCKS4 is
         * used as default. Setting the pre proxy string to "" (an empty string)
         * will explicitly disable the use of a pre proxy.
         *)
        property PreProxy : string write SetPreProxy;

        (**
         * Set proxy to use
         *
         * Set the proxy to use for the upcoming request. The parameter should
         * be a string holding the host name or dotted numerical IP address. A
         * numerical IPv6 address must be written within [brackets].
         * To specify port number in this string, append :[port] to the end of
         * the host name. If not specified, libcurl will default to using port
         * 1080 for proxies.
         * The proxy string may be prefixed with [scheme]:// to specify which
         * kind of proxy is used.
         * http://    HTTP Proxy. Default when no scheme or proxy type is
         *            specified.
         * https://   HTTPS Proxy.
         * socks4://  SOCKS4 Proxy.
         * socks4a:// SOCKS4a Proxy. Proxy resolves URL hostname.
         * socks5://  SOCKS5 Proxy.
         * socks5h:// SOCKS5 Proxy. Proxy resolves URL hostname.
         * When you tell the library to use an HTTP proxy, libcurl will
         * transparently convert operations to HTTP even if you specify an FTP
         * URL etc.
         * Setting the proxy string to "" (an empty string) will explicitly
         * disable the use of a proxy, even if there is an environment variable
         * set for it. A proxy host string can also include protocol scheme
         * (http://) and embedded user + password.
         *)
        property Proxy : string write SetProxy;

        (**
         * Port number the proxy listens on
         *
         * Set the proxy port to connect to unless it is specified in the proxy
         * string.
         *)
        property Port : Longint write SetPort;

        (**
         * Proxy protocol type
         *)
        property Protocol : TProxyType write SetProxyType default PROXY_HTTP;

        (**
         * Proxy authentication service name
         *
         * String holding the name of the service. The default service name is
         * "HTTP" for HTTP based proxies and "rcmd" for SOCKS5. This option
         * allows you to change it.
         *)
        property ServiceName : string write SetProxyServiceName;

        (**
         * Disable proxy use for specific hosts
         *
         * The string consists of a comma separated list of host names that do
         * not require a proxy to get reached, even if one is specified. The
         * only wildcard available is a single * character, which matches all
         * hosts, and effectively disables the proxy. Each name in this list is
         * matched as either a domain which contains the hostname, or the
         * hostname itself. For example, example.com would match example.com,
         * example.com:80, and www.example.com, but not www.notanexample.com or
         * example.com.othertld. If the name in the noproxy list has a leading
         * period, it is a domain match against the provided host name. This way
         * ".example.com" will switch off proxy use for both "www.example.com"
         * as well as for "foo.example.com".
         * Setting the noproxy string to "" (an empty string) will explicitly
         * enable the proxy for all host names, even if there is an environment
         * variable set for it.
         * Enter IPv6 numerical addresses in the list of host names without
         * enclosing brackets: "example.com,::1,localhost"
         *)
        property NoProxyHosts : string write SetNoProxyHosts;

        (**
         * Tunnel through HTTP proxy
         *
         * Make libcurl tunnel all operations through the HTTP proxy (set with
         * Proxy property). There is a big difference between using a proxy and
         * to tunnel through it.
         * Tunneling means that an HTTP CONNECT request is sent to the proxy,
         * asking it to connect to a remote host on a specific port number and
         * then the traffic is just passed through the proxy. Proxies tend to
         * white-list specific port numbers it allows CONNECT requests to and
         * often only port 80 and 443 are allowed.
         *)
        property HttpTunnel : Boolean write SetHttpProxyTunnel;

        (**
         * User name and password to use for proxy authentification
         *
         * Pass a parameter, which should be [user name]:[password] to use for
         * the connection to the HTTP proxy. Both the name and the password will
         * be URL decoded before use, so to include for example a colon in the
         * user name you should encode it as %3A. (This is different to how
         * UserPassword is used - beware.)
         *)
        property UserPassword : string write SetProxyUserPassword;

        (**
         * User name to use for proxy authentication
         *
         * Sets the user name to be used in protocol authentication with the
         * proxy.
         *)
        property Username : string write SetProxyUsername;

        (**
         * Password to use with proxy authentication
         *
         * The option should be used in conjunction with the ProxyUsername
         * option.
         *)
        property Password : string write SetProxyPassword;

        (**
         * User name to use for proxy TLS authentication
         *)
        property TLSUsername : string write SetProxyTLSUsername;

        (**
         * Password to use for proxy TLS
         *
         * Requires that the ProxyTLSUsername option also be set.
         *)
        property TLSPassword : string write SetProxyTLSPassword;

        (**
         * Set proxy TLS authentication methods
         *)
        property TLSAuth : TTLSAuthMethod write SetProxyTLSAuth;

        (**
         * Set HTTP proxy authentication methods to try
         *
         * Tell libcurl which HTTP authentication method(s) you want it to use
         * for your proxy authentication. If more than one bit is set, libcurl
         * will first query the site to see what authentication methods it
         * supports and then pick the best one you allow it to use. For some
         * methods, this will induce an extra network round-trip.
         *)
        property HTTPAuth : TAuthMethods write SetProxyHTTPAuth
          default [AUTH_BASIC];

        (**
         * Send HAProxy PROXY protocol v.1 header
         *
         * Tells the library to send an HAProxy PROXY protocol v1 header at
         * beginning of the connection. The default action is not to send this
         * header.
         * This option is primarily useful when sending test requests to a
         * service that expects this header.
         *)
        property HAProxyProtocol : Boolean write SetHAProxyHeader default False;
      end;

      { TDNSProperty }

      TDNSProperty = class
      private
        FHandle : CURL;

        procedure SetDNSCacheTimeout (ATimeout : TTimeInterval);
        procedure SetDNSGlobalCache (AEnable : Boolean);
        procedure SetDNSoverHTTPS (AUrl : string);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Set life-time for DNS cache entries
         *
         * Name resolves will be kept in memory and used for this time interval.
         * Set to zero to completely disable caching.
         *)
        property CacheTimeout : TTimeInterval write SetDNSCacheTimeout;

        (**
         * Enable/disable global DNS cache
         *
         * Tells curl to use a global DNS cache that will survive between easy
         * handle creations and deletions. This is not thread-safe and this will
         * use a global variable.
         *
         * WARNING: this option is considered obsolete. Stop using it. Switch
         * over to using the share interface instead!
         *)
        property GlobalCache : Boolean write SetDNSGlobalCache;

        (**
         * Provide the DNS-over-HTTPS URL
         *
         * Pass in a string to a URL for the DOH server to use for name resolving.
         * The parameter should be a string which must be URL-encoded in the
         * following format: "https://host:port/path". It MUST specify a HTTPS URL.
         * Disable DOH use again by setting this option to '' (empty string).
         *)
        property DNSoverHTTPS : string write SetDNSoverHTTPS;
      end;

      { TSecurityProperty }

      TSecurityProperty = class
      private
        FHandle : CURL;

        procedure SetUserPassword (AUserpwd : string);
        procedure SetUsername (AName : string);
        procedure SetPassword (APassword : string);
        procedure SetTLSUsername (AName : string);
        procedure SetTLSPassword (APassword : string);
        procedure SetTLSAuth (AMethod : TTLSAuthMethod);
        procedure SetAllowUsernameInURL (AAllow : Boolean);
        procedure SetAuthServiceName (AName : string);
        procedure SetNetrc (AOption : TNETRCOption);
        procedure SetNetrcFile (AFile : string);
        procedure SetUnrestrictedAuth (AEnable : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Allow/disallow specifying user name in the url
         *)
        property AllowUsernameInURL : Boolean write SetAllowUsernameInURL
          default True;

        (**
         * User name and password to use in authentification
         *
         * Login details string for the connection. The format of which is:
         * [user name]:[password].
         * When using Kerberos V5 authentication with a Windows based server,
         * you should specify the user name part with the domain name in order
         * for the server to successfully obtain a Kerberos Ticket. If you don't
         * then the initial part of the authentication handshake may fail.
         * When using NTLM, the user name can be specified simply as the user
         * name without the domain name should the server be part of a single
         * domain and forest.
         * To specify the domain name use either Down-Level Logon Name or UPN
         * (User Principal Name) formats. For example, EXAMPLE\user and
         * user@example.com respectively.
         * When using HTTP and FollowLocation, libcurl might perform several
         * requests to possibly different hosts. libcurl will only send this
         * user and password information to hosts using the initial host name,
         * so if libcurl follows locations to other hosts it will not send the
         * user and password to those. This is enforced to prevent accidental
         * information leakage.
         *)
        property UserPassword : string write SetUserPassword;

        (**
         * User name to use in authentication
         *
         * Sets the user name to be used in protocol authentication. You should
         * not use this option together with the (older) UserPassword option.
         * When using Kerberos V5 authentication with a Windows based server,
         * you should include the domain name in order for the server to
         * successfully obtain a Kerberos Ticket. If you don't then the initial
         * part of the authentication handshake may fail.
         * When using NTLM, the user name can be specified simply as the user
         * name without the domain name should the server be part of a single
         * domain and forest.
         * To include the domain name use either Down-Level Logon Name or UPN
         * (User Principal Name) formats. For example, EXAMPLE\user and
         * user@example.com respectively.
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
         * User name to use for TLS authentication
         *)
        property TLSUsername : string write SetTLSUsername;

        (**
         * Password to use for TLS authentication
         *
         * Requires that the TLSUsername option also be set.
         *)
        property TLSPassword : string write SetTLSPassword;

        (**
         * Set TLS authentication methods
         *)
        property TLSAuth : TTLSAuthMethod write SetTLSAuth;

        (**
         * Authentication service name
         *
         * String holding the name of the service for DIGEST-MD5, SPNEGO and
         * Kerberos 5 authentication mechanisms. The default service names are
         * "ftp", "HTTP", "imap", "pop" and "smtp". This option allows you to
         * change them.
         *)
        property AuthServiceName : string write SetAuthServiceName;

        (**
         * Request then .netrc is used
         *
         * This parameter controls the preference level of libcurl between using
         * user names and passwords from your ~/.netrc file, relative to user
         * names and passwords in the URL supplied with URL. On Windows, libcurl
         * will use the file as %HOME%/_netrc, but you can also tell libcurl a
         * different file name to use with NetrcFile.
         *)
        property Netrc : TNETRCOption write SetNetrc default NETRC_IGNORED;

        (**
         * File name to read .netrc info from
         *
         * String containing the full path name to the file you want libcurl to
         * use as .netrc file. If this option is omitted, and Netrc is set,
         * libcurl will attempt to find a .netrc file in the current user's home
         * directory.
         *)
        property NetrcFile : string write SetNetrcFile;

        (**
         * Send credentials to other hosts too
         *
         * Set the long gohead parameter to 1L to make libcurl continue to send
         * authentication (user+password) credentials when following locations,
         * even when hostname changed. This option is meaningful only when
         * setting FollowRedirect.
         * By default, libcurl will only send given credentials to the initial
         * host name as given in the original URL, to avoid leaking username +
         * password to other sites.
         *)
        property UnrestrictedAuth : Boolean write SetUnrestrictedAuth
          default False;
      end;

      { THTTPCookie }
      THTTPCookie = class
      private
        FHandle : CURL;

        procedure SetCookie (ACookie : string);
        procedure SetCookieFile (AFile : string);
        procedure SetCookieJar (AFile : string);
        procedure SetCookieSession (ACreate : Boolean);
        procedure SetCookieList (ACookie : string);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Set contents of HTTP Cookie header
         *
         * Will be used to set a cookie in the HTTP request. The format of the
         * string should be NAME=CONTENTS, where NAME is the cookie name and
         * CONTENTS is what the cookie should contain.
         * If you need to set multiple cookies, set them all using a single
         * option concatenated like this: "name1=content1; name2=content2;" etc.
         * This option sets the cookie header explicitly in the outgoing
         * request(s). If multiple requests are done due to authentication,
         * followed redirections or similar, they will all get this cookie
         * passed on.
         * The cookies set by this option are separate from the internal cookie
         * storage held by the cookie engine and will not be modified by it. If
         * you enable the cookie engine and either you've imported a cookie of
         * the same name (e.g. 'foo') or the server has set one, it will have no
         * effect on the cookies you set here. A request to the server will send
         * both the 'foo' held by the cookie engine and the 'foo' held by this
         * option. To set a cookie that is instead held by the cookie engine and
         * can be modified by the server use CookieList.
         * Using this option multiple times will only make the latest string
         * override the previous ones.
         * This option will not enable the cookie engine. Use CookieFile or
         * CookieJar to enable parsing and sending cookies automatically.
         *)
        property Cookie : string write SetCookie;

        (**
         * File name to read cookie from
         *
         * It should point to the file name of your file holding cookie data to
         * read. The cookie data can be in either the old Netscape / Mozilla
         * cookie data format or just regular HTTP headers (Set-Cookie style)
         * dumped to a file.
         * It also enables the cookie engine, making libcurl parse and send
         * cookies on subsequent requests with this handle.
         * Given an empty or non-existing file or by passing the empty string
         * ("") to this option, you can enable the cookie engine without reading
         * any initial cookies. If you tell libcurl the file name is "-" (just a
         * single minus sign), libcurl will instead read from stdin.
         * This option only reads cookies. To make libcurl write cookies to
         * file, see CookieJar.
         * Exercise caution if you are using this option and multiple transfers
         * may occur. If you use the Set-Cookie format and don't specify a
         * domain then the cookie is sent for any domain (even after redirects
         * are followed) and cannot be modified by a server-set cookie. If a
         * server sets a cookie of the same name then both will be sent on a
         * future transfer to that server, likely not what you intended. To
         * address these issues set a domain in Set-Cookie (doing that will
         * include sub-domains) or use the Netscape format.
         * If you use this option multiple times, you just add more files to
         * read. Subsequent files will add more cookies.
         *)
        property CookieFile : string write SetCookieFile;

        (**
         * File name to store cookies to
         *
         * This will make libcurl write all internally known cookies to the
         * specified file when curl_easy_cleanup is called. If no cookies are
         * known, no file will be created. Specify "-" as filename to instead
         * have the cookies written to stdout. Using this option also enables
         * cookies for this session, so if you for example follow a location it
         * will make matching cookies get sent accordingly.
         * Note that libcurl doesn't read any cookies from the cookie jar. If
         * you want to read cookies from a file, use CookieFile.
         * If the cookie jar file can't be created or written to (when the
         * curl_easy_cleanup is called), libcurl will not and cannot report an
         * error for this.
         * Since 7.43.0 cookies that were imported in the Set-Cookie format
         * without a domain name are not exported by this option.
         *)
        property CookieJar : string write SetCookieJar;

        (**
         * Start a new cookie session
         *
         * Mark this as a new cookie "session". It will force libcurl to ignore
         * all cookies it is about to load that are "session cookies" from the
         * previous session. By default, libcurl always stores and loads all
         * cookies, independent if they are session cookies or not. Session
         * cookies are cookies without expiry date and they are meant to be
         * alive and existing for this "session" only.
         *)
        property CookieNewSession : Boolean write SetCookieSession;

        (**
         * Add to or manipulate cookies held in memory
         *
         * Such a cookie can be either a single line in Netscape / Mozilla
         * format or just regular HTTP-style header (Set-Cookie: ...) format.
         * This will also enable the cookie engine. This adds that single cookie
         * to the internal cookie store.
         * Exercise caution if you are using this option and multiple transfers
         * may occur. If you use the Set-Cookie format and don't specify a
         * domain then the cookie is sent for any domain (even after redirects
         * are followed) and cannot be modified by a server-set cookie. If a
         * server sets a cookie of the same name (or maybe you've imported one)
         * then both will be sent on a future transfer to that server, likely
         * not what you intended. To address these issues set a domain in
         * Set-Cookie (doing that will include sub-domains) or use the Netscape
         * format.
         * Additionally, there are commands available that perform actions if
         * you pass in these exact strings:
         * ALL
         * erases all cookies held in memory
         * SESS
         * erases all session cookies held in memory
         * FLUSH
         * writes all known cookies to the file specified by CookieJar
         * RELOAD
         * loads all cookies from the files specified by CookieFile
         *)
        property CookieList : string write SetCookieList;
      end;

      { THTTPProperty }

      THTTPProperty = class
      private
        FHandle : CURL;
        FCookie : THTTPCookie;

        procedure SetUserAgent (AAgent : string);
        procedure SetAutoReferer (AUpdateHeaders : Boolean);
        procedure SetHTTPAuth (AMethod : TAuthMethods);
        procedure SetUnrestrictedAuth (ASend : Boolean);
        procedure SetPostRedirect (ARedirect : TPostRedirects);
        procedure SetPutMethod (AEnable : Boolean);
        procedure SetPostMethod (AEnable : Boolean);
        procedure SetPostFields (AData : string);
        procedure SetPostFieldsSize (ASize : Longint);
        procedure SetAcceptEncoding (AEncodings : TEncodings);
        procedure SetTransferEncoding (AEnable : Boolean);
        procedure SetReferer (AWhere : string);
        procedure SetAltSvcCacheFile (AFile : string);
        procedure SetAltSvcCtrl (AAltSvc : TAltSvcs);
        procedure SetGetMethod (AEnable : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        property Cookie : THTTPCookie read FCookie write FCookie;

        (**
         * Set HTTP user-agent header
         *
         * It will be used to set the User-Agent: header in the HTTP request
         * sent to the remote server. This can be used to fool servers or
         * scripts.
         *)
        property UserAgent : string write SetUserAgent;

        (**
         * Automatically update the referer header
         *
         * When enabled, libcurl will automatically set the Referer: header
         * field in HTTP requests where it follows a Location: redirect.
         *)
        property AutoReferer : Boolean write SetAutoReferer default True;

        (**
         * Tell libcurl which authentication method(s) you want it to use
         * speaking to the remote server.
         *)
        property HTTPAuth : TAuthMethods write SetHTTPAuth default [AUTH_BASIC];

        (**
         * Send credentials to other hosts too
         *
         * Make libcurl continue to send authentication (user+password)
         * credentials when following locations, even when hostname changed.
         * By default, libcurl will only send given credentials to the initial
         * host name as given in the original URL, to avoid leaking username +
         * password to other sites.
         *)
        property UnrestrictedAuth : Boolean write SetUnrestrictedAuth;

        (**
         * How to act on an HTTP POST redirect
         *)
        property PostRedirect : TPostRedirects write SetPostRedirect
          default [REDIRECT_POST_NONE];

        (**
         * Make an HTTP PUT request
         *
         * This option is deprecated since version 7.12.1. Use Upload!
         *)
        property Put : Boolean write SetPutMethod default False;

        (**
         * Request an HTTP POST
         *)
        property Post : Boolean write SetPostMethod default False;

        (**
         * Specify data to POST to server
         *
         * Pass the full data to send in an HTTP POST operation. You must make
         * sure that the data is formatted the way you want the server to
         * receive it. libcurl will not convert or encode it for you in any way.
         * For example, the web server may assume that this data is url-encoded.
         *)
        property PostFields : string write SetPostFields;

        (**
         * Size of POST data
         *
         * If you want to post data to the server without having libcurl do a
         * strlen() to measure the data size, this option must be used. When
         * this option is used you can post fully binary data, which otherwise
         * is likely to fail. If this size is set to -1, the library will use
         * strlen() to get the size.
         *)
        property PostFieldsSize : Longint write SetPostFieldsSize default -1;

        (**
         * Enables automatic decompression of HTTP
         *
         * Sets the contents of the Accept-Encoding: header sent in an HTTP
         * request, and enables decoding of a response when a Content-Encoding:
         * header is received.
         * libcurl potentially supports several different compressed encodings
         * depending on what support that has been built-in.
         * Alternatively, you can specify exactly the encoding or list of
         * encodings you want in the response. Four encodings are supported:
         * identity, meaning non-compressed, deflate which requests the server
         * to compress its response using the zlib algorithm, gzip which
         * requests the gzip algorithm and (since curl 7.57.0) br which is
         * brotli.
         *)
        property AcceptEncoding : TEncodings write SetAcceptEncoding
          default [ENCODE_NONE];

        (**
         * Ask for HTTP Transfer Encoding
         *
         * Adds a request for compressed Transfer Encoding in the outgoing HTTP
         * request. If the server supports this and so desires, it can respond
         * with the HTTP response sent using a compressed Transfer-Encoding that
         * will be automatically uncompressed by libcurl on reception.
         * Transfer-Encoding differs slightly from the Content-Encoding you ask
         * for with AcceptEncoding in that a Transfer-Encoding is strictly meant
         * to be for the transfer and thus MUST be decoded before the data
         * arrives in the client. Traditionally, Transfer-Encoding has been much
         * less used and supported by both HTTP clients and HTTP servers.
         *)
        property TransferEncoding : Boolean write SetTransferEncoding
          default False;

        (**
         * Set the HTTP referer header
         *
         * It will be used to set the Referer: header in the http request sent
         * to the remote server. This can be used to fool servers or scripts.
         * You can also set any custom header with Header.
         *)
        property Referer : string write SetReferer;

        (**
         * Set alt-svc cache file name
         *
         * Pass in a pointer to a filename to instruct libcurl to use that file
         * as the Alt-Svc cache to read existing cache contents from and
         * possibly also write it back to a after a transfer.
         * Specify a blank file name ("") to make libcurl not load from a file
         * at all.
         *)
        property AltSvcCacheFile : string write SetAltSvcCacheFile;

        (**
         * Control alt-svc behavior
         *
         * Populate the correct set of features to instruct libcurl how to
         * handle Alt-Svc for the transfers using this handle.
         * libcurl will only accept Alt-Svc headers over a secure transport,
         * meaning HTTPS. It will also only complete a request to an alternative
         * origin if that origin is properly hosted over HTTPS. These
         * requirements are there to make sure both the source and the
         * destination are legitimate.
         * Setting any bit will enable the alt-svc engine.
         *)
        property AltSvcCtrl : TAltSvcs write SetAltSvcCtrl;

        (**
         * Ask for an HTTP GET request
         *
         * This forces the HTTP request to get back to using GET. Usable if a
         * POST, HEAD, PUT, etc has been used previously using the same curl
         * handle.
         * When setting Get, it will automatically set NoBody to False and
         * Upload to False.
         *)
        property Get : Boolean write SetGetMethod;
      end;

      { TIMAPProperty }

      TIMAPProperty = class
      private
        FHandle : CURL;

        procedure SetLoginOptions (AOptions : string);
        procedure SetSASLAuthzid (AAuthzid : string);
        procedure SetSASLIR (ASend : Boolean);
        procedure SetXOAuth2Bearer (AToken : string);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Set login options
         *
         * For more information about the login options please see RFC 2384, RFC
         * 5092 and IETF draft draft-earhart-url-smtp-00.txt
         * LoginOptions can be used to set protocol specific login options, such
         * as the preferred authentication mechanism via "AUTH=NTLM" or
         * "AUTH=*", and should be used in conjunction with the Username option.
         *)
        property LoginOptions : string write SetLoginOptions;

        (**
         * Authorisation identity (identity to act as)
         *
         * Authorisation identity (authzid) for the transfer. Only applicable to
         * the PLAIN SASL authentication mechanism where it is optional.
         * When not specified only the authentication identity (authcid) as
         * specified by the username will be sent to the server, along with the
         * password. The server will derive a authzid from the authcid when not
         * provided, which it will then uses internally.
         * When the authzid is specified, the use of which is server dependent,
         * it can be used to access another user's inbox, that the user has been
         * granted access to, or a shared mailbox for example.
         *)
        property SASLAuthzid : string write SetSASLAuthzid;

        (**
         * Enable/disable sending initial response in first packet
         *
         * curl will send the initial response to the server in the first
         * authentication packet in order to reduce the number of ping pong
         * requests. Only applicable to the following supporting SASL
         * authentication mechanisms:
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
      end;

      { TFTPProperty }

      TFTPProperty = class
      private
        FHandle : CURL;

      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;
      end;

  protected
    FHandle : CURL;
    FBuffer : TMemoryStream;
    FUploadOffset : Int64;
    FOptions : TOptionsProperty;
    FProtocol : TProtocolProperty;
    FTCP : TTCPProperty;
    FProxy : TProxyProperty;
    FDNS : TDNSProperty;
    FSecurity : TSecurityProperty;
    FHTTP : THTTPProperty;
    FIMAP : TIMAPProperty;
    FFTP : TFTPProperty;

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
    procedure SetLocalPort (APort : Word);
    procedure SetLocalPortRange (ARange : Longint);
  public
    function ExtractProtocol (AUrl : string) : TProtocol;
  public
    constructor Create;
    destructor Destroy; override;

    property Options : TOptionsProperty read FOptions write FOptions;
    property Protocol : TProtocolProperty read FProtocol write FProtocol;
    property TCP : TTCPProperty read FTCP write FTCP;
    property Proxy : TProxyProperty read FProxy write FProxy;
    property DNS : TDNSProperty read FDNS write FDNS;
    property Security : TSecurityProperty read FSecurity write FSecurity;
    property HTTP : THTTPProperty read FHTTP write FHTTP;
    property IMAP : TIMAPProperty read FIMAP write FIMAP;

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
     * Set local port number to use for socket
     *
     * This sets the local port number of the socket used for the connection.
     * 0, disabled - use whatever the system thinks is fine
     *)
     property Port : Word write SetLocalPort;

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
     property PortRange : Longint write SetLocalPortRange default 1;
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

  { TResponse }
  { Getting information from server response. }

  TResponse = class
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
    function GetResponseCode : Longint;
    function GetContent : string;
    function GetVerifySSLResult : boolean;
    function GetVerifySSLProxyResult : boolean;
    function GetConnectResponseCode : THTTPStatusCode;
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
    constructor Create (s : TSession);

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
    property PrimaryIP : string read GetPrimaryIP;

    (**
     * Get local IP address of last connection
     *)
    property LocalIP : string read GetLocalIP;

    (**
     * Get the last response code
     *)
    property ResponseCode : Longint read GetResponseCode;

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
    property ConnectResponseCode : THTTPStatusCode read GetConnectResponseCode;

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

{ TSession.THTTPCookie }

procedure TSession.THTTPCookie.SetCookie(ACookie: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_COOKIE, PChar(ACookie));
end;

procedure TSession.THTTPCookie.SetCookieFile(AFile: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_COOKIEFILE, PChar(AFile));
end;

procedure TSession.THTTPCookie.SetCookieJar(AFile: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_COOKIEJAR, PChar(AFile));
end;

procedure TSession.THTTPCookie.SetCookieSession(ACreate: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_COOKIESESSION, Longint(ACreate));
end;

procedure TSession.THTTPCookie.SetCookieList(ACookie: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_COOKIELIST, PChar(ACookie));
end;

constructor TSession.THTTPCookie.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.THTTPCookie.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TFTPProperty }

constructor TSession.TFTPProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TFTPProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TIMAPProperty }

procedure TSession.TIMAPProperty.SetLoginOptions(AOptions: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_LOGIN_OPTIONS, PChar(AOptions));
end;

procedure TSession.TIMAPProperty.SetSASLAuthzid(AAuthzid: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_SASL_AUTHZID, PChar(AAuthzid));
end;

procedure TSession.TIMAPProperty.SetSASLIR(ASend: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_SASL_IR, Longint(ASend));
end;

procedure TSession.TIMAPProperty.SetXOAuth2Bearer(AToken: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_XOAUTH2_BEARER, PChar(AToken));
end;

constructor TSession.TIMAPProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TIMAPProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TProtocolProperty }

procedure TSession.TProtocolProperty.SetAllowedProtocols
  (AProtocols: TProtocols);
var
  bitmask : Longint;
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

  curl_easy_setopt(FHandle, CURLOPT_PROTOCOLS, bitmask);
end;

procedure TSession.TProtocolProperty.SetAllowedRedirectProtocols
  (AProtocols: TProtocols);
var
  bitmask : Longint;
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

  curl_easy_setopt(FHandle, CURLOPT_REDIR_PROTOCOLS, bitmask);
end;

procedure TSession.TProtocolProperty.SetDefaultProtocol(AProtocol: TProtocol);
var
  protocol : string;
begin
  protocol := GetEnumName(TypeInfo(TProtocol), Ord(AProtocol));
  protocol := LowerCase(Copy(protocol, Length('PROTOCOL_') + 1,
    Length(protocol) - Length('PROTOCOL_') + 1));
  curl_easy_setopt(FHandle, CURLOPT_DEFAULT_PROTOCOL, PChar(protocol));
end;

procedure TSession.TProtocolProperty.SetFollowRedirect(AFollow: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_FOLLOWLOCATION, Longint(AFollow));
end;

procedure TSession.TProtocolProperty.SetMaxRedirects(AAmount: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_MAXREDIRS, AAmount);
end;

procedure TSession.TProtocolProperty.SetNoBody(ANoBody: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_NOBODY, Longint(ANoBody));
end;

procedure TSession.TProtocolProperty.SetVerbose(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_VERBOSE, Longint(AEnable));
end;

procedure TSession.TProtocolProperty.SetIncludeHeader(AIncludeHeader: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_HEADER, Longint(AIncludeHeader));
end;

procedure TSession.TProtocolProperty.SetIgnoreContentLength(
  AIgnoreLength: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_IGNORE_CONTENT_LENGTH,
    Longint(AIgnoreLength));
end;

procedure TSession.TProtocolProperty.SetTransferEncoding(AEncoding: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TRANSFER_ENCODING, Longint(AEncoding));
end;

procedure TSession.TProtocolProperty.SetWildcardMatch(AMatch: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_WILDCARDMATCH, Longint(AMatch));
end;

procedure TSession.TProtocolProperty.SetKeepSendingOnError
  (AKeepSending: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_KEEP_SENDING_ON_ERROR,
    Longint(AKeepSending));
end;

procedure TSession.TProtocolProperty.SetRemotePort(APort: Word);
begin
  curl_easy_setopt(FHandle, CURLOPT_PORT, Longint(APort));
end;

constructor TSession.TProtocolProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TProtocolProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.THTTPProperty }

procedure TSession.THTTPProperty.SetUserAgent(AAgent: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_USERAGENT, PChar(AAgent));
end;

procedure TSession.THTTPProperty.SetAutoReferer(AUpdateHeaders: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_AUTOREFERER, Longint(AUpdateHeaders));
end;

procedure TSession.THTTPProperty.SetHTTPAuth(AMethod: TAuthMethods);
var
  bitmask : Longint;
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

  curl_easy_setopt(FHandle, CURLOPT_HTTPAUTH, bitmask);
end;

procedure TSession.THTTPProperty.SetUnrestrictedAuth(ASend: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_UNRESTRICTED_AUTH, Longint(ASend));
end;

procedure TSession.THTTPProperty.SetPostRedirect(ARedirect: TPostRedirects);
var
  bitmask : Longint;
begin
  bitmask := 0;
  if REDIRECT_POST_301 in ARedirect then
    bitmask := bitmask or CURL_REDIR_POST_301;
  if REDIRECT_POST_302 in ARedirect then
    bitmask := bitmask or CURL_REDIR_POST_302;
  if REDIRECT_POST_303 in ARedirect then
    bitmask := bitmask or CURL_REDIR_POST_303;
  if REDIRECT_POST_ALL in ARedirect then
    bitmask := CURL_REDIR_POST_ALL;

  curl_easy_setopt(FHandle, CURLOPT_POSTREDIR, bitmask);
end;

procedure TSession.THTTPProperty.SetPutMethod(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_PUT, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetPostMethod(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_POST, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetPostFields(AData: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_POSTFIELDS, PChar(AData));
end;

procedure TSession.THTTPProperty.SetPostFieldsSize(ASize: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_POSTFIELDSIZE_LARGE, ASize);
end;

procedure TSession.THTTPProperty.SetAcceptEncoding(AEncodings: TEncodings);

  function GetEncodingName (AEncoding : TEncoding) : string;
  var
    alg : string;
  begin
    alg := GetEnumName(TypeInfo(TEncodings), Ord(AEncoding));
    Result := LowerCase(Copy(alg, Length('ENCODE_') + 1, Length(alg) -
      Length('ENCODE_') + 1));
  end;

  procedure UpdateEncodeString (var encode : string);
  begin
    if encode <> '' then
      encode := encode + ', ';
  end;

var
  enc : string = '';
begin
  if AEncodings = [ENCODE_NONE] then
  begin
    curl_easy_setopt(FHandle, CURLOPT_ACCEPT_ENCODING, 0);
  end else
  begin
    if ENCODE_DEFLATE in AEncodings then
    begin
      UpdateEncodeString(enc);
      enc := enc + GetEncodingName(ENCODE_DEFLATE);
    end;
    if ENCODE_GZIP in AEncodings then
    begin
      UpdateEncodeString(enc);
      enc := enc + GetEncodingName(ENCODE_GZIP);
    end;
    if ENCODE_BR in AEncodings then
    begin
      UpdateEncodeString(enc);
      enc := enc + GetEncodingName(ENCODE_BR);
    end;

    curl_easy_setopt(FHandle, CURLOPT_ACCEPT_ENCODING, PChar(enc));
  end;
end;

procedure TSession.THTTPProperty.SetTransferEncoding(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TRANSFER_ENCODING, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetReferer(AWhere: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_REFERER, PChar(AWhere));
end;

procedure TSession.THTTPProperty.SetAltSvcCacheFile(AFile: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_ALTSVC, PChar(AFile));
end;

procedure TSession.THTTPProperty.SetAltSvcCtrl(AAltSvc: TAltSvcs);
var
  bitmask : Longint;
begin
  bitmask := 0;
  if [ALTSVC_DISABLE] = AAltSvc then
  begin
    curl_easy_setopt(FHandle, CURLOPT_ALTSVC_CTRL, 0);
  end else
  begin
    if ALTSVC_IMMEDIATELY in AAltSvc then
    begin
      bitmask := bitmask or CURLALTSVC_IMMEDIATELY;
    end;
    if ALTSVC_READONLYFILE in AAltSvc then
    begin
      bitmask := bitmask or CURLALTSVC_READONLYFILE;
    end;
    if ALTSVC_H1 in AAltSvc then
    begin
      bitmask := bitmask or CURLALTSVC_H1;
    end;
    if ALTSVC_H2 in AAltSvc then
    begin
      bitmask := bitmask or CURLALTSVC_H2;
    end;
    if ALTSVC_H3 in AAltSvc then
    begin
      bitmask := bitmask or CURLALTSVC_H3;
    end;
    curl_easy_setopt(FHandle, CURLOPT_ALTSVC_CTRL, bitmask);
  end;
end;

procedure TSession.THTTPProperty.SetGetMethod(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_HTTPGET, Longint(AEnable));
end;

constructor TSession.THTTPProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
  FCookie := THTTPCookie.Create(AHandle);
end;

destructor TSession.THTTPProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TSecurityProperty }

procedure TSession.TSecurityProperty.SetUserPassword(AUserpwd: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_USERPWD, PChar(AUserpwd));
end;

procedure TSession.TSecurityProperty.SetUsername(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_USERNAME, PChar(AName));
end;

procedure TSession.TSecurityProperty.SetPassword(APassword: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PASSWORD, PChar(APassword));
end;

procedure TSession.TSecurityProperty.SetTLSUsername(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_TLSAUTH_USERNAME, PChar(AName));
end;

procedure TSession.TSecurityProperty.SetTLSPassword(APassword: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_TLSAUTH_PASSWORD, PChar(APassword));
end;

procedure TSession.TSecurityProperty.SetTLSAuth(AMethod: TTLSAuthMethod);
begin
  curl_easy_setopt(FHandle, CURLOPT_TLSAUTH_TYPE,
    PChar(GetEnumName(TypeInfo(TTLSAuthMethod), ord(AMethod))));
end;

procedure TSession.TSecurityProperty.SetAllowUsernameInURL(AAllow: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_DISALLOW_USERNAME_IN_URL,
    Longint(Boolean(not AAllow)));
end;

procedure TSession.TSecurityProperty.SetAuthServiceName(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_SERVICE_NAME, PChar(AName));
end;

procedure TSession.TSecurityProperty.SetNetrc(AOption: TNETRCOption);
begin
  curl_easy_setopt(FHandle, CURLOPT_NETRC, Longint(AOption));
end;

procedure TSession.TSecurityProperty.SetNetrcFile(AFile: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_NETRC_FILE, PChar(AFile));
end;

procedure TSession.TSecurityProperty.SetUnrestrictedAuth(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_UNRESTRICTED_AUTH, Longint(AEnable));
end;

constructor TSession.TSecurityProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TSecurityProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TDNSProperty }

procedure TSession.TDNSProperty.SetDNSCacheTimeout(ATimeout: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_DNS_CACHE_TIMEOUT,
    Longint(ceil(ATimeout.Seconds)));
end;

procedure TSession.TDNSProperty.SetDNSGlobalCache(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_DNS_USE_GLOBAL_CACHE, Longint(AEnable));
end;

procedure TSession.TDNSProperty.SetDNSoverHTTPS(AUrl: string);
begin
  if AUrl <> '' then
  begin
    curl_easy_setopt(FHandle, CURLOPT_DOH_URL, PChar(AUrl));
  end else
  begin
    curl_easy_setopt(FHandle, CURLOPT_DOH_URL, 0);
  end;
end;

constructor TSession.TDNSProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TDNSProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TSOCKS5Property }

procedure TSession.TSOCKS5Property.SetSOCKS5Auth(AMethod: TAuthMethods);
var
  bitmask : Longint;
begin
  bitmask := 0;
  if AUTH_BASIC in AMethod then
    bitmask := bitmask or CURLAUTH_BASIC;
  if AUTH_GSSAPI in AMethod then
    bitmask := bitmask or CURLAUTH_GSSAPI;

  curl_easy_setopt(FHandle, CURLOPT_SOCKS5_AUTH, bitmask);
end;

procedure TSession.TSOCKS5Property.SetSOCKS5GSSAPIServiceName(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_SOCKS5_GSSAPI_SERVICE, PChar(AName));
end;

procedure TSession.TSOCKS5Property.SetSOCKS5GSSAPINegotiation(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_SOCKS5_GSSAPI_NEC, Longint(AEnable));
end;

constructor TSession.TSOCKS5Property.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TSOCKS5Property.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TProxyProperty }

procedure TSession.TProxyProperty.SetPreProxy(APreProxy: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PRE_PROXY, PChar(APreProxy));
end;

procedure TSession.TProxyProperty.SetProxy(AProxy: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY, PChar(AProxy));
end;

procedure TSession.TProxyProperty.SetPort(APort: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXYPORT, APort);
end;

procedure TSession.TProxyProperty.SetProxyType(AType: TProxyType);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXYTYPE, Longint(AType));
end;

procedure TSession.TProxyProperty.SetProxyServiceName(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_SERVICE_NAME, PChar(AName));
end;

procedure TSession.TProxyProperty.SetNoProxyHosts(AHosts: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_NOPROXY, PChar(AHosts));
end;

procedure TSession.TProxyProperty.SetHttpProxyTunnel(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_HTTPPROXYTUNNEL, Longint(AEnable));
end;

procedure TSession.TProxyProperty.SetProxyUserPassword(AUserpwd: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXYUSERPWD, PChar(AUserpwd));
end;

procedure TSession.TProxyProperty.SetProxyUsername(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXYUSERNAME, PChar(AName));
end;

procedure TSession.TProxyProperty.SetProxyPassword(APassword: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXYPASSWORD, PChar(APassword));
end;

procedure TSession.TProxyProperty.SetProxyTLSUsername(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_TLSAUTH_USERNAME, PChar(AName));
end;

procedure TSession.TProxyProperty.SetProxyTLSPassword(APassword: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_TLSAUTH_PASSWORD, PChar(APassword));
end;

procedure TSession.TProxyProperty.SetProxyTLSAuth(AMethod: TTLSAuthMethod);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_TLSAUTH_TYPE,
    PChar(GetEnumName(TypeInfo(TTLSAuthMethod), ord(AMethod))));
end;

procedure TSession.TProxyProperty.SetProxyHTTPAuth(AMethod: TAuthMethods);
var
  bitmask : Longint;
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

  curl_easy_setopt(FHandle, CURLOPT_PROXYAUTH, bitmask);
end;

procedure TSession.TProxyProperty.SetHAProxyHeader(ASend: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_HAPROXYPROTOCOL, Longint(ASend));
end;

constructor TSession.TProxyProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
  FSOCKS5 := TSOCKS5Property.Create(AHandle);
end;

destructor TSession.TProxyProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TTCPProperty }

procedure TSession.TTCPProperty.SetTCPFastOpen(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TCP_FASTOPEN, Longint(AEnable));
end;

procedure TSession.TTCPProperty.SetTCPNoDelay(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TCP_NODELAY, Longint(AEnable));
end;

procedure TSession.TTCPProperty.SetTCPKeepalive(ASendProbe: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TCP_KEEPALIVE, Longint(ASendProbe));
end;

procedure TSession.TTCPProperty.SetTCPKeepIdle(ATime: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_TCP_KEEPIDLE, Longint(Ceil(ATime.Seconds)));
end;

procedure TSession.TTCPProperty.SetTCPKeepInterval(ATime: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_TCP_KEEPINTVL,
    Longint(Ceil(ATime.Seconds)));
end;

constructor TSession.TTCPProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TTCPProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TOptionsProperty }

procedure TSession.TOptionsProperty.SetAddressScope(AScope: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_ADDRESS_SCOPE, AScope);
end;

procedure TSession.TOptionsProperty.SetInterface(AInterface: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_INTERFACE, PChar(AInterface));
end;

procedure TSession.TOptionsProperty.SetUnixSocketPath(APath: string);
begin
  if APath = '' then
  begin
    curl_easy_setopt(FHandle, CURLOPT_UNIX_SOCKET_PATH, 0);
  end else
  begin
    curl_easy_setopt(FHandle, CURLOPT_UNIX_SOCKET_PATH, PChar(APath));
  end;
end;

procedure TSession.TOptionsProperty.SetAbstractUnixSocketPath(APath: string);
begin
  if APath = '' then
  begin
    curl_easy_setopt(FHandle, CURLOPT_ABSTRACT_UNIX_SOCKET, 0);
  end else
  begin
    curl_easy_setopt(FHandle, CURLOPT_ABSTRACT_UNIX_SOCKET, PChar(APath));
  end;
end;

procedure TSession.TOptionsProperty.SetBufferSize(ASize: TDataSize);
begin
  curl_easy_setopt(FHandle, CURLOPT_BUFFERSIZE, Longint(ASize.Bytes));
end;

procedure TSession.TOptionsProperty.SetFailOnError(AFailOnError: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_FAILONERROR, Longint(AFailOnError));
end;

procedure TSession.TOptionsProperty.SetPathAsIs(ALeaveIt: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_PATH_AS_IS, Longint(ALeaveIt));
end;

constructor TSession.TOptionsProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TOptionsProperty.Destroy;
begin
  inherited Destroy;
end;

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

procedure TTimeInterval.SetMicroseconds(us: QWord);
begin
  FMicroseconds := us;
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

{ TResponse }

function TResponse.IsOpened: Boolean;
begin
  Result := (session.Opened and hasInfo);
end;

function TResponse.CheckErrors: Boolean;
begin
  Result := not Opened;
end;

function TResponse.GetErrorMessage: string;
begin
  if HasErrors then
  begin
    Result := errorBuffer;
  end;
end;

function TResponse.GetEffectiveUrl: string;
var
  url : PChar;
begin
  if Opened then
  begin
    New(url);
    curl_easy_getinfo(session.FHandle, CURLINFO_EFFECTIVE_URL, @url);
    Result := url;
  end;
end;

function TResponse.GetRedirectUrl: string;
var
  url : PChar;
begin
  if Opened then
  begin
    New(url);
    curl_easy_getinfo(session.FHandle, CURLINFO_REDIRECT_URL, @url);
    Result := url;
  end;
end;

function TResponse.GetContentType: string;
var
  content_type : PChar;
begin
  if Opened then
  begin
    New(content_type);
    curl_easy_getinfo(session.FHandle, CURLINFO_CONTENT_TYPE, @content_type);
    Result := content_type;
  end;
end;

function TResponse.GetPrimaryIP: string;
var
  ip : PChar;
begin
  if Opened then
  begin
    New(ip);
    curl_easy_getinfo(session.FHandle, CURLINFO_PRIMARY_IP, @ip);
    Result := ip;
  end;
end;

function TResponse.GetLocalIP: string;
var
  ip : PChar;
begin
  if Opened then
  begin
    New(ip);
    curl_easy_getinfo(session.FHandle, CURLINFO_LOCAL_IP, @ip);
    Result := ip;
  end;
end;

function TResponse.GetResponseCode: Longint;
var
  code : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_RESPONSE_CODE, @code);
    Result := code;
  end;
end;

function TResponse.GetContent: string;
var
  ContentLength : Longint;
begin
  if Opened then
  begin
    ContentLength := Length(string(PChar(session.FBuffer.Memory)));
    Result := '';
    UniqueString(Result);
    SetLength(Result, ContentLength);
    Move(PChar(session.FBuffer.Memory^), PChar(Result)[0], ContentLength);
  end;
end;

function TResponse.GetVerifySSLResult: boolean;
var
  verify : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_SSL_VERIFYRESULT, @verify);
    Result := (verify = 0);
  end;
end;

function TResponse.GetVerifySSLProxyResult: boolean;
var
  verify : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_PROXY_SSL_VERIFYRESULT, @verify);
    Result := Boolean(verify);
  end;
end;

function TResponse.GetConnectResponseCode: THTTPStatusCode;
var
  code : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_HTTP_CONNECTCODE, @code);
    Result := THTTPStatusCode(code);
  end;
end;

function TResponse.GetHttpVersion: HTTPVersionCode;
var
  ver : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_HTTP_VERSION, @ver);
    Result := HTTPVersionCode(ver);
  end;
end;

function TResponse.GetRedirectCount: Longint;
var
  count : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_REDIRECT_COUNT, @count);
    Result := count;
  end;
end;

function TResponse.GetUploaded: TDataSize;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_SIZE_UPLOAD_T, @bytes);
    Result := TDataSize.Create(bytes);
  end;
end;

function TResponse.GetDownloaded: TDataSize;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_SIZE_DOWNLOAD_T, @bytes);
    Result := TDataSize.Create(bytes);
  end;
end;

function TResponse.GetDownloadSpeed: TDataSize;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_SPEED_DOWNLOAD_T, @bytes);
    Result := TDataSize.Create(bytes);
  end;
end;

function TResponse.GetUploadSpeed: TDataSize;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_SPEED_UPLOAD_T, @bytes);
    Result := TDataSize.Create(bytes);
  end;
end;

function TResponse.GetHeaderSize: TDataSize;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_HEADER_SIZE, @bytes);
    Result := TDataSize.Create(bytes);
  end;
end;

function TResponse.GetRequestSize: TDataSize;
var
  bytes : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_REQUEST_SIZE, @bytes);
    Result := TDataSize.Create(bytes);
  end;
end;

function TResponse.GetContentLengthDownload: LongWord;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T, @bytes);
    Result := bytes;
  end;
end;

function TResponse.GetContentLengthUpload: LongWord;
var
  bytes : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_CONTENT_LENGTH_UPLOAD_T, @bytes);
    Result := bytes;
  end;
end;

function TResponse.GetNumConnects: Longint;
var
  num : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_NUM_CONNECTS, @num);
    Result := num;
  end;
end;

function TResponse.GetPrimaryPort: Longint;
var
  port : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_PRIMARY_PORT, @port);
    Result := port;
  end;
end;

function TResponse.GetLocalPort: Longint;
var
  port : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_LOCAL_PORT, @port);
    Result := port;
  end;
end;

function TResponse.GetFileTime: time_t;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_FILETIME_T, @time);
    Result := time_t(time);
  end;
end;

function TResponse.GetTotalTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_TOTAL_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TResponse.GetNameLookup: TTimeInterval;
var
  time : Longword;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_NAMELOOKUP_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TResponse.GetConnectTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_CONNECT_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TResponse.GetAppConnectTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_APPCONNECT_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TResponse.GetPretransferTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_PRETRANSFER_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TResponse.GetStartTransferTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_STARTTRANSFER_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TResponse.GetRedirectTime: TTimeInterval;
var
  time : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_REDIRECT_TIME_T, @time);
    Result := TTimeInterval.Create(time);
  end;
end;

function TResponse.GetRetryAfterDelay: TTimeInterval;
var
  delay : LongWord;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_RETRY_AFTER, @delay);
    Result := TTimeInterval.Create(Double(delay), tiSeconds);
  end;
end;

function TResponse.GetOsErrno: Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_OS_ERRNO, @Result);
  end;
end;

function TResponse.GetLastSocket: Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_LASTSOCKET, @Result);
  end;
end;

function TResponse.GetActiveSocket: curl_socket_t;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_ACTIVESOCKET, @Result);
  end;
end;

function TResponse.GetFTPEntryPath: string;
var
  path : PChar;
begin
  if Opened then
  begin
    New(path);
    curl_easy_getinfo(session.FHandle, CURLINFO_FTP_ENTRY_PATH, @path);
    Result := path;
  end;
end;

function TResponse.GetConditionUnmet: Boolean;
var
  unmet : Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_CONDITION_UNMET, @unmet);
    Result := Boolean(unmet);
  end;
end;

function TResponse.GetRTSPSessionID: string;
var
  id : PChar;
begin
  if Opened then
  begin
    New(id);
    curl_easy_getinfo(session.FHandle, CURLINFO_RTSP_SESSION_ID, @id);
    Result := id;
  end;
end;

function TResponse.GetRTSPClientCSeq: Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_RTSP_CLIENT_CSEQ, @Result);
  end;
end;

function TResponse.GetRTSPServerCSeq: Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_RTSP_SERVER_CSEQ, @Result);
  end;
end;

function TResponse.GetRTSPReceivedCSeq: Longint;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_RTSP_CSEQ_RECV, @Result);
  end;
end;

function TResponse.GetScheme: string;
var
  sc : PChar;
begin
  if Opened then
  begin
    New(sc);
    curl_easy_getinfo(session.FHandle, CURLINFO_SCHEME, @sc);
    Result := sc;
  end;
end;

constructor TResponse.Create(s: TSession);
begin
  if s.Opened then
  begin
    Self.session := s;
    curl_easy_setopt(session.FHandle, CURLOPT_ERRORBUFFER, PChar(errorBuffer));
    hasInfo := (curl_easy_perform(session.FHandle) = CURLE_OK);
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
  Result := FBuffer.Write(ptr^, size * nmemb);
end;

function TSession.Read(buf: PChar; size: LongWord; nitems: LongWord): LongWord;
var
  dataSize : Int64;
begin
  Result := 0;
  if FBuffer.Size > 0 then
  begin
    dataSize := size * nitems;
    if (dataSize > FBuffer.Size) then
      dataSize := FBuffer.Size;
    Move(PChar(FBuffer.Memory)[FUploadOffset], buf[0], dataSize);
    FUploadOffset := FUploadOffset + dataSize;
    Result := dataSize;
  end;
end;

constructor TSession.Create;
begin
  inherited Create;

  FHandle := curl_easy_init;
  FBuffer := TMemoryStream.Create;
  FUploadOffset := 0;

  FOptions := TOptionsProperty.Create(FHandle);
  FProtocol := TProtocolProperty.Create(FHandle);
  FTCP := TTCPProperty.Create(FHandle);
  FProxy := TProxyProperty.Create(FHandle);
  FDNS := TDNSProperty.Create(FHandle);
  FSecurity := TSecurityProperty.Create(FHandle);
  FHTTP := THTTPProperty.Create(FHandle);
  FIMAP := TIMAPProperty.Create(FHandle);
  FFTP := TFTPProperty.Create(FHandle);

  if Opened then
  begin
    curl_easy_setopt(FHandle, CURLOPT_WRITEDATA, Pointer(Self));
    curl_easy_setopt(FHandle, CURLOPT_WRITEFUNCTION,
      @TSession.WriteFunctionCallback);
    curl_easy_setopt(FHandle, CURLOPT_READDATA, Pointer(Self));
    curl_easy_setopt(FHandle, CURLOPT_READFUNCTION,
      @TSession.ReadFunctionCallback);
    FProtocol.FollowRedirect := True;
  end;
end;

destructor TSession.Destroy;
begin
  curl_easy_cleanup(FHandle);
  FreeAndNil(FBuffer);

  inherited Destroy;
end;

function TSession.IsOpened: Boolean;
begin
  Result := FHandle <> Pointer(0);
end;

procedure TSession.SetUrl(url: string);
begin
  if Opened then
  begin
    FBuffer.Clear;
    curl_easy_setopt(FHandle, CURLOPT_URL, PChar(url));
  end;
end;

procedure TSession.SetLocalPort(APort: Word);
begin
  curl_easy_setopt(FHandle, CURLOPT_LOCALPORT, Longint(APort));
end;

procedure TSession.SetLocalPortRange(ARange: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_LOCALPORTRANGE, ARange);
end;

function TSession.ExtractProtocol(AUrl: string): TProtocol;
var
  proto : TProtocol;
  search, extract, protocolName : string;
begin
  search := LowerCase(Copy(AUrl, 1, Pos('://', AUrl) - 1));

  for proto := Low(TProtocol) to High(TProtocol) do
  begin
    protocolName := GetEnumName(TypeInfo(TProtocol), Ord(proto));
    extract := LowerCase(Copy(protocolName, Length('PROTOCOL_') + 1,
      Length(protocolName) - Length('PROTOCOL_') + 1));
    if extract = search then
      Result := proto;
  end;
end;

initialization
  curl_global_init(CURL_GLOBAL_ALL);

finalization
  curl_global_cleanup;

end.


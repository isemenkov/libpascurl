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

unit curl.session;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.easy, curl.request.method;

type
  TSession = class(TCURLEasy)
  private
    procedure SetPathAsIs (APathAsIs : Boolean);
    procedure SetUrl (AUrl : String);
    
    procedure SetInterfaceName (AInterfaceName : String);
    procedure SetUnixSocketPath (AUnixSocketPath : String);
    procedure SetAbstractUnixSocket (AAbstractUnixSocket : String);
  protected
    constructor Create;

    { Do not handle dot dot sequences. 
      Set the True, to explicitly tell libcurl to not alter the given path 
      before passing it on to the server.
      This instructs libcurl to NOT squash sequences of "/../" or "/./" that may
      exist in the URL's path part and that is supposed to be removed according 
      to RFC 3986 section 5.2.4.
      Some server implementations are known to (erroneously) require the dot dot 
      sequences to remain in the path and some clients want to pass these on in 
      order to try out server implementations.
      By default libcurl will merge such sequences before using the path. }
    property PathAsIs : Boolean write SetPathAsIs default False;

    { Provide the URL to use in the request. 
      Pass in a string to the URL to work with. The parameter should be a string
      which must be URL-encoded in the following format: scheme://host:port/path
      For a greater explanation of the format please see RFC 3986.
      libcurl doesn't validate the syntax or use this variable until the 
      transfer is issued. Even if you set a crazy value here, curl_easy_setopt 
      will still return CURLE_OK.
      If the given URL is missing a scheme name (such as "http://" or "ftp://" 
      etc) then libcurl will make a guess based on the host. If the outermost 
      sub-domain name matches DICT, FTP, IMAP, LDAP, POP3 or SMTP then that 
      protocol will be used, otherwise HTTP will be used. Since 7.45.0 guessing 
      can be disabled by setting a default protocol, see DefaultProtocol for 
      details.
      Should the protocol, either that specified by the scheme or deduced by 
      libcurl from the host name, not be supported by libcurl then 
      CURLE_UNSUPPORTED_PROTOCOL will be returned from either the 
      curl_easy_perform or curl_multi_perform functions when you call them. Use 
      curl_version_info for detailed information of which protocols are 
      supported by the build of libcurl you are using.  
      The host part of the URL contains the address of the server that you want 
      to connect to. This can be the fully qualified domain name of the server, 
      the local network name of the machine on your network or the IP address of 
      the server or machine represented by either an IPv4 or IPv6 address. For 
      example:
      http://www.example.com/
      http://hostname/
      http://192.168.0.1/
      http://[2001:1890:1112:1::20]/
      It is also possible to specify the user name, password and any supported 
      login options as part of the host, for the following protocols, when 
      connecting to servers that require authentication:
      http://user:password@www.example.com
      ftp://user:password@ftp.example.com
      smb://domain%2fuser:password@server.example.com
      imap://user:password;options@mail.example.com
      pop3://user:password;options@mail.example.com
      smtp://user:password;options@mail.example.com 
      At present only IMAP, POP3 and SMTP support login options as part of the 
      host. For more information about the login options in URL syntax please 
      see RFC 2384, RFC 5092 and IETF draft draft-earhart-url-smtp-00.txt 
      (Added in 7.31.0).
      The port is optional and when not specified libcurl will use the default 
      port based on the determined or specified protocol: 80 for HTTP, 21 for 
      FTP and 25 for SMTP, etc. The following examples show how to specify the 
      port:
      http://www.example.com:8080/ - This will connect to a web server using 
      port 8080 rather than 80.
      smtp://mail.example.com:587/ - This will connect to a SMTP server on the 
      alternative mail port. }
    property Url : String write SetUrl;

    { Source interface for outgoing traffic. 
      Pass a string as parameter. This sets the interface name to use as 
      outgoing network interface. The name can be an interface name, an IP 
      address, or a host name.
      If the parameter starts with "if!" then it is treated as only as interface
      name and no attempt will ever be named to do treat it as an IP address or 
      to do name resolution on it. If the parameter starts with "host!" it is 
      treated as either an IP address or a hostname. Hostnames are resolved 
      synchronously. Using the if! format is highly recommended when using the
      multi interfaces to avoid allowing the code to block. If "if!" is 
      specified but the parameter does not match an existing interface,
      CURLE_INTERFACE_FAILED is returned from the libcurl function used to 
      perform the transfer.
      libcurl does not support using network interface names for this option on 
      Windows. }
    property InterfaceName : String write SetInterfaceName;

    { Set Unix domain socket. 
      Enables the use of Unix domain sockets as connection endpoint and sets the
      path to path. If path is NULL, then Unix domain sockets are disabled. An 
      empty string will result in an error at some point, it will not disable 
      use of Unix domain sockets.
      When enabled, curl will connect to the Unix domain socket instead of 
      establishing a TCP connection to a host. Since no TCP connection is 
      created, curl does not need to resolve the DNS hostname in the URL.
      The maximum path length on Cygwin, Linux and Solaris is 107. On other 
      platforms it might be even less. }
    property UnixSocketPath : String write SetUnixSocketPath;

    { Set an abstract Unix domain socket. 
      Enables the use of an abstract Unix domain socket instead of establishing 
      a TCP connection to a host. The parameter should be a string to a 
      null-terminated string holding the path of the socket. The path will be 
      set to path prefixed by a NULL byte (this is the convention for abstract 
      sockets, however it should be stressed that the path passed to this 
      function should not contain a leading NULL).
      On non-supporting platforms, the abstract address will be interpreted as 
      an empty string and fail gracefully, generating a run-time error. }
    property AbstractUnixSocket : String write SetAbstractUnixSocket;
  end;

implementation

{ TSession }

constructor TSession.Create;
begin
  inherited Create;
  FErrorsStack.Push(curl_easy_setopt(FCURL, CURLOPT_ERRORBUFFER,
    FErrorsStack.ErrorBuffer));
  PathAsIs := False;
end;

procedure TSession.SetUrl (AUrl : String);
begin
  FErrorsStack.Push(curl_easy_setopt(FCURL, CURLOPT_URL, PChar(AUrl)));
end;

procedure TSession.SetPathAsIs (APathAsIs : Boolean);
begin
  FErrorsStack.Push(curl_easy_setoptFCURL, CURLOPT_PATH_AS_IS, 
    Longint(APathAsIs));
end;

procedure TSession.SetInterfaceName (AInterfaceName : String);
begin
  FErrorsStack.Push(curl_easy_setopt(FCURL, CURLOPT_INTERFACE,
    PChar(AInterfaceName)));
end;

procedure TSession.SetUnixSocketPath (AUnixSocketPath : String);
begin
  FErrorsStack.Push(curl_easy_setopt(FCURL, CURLOPT_UNIX_SOCKET_PATH,
    PChar(AUnixSocketPath)));
end;

procedure TSession.SetAbstractUnixSocket (AAbstractUnixSocket : String);
begin
  FErrorsStack.Push(curl_easy_setopt(FCURL, CURLOPT_ABSTRACT_UNIX_SOCKET,
    PChar(AAbstractUnixSocket)));
end;

end.
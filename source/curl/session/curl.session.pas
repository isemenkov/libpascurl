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
  libpascurl, curl.session.protocol, curl.utils.errorsstack, 
  curl.request.method;

type
  TSession = class
  private
    FAllowedProtocols : TProtocols;
    FAllowedProtocolRedirects : TProtocols;
    FDefaultProtocol : TProtocol;
    FPathAsIs : Boolean;
    FUrl : String;
    FErrorsStack : curl.utils.errorsstack.TErrorsStack;
    FRequestMethod : curl.request.method.TMethod;
    FCustomRequestMethod : String;
  protected
    constructor Create;
    destructor Destroy; override;

    { Collect CURL errors. }
    property ErrorsStack : TErrorsStack read FErrorsStack;

    { Set allowed protocols. 
      Pass a bitmask of TProtocol defines. If used, this bitmask limits what 
      protocols libcurl may use in the transfer. This allows you to have a 
      libcurl built to support a wide range of protocols but still limit 
      specific transfers to only be allowed to use a subset of them. 
      By default libcurl will accept all protocols it supports. }
    property AllowedProtocols : TProtocols write FAllowedProtocols;

    { Set protocols allowed to redirect to.
      Pass a bitmask of TProtocol defines. If used, this bitmask limits what 
      protocols libcurl may use in a transfer that it follows to in a redirect 
      when CURLOPT_FOLLOWLOCATION is enabled. This allows you to limit specific 
      transfers to only be allowed to use a subset of protocols in redirections.
      Protocols denied by AllowerProtocols are not overridden by this option.
      By default libcurl will allow HTTP, HTTPS, FTP and FTPS on redirect 
      (7.65.2). Older versions of libcurl allowed all protocols on redirect 
      except several disabled for security reasons: Since 7.19.4 FILE and SCP 
      are disabled, and since 7.40.0 SMB and SMBS are also disabled. 
      CURLPROTO_ALL enables all protocols on redirect, including those disabled 
      for security. }
    property AllowedProtocolRedirects : TProtocols 
      write FAllowedProtocolRedirects;
    
    { Default protocol to use if the URL is missing a scheme name. 
      This option tells libcurl to use protocol if the URL is missing a scheme 
      name.  
      An unknown or unsupported protocol causes error CURLE_UNSUPPORTED_PROTOCOL 
      when libcurl parses a schemeless URL. 
      This option does not change the default proxy protocol (http). }
    property DefaultProtocol : TProtocol write FDefaultProtocol;

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
    property PathAsIs : Boolean write FPathAsIs default False;

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
    property Url : String write FUrl;

    { Set request method. }
    property RequestMethod : curl.request.method.TMethod write FRequestMethod;

    { Set custom request method. }
    property CustomRequestMethod : String write FCustomRequestMethod;
  end;

implementation

{ TSession }

constructor TSession.Create;
begin
  FErrorsStack := TErrorsStack.Create;
  PathAsIs := False;
end;

destructor TSession.Destroy;
begin
  FreeAndNil(FErrorsStack);
  inherited Destroy;
end;

end.
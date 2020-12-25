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

unit curl.session;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, curl.easy, container.memorybuffer;

type
  TSession = class(TCURLEasy)
  public
  protected
    { Download / Upload data buffer. }
    FBuffer : TMemoryBuffer;

    { Provide the URL to use in the request. }
    procedure SetUrl (AUrl : String);

    { Get memory buffer download / upload data buffer. }
    function GetMemoryBuffer : PMemoryBuffer;
  protected
    { Get memory buffer download / upload data buffer. }
    property MemoryBuffer : PMemoryBuffer read GetMemoryBuffer;

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
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSession }

constructor TSession.Create;
begin
  inherited Create;
  FBuffer := TMemoryBuffer.Create;
end;

destructor TSession.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

procedure TSession.SetUrl (AUrl : String);
begin
  FErrorsStack.Push(curl_easy_setopt(FCURL, CURLOPT_URL, PChar(AUrl)));
end;

function TSession.GetMemoryBuffer : PMemoryBuffer;
begin
  Result := @FBuffer;
end;

end.

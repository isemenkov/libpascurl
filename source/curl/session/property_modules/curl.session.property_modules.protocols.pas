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

unit curl.session.property_modules.protocols;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.session.property_module, curl.protocol;

type
  TModuleProtocols = class(TPropertyModule)
  protected  
    { Get CURL library protocols bitmask. }
    function GetCURLProtocolsBitmask (AProtocols : TProtocols) : 
      Longint;

    { Set allowed protocols. }
    procedure SetAllowed (AProtocols : TProtocols);

    { Set protocols allowed to redirect to. }
    procedure SetAllowedRedirects (AProtocols : TProtocols);

    { Default protocol to use if the URL is missing a scheme name. }
    procedure SetDefault (AProtocol : TProtocol);

    { Enable CRLF conversion. }
    procedure SetCRLFConversion (AEnable : Boolean);

    { Do the download request without getting the body. }
    procedure SetNoBody (AEnable : Boolean);
  protected
    { Set allowed protocols. 
      Pass a bitmask of TProtocol defines. If used, this bitmask limits what 
      protocols libcurl may use in the transfer. This allows you to have a 
      libcurl built to support a wide range of protocols but still limit 
      specific transfers to only be allowed to use a subset of them. 
      By default libcurl will accept all protocols it supports. }
    property Allowed : TProtocols write SetAllowed;

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
    property AllowedRedirects : TProtocols write SetAllowedRedirects;

    { Default protocol to use if the URL is missing a scheme name. 
      This option tells libcurl to use protocol if the URL is missing a scheme 
      name.  
      An unknown or unsupported protocol causes error CURLE_UNSUPPORTED_PROTOCOL 
      when libcurl parses a schemeless URL. }
    property Default : TProtocol write SetDefault;

    { Enable CRLF conversion.
      If the value is set, libcurl converts Unix newlines to CRLF newlines on 
      transfers. }
    property CRLFConversion : Boolean write SetCRLFConversion;

    { Do the download request without getting the body. 
      Tells libcurl to not include the body-part in the output when doing what 
      would otherwise be a download. }
    property NoBody : Boolean write SetNoBody;
  end;

implementation

{ TModuleProtocols }

function TModuleProtocols.GetCURLProtocolsBitmask (AProtocols : TProtocols) :
  Longint;
begin
  Result := 0;

  if PROTOCOL_DICT   in AProtocols then Result := (Result or CURLPROTO_DICT);
  if PROTOCOL_FILE   in AProtocols then Result := (Result or CURLPROTO_FILE);
  if PROTOCOL_FTP    in AProtocols then Result := (Result or CURLPROTO_FTP);
  if PROTOCOL_FTPS   in AProtocols then Result := (Result or CURLPROTO_FTPS);
  if PROTOCOL_GOPHER in AProtocols then Result := (Result or CURLPROTO_GOPHER);
  if PROTOCOL_HTTP   in AProtocols then Result := (Result or CURLPROTO_HTTP);
  if PROTOCOL_HTTPS  in AProtocols then Result := (Result or CURLPROTO_HTTPS);
  if PROTOCOL_IMAP   in AProtocols then Result := (Result or CURLPROTO_IMAP);
  if PROTOCOL_IMAPS  in AProtocols then Result := (Result or CURLPROTO_IMAPS);
  if PROTOCOL_LDAP   in AProtocols then Result := (Result or CURLPROTO_LDAP);
  if PROTOCOL_LDAPS  in AProtocols then Result := (Result or CURLPROTO_LDAPS);
  if PROTOCOL_POP3   in AProtocols then Result := (Result or CURLPROTO_POP3);
  if PROTOCOL_POP3S  in AProtocols then Result := (Result or CURLPROTO_POP3S);
  if PROTOCOL_RTMP   in AProtocols then Result := (Result or CURLPROTO_RTMP);
  if PROTOCOL_RTMPE  in AProtocols then Result := (Result or CURLPROTO_RTMPE); 
  if PROTOCOL_RTMPS  in AProtocols then Result := (Result or CURLPROTO_RTMPS);
  if PROTOCOL_RTMPT  in AProtocols then Result := (Result or CURLPROTO_RTMPT);
  if PROTOCOL_RTMPTE in AProtocols then Result := (Result or CURLPROTO_RTMPTE);
  if PROTOCOL_RTMPTS in AProtocols then Result := (Result or CURLPROTO_RTMPTS);
  if PROTOCOL_RTSP   in AProtocols then Result := (Result or CURLPROTO_RTSP);
  if PROTOCOL_SCP    in AProtocols then Result := (Result or CURLPROTO_SCP);
  if PROTOCOL_SFTP   in AProtocols then Result := (Result or CURLPROTO_SFTP);
  if PROTOCOL_SMB    in AProtocols then Result := (Result or CURLPROTO_SMB);
  if PROTOCOL_SMBS   in AProtocols then Result := (Result or CURLPROTO_SMBS);
  if PROTOCOL_SMTP   in AProtocols then Result := (Result or CURLPROTO_SMTP);
  if PROTOCOL_SMTPS  in AProtocols then Result := (Result or CURLPROTO_SMTPS);
  if PROTOCOL_TELNET in AProtocols then Result := (Result or CURLPROTO_TELNET);
  if PROTOCOL_TFTP   in AProtocols then Result := (Result or CURLPROTO_TFTP);
end;

procedure TModuleProtocols.SetAllowed (AProtocols : TProtocols);
begin
  Option(CURLOPT_PROTOCOLS, GetCURLProtocolsBitmask(AProtocols));
end;

procedure TModuleProtocols.SetAllowedRedirects (AProtocols : TProtocols);
begin
  Option(CURLOPT_REDIR_PROTOCOLS, GetCURLProtocolsBitmask(AProtocols));
end;

procedure TModuleProtocols.SetDefault (AProtocol : TProtocol);
var
  protocol : String;
begin
  case AProtocol of
    PROTOCOL_DICT   : begin protocol := 'dict';   end;
    PROTOCOL_FILE   : begin protocol := 'file';   end;
    PROTOCOL_FTP    : begin protocol := 'ftp';    end;
    PROTOCOL_FTPS   : begin protocol := 'ftps';   end;
    PROTOCOL_GOPHER : begin protocol := 'gopher'; end;
    PROTOCOL_HTTP   : begin protocol := 'http';   end;
    PROTOCOL_HTTPS  : begin protocol := 'https';  end;
    PROTOCOL_IMAP   : begin protocol := 'imap';   end;
    PROTOCOL_IMAPS  : begin protocol := 'imaps';  end;
    PROTOCOL_LDAP   : begin protocol := 'ldap';   end;
    PROTOCOL_LDAPS  : begin protocol := 'ldaps';  end;
    PROTOCOL_POP3   : begin protocol := 'pop3';   end;
    PROTOCOL_POP3S  : begin protocol := 'pop3s';  end;
    PROTOCOL_RTMP   : begin protocol := 'rtmp';   end;
    PROTOCOL_RTMPE  : begin protocol := 'rtmpe';  end;
    PROTOCOL_RTMPS  : begin protocol := 'rtmps';  end;
    PROTOCOL_RTMPT  : begin protocol := 'rtmpt';  end;
    PROTOCOL_RTMPTE : begin protocol := 'rtmpte'; end;
    PROTOCOL_RTMPTS : begin protocol := 'rtmpts'; end;
    PROTOCOL_RTSP   : begin protocol := 'rtsp';   end;
    PROTOCOL_SCP    : begin protocol := 'scp';    end;
    PROTOCOL_SFTP   : begin protocol := 'sftp';   end;
    PROTOCOL_SMB    : begin protocol := 'smb';    end;
    PROTOCOL_SMBS   : begin protocol := 'smbs';   end;
    PROTOCOL_SMTP   : begin protocol := 'smtp';   end;
    PROTOCOL_SMTPS  : begin protocol := 'smtps';  end;
    PROTOCOL_TELNET : begin protocol := 'telnet'; end;
    PROTOCOL_TFTP   : begin protocol := 'tftp';   end;
  end;

  Option(CURLOPT_DEFAULT_PROTOCOL, protocol);
end;

procedure TModuleProtocols.SetCRLFConversion (AEnable : Boolean);
begin
  Option(CURLOPT_CRLF, AEnable);
end;

procedure TModuleProtocols.SetNoBody (AEnable : Boolean);
begin
  Option(CURLOPT_NOBODY, AEnable);
end;

end.

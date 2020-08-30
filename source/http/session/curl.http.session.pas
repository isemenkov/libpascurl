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

unit curl.http.session;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.session, curl.http.writer, curl.http.content;

type
  THTTP = class
  public
    type
      TSession = class(curl.session.TSession)
      protected
        FWriter : curl.http.writer.TWriter;
        FContent : curl.http.content.TContent;  
      public
        constructor Create;
        destructor Destroy; override;

        { Do not handle dot dot sequences. }
        property PathAsIs; 

        { Provide the URL to use in the request. }
        property Url;  

        { Source interface for outgoing traffic. }
        property InterfaceName; 

        { Set Unix domain socket. }
        property UnixSocketPath;

        { Set an abstract Unix domain socket. }
        property AbstractUnixSocket;

        { Get download data. }
        property Download : curl.http.writer.TWriter read FWriter;

        { Get content. }
        property Content : curl.http.content.TContent read FContent;
      end;
  end;    

implementation

{ THTTP.TSession }

constructor THTTP.TSession.Create;
begin
  FWriter := curl.http.writer.TWriter.Create(FCURL, FErrorsStack);
  FContent := curl.http.content.TContent.Create(@FWriter);
  AllowedProtocols := [PROTOCOL_HTTP, PROTOCOL_HTTPS];
  AllowedProtocolRedirects := [PROTOCOL_HTTP, PROTOCOL_HTTPS];
  DefaultProtocol := [PROTOCOL_HTTPS];
end;

destructor THTTP.TSession.Destroy;
begin
  FreeAndNil(FContent);
  FreeAndNil(FWriter);
  inherited Destroy;
end;

end.
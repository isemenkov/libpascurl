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
  libpascurl, curl.session;

type
  THTTP = class
  public
    type
      TSession = class(curl.session.TSession)
      protected
        
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
      end;
  end;    

implementation

{ THTTP.TSession }

constructor THTTP.TSession.Create;
begin
  AllowedProtocols := [PROTOCOL_HTTP, PROTOCOL_HTTPS];
  AllowedProtocolRedirects := [PROTOCOL_HTTP, PROTOCOL_HTTPS];
  DefaultProtocol := [PROTOCOL_HTTPS];
end;

destructor THTTP.TSession.Destroy;
begin
  inherited Destroy;
end;

end.
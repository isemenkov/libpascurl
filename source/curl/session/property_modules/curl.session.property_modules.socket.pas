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

unit curl.session.property_modules.socket;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, container.memorybuffer, curl.session.property_module;

type
  TModuleSocket = class(TPropertyModule)
  protected
    { Source interface for outgoing traffic. }
    procedure SetInterfaceName (AInterfaceName : String);

    { Set Unix domain socket. }
    procedure SetUnixSocketPath (AUnixSocketPath : String);

    { Set an abstract Unix domain socket. }
    procedure SetAbstractUnixSocket (AAbstractUnixSocket : String);
  protected
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

{ TModuleSocket }

procedure TModuleSocket.SetInterfaceName (AInterfaceName : String);
begin
  Option(CURLOPT_INTERFACE, AInterfaceName);
end;

procedure TModuleSocket.SetUnixSocketPath (AUnixSocketPath : String);
begin
  Option(CURLOPT_UNIX_SOCKET_PATH, AUnixSocketPath);
end;

procedure TModuleSocket.SetAbstractUnixSocket (AAbstractUnixSocket : String);
begin
  Option(CURLOPT_ABSTRACT_UNIX_SOCKET, AAbstractUnixSocket);
end;

end.
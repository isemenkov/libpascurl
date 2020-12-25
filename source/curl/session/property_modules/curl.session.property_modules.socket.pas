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

unit curl.session.property_modules.socket;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.utils.errors_stack, curl.session.property_module;

type
  TModuleSocket = class(TPropertyModule)
  public
    type
      { Set callback for setting socket options. }
      TSocketCreatedFunction = function (Curlfd : curl_socket_t; Purpose :
        curlsocktype) : Integer of object;

      { Set callback for opening sockets. }
      TOpenSocketFunction = function (Purpose : curlsocktype; Address : 
        curl_sockaddr) : curl_socket_t of object;

      { Callback to socket close replacement function. }
      TCloseSocketFunction = function (Item : curl_socket_t) : Boolean
        of object;
  protected
    FSocketCreatedFunction : TSocketCreatedFunction;
    FOpenSocketFunction : TOpenSocketFunction;
    FCloseSocketFunction : TCloseSocketFunction;

    { Source interface for outgoing traffic. }
    procedure SetInterfaceName (AInterfaceName : String);

    { Set Unix domain socket. }
    procedure SetUnixSocketPath (AUnixSocketPath : String);

    { Set an abstract Unix domain socket. }
    procedure SetAbstractUnixSocket (AAbstractUnixSocket : String);

    { Set callback for setting socket options. }
    procedure SetSocketCreatedFunction (ACallback : TSocketCreatedFunction);

    { Set callback for opening sockets. }
    procedure SetSocketOpenFunction (ACallback : TOpenSocketFunction);

    { Callback to socket close replacement function. }
    procedure SetSocketCloseFunction (ACallback : TCloseSocketFunction);
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

    { Set callback for setting socket options. 
      When set, this callback function gets called by libcurl when the socket 
      has been created, but before the connect call to allow applications to 
      change specific socket options. The callback's purpose argument identifies 
      the exact purpose for this particular socket.
      Return CURL_SOCKOPT_OK from the callback on success. Return 
      CURL_SOCKOPT_ERROR from the callback function to signal an unrecoverable 
      error to the library and it will close the socket and return 
      CURLE_COULDNT_CONNECT. Alternatively, the callback function can return 
      CURL_SOCKOPT_ALREADY_CONNECTED, to tell libcurl that the socket is already 
      connected and then libcurl will not attempt to connect it. This allows an 
      application to pass in an already connected socket with 
      TOpenSocketFunction and then have this function make libcurl not attempt 
      to connect (again). }
    property SocketCreatedCallback : TSocketCreatedFunction 
      read FSocketCreatedFunction write SetSocketCreatedFunction;

    { Set callback for opening sockets. 
      This callback function gets called by libcurl instead of the socket call.
      The callback's purpose argument identifies the exact purpose for this 
      particular socket.
      The callback gets the resolved peer address as the address argument and is 
      allowed to modify the address or refuse to connect completely.
      The callback function should return the newly created socket or 
      CURL_SOCKET_BAD in case no connection could be established or another 
      error was detected. Any additional setsockopt(2) calls can of course be 
      done on the socket at the user's discretion. A CURL_SOCKET_BAD return 
      value from the callback function will signal an unrecoverable error to 
      libcurl and it will return CURLE_COULDNT_CONNECT from the function that 
      triggered this callback. This return code can be used for IP address block 
      listing. }
    property OpenSocketCallback : TOpenSocketFunction read FOpenSocketFunction
      write SetSocketOpenFunction;

    { Callback to socket close replacement function.
      This callback function gets called by libcurl instead of the close or 
      closesocket(3) call when sockets are closed (not for any other file 
      descriptors). This is pretty much the reverse to the OpenSocketFunction 
      option. Return True to signal success and False if there was an error. }
    property CloseSocketCallback : TCloseSocketFunction 
      read FCloseSocketFunction write SetSocketCloseFunction;

  private
    class function CreatedSocketFunctionCallback (clientp : Pointer; curlfd :
      curl_socket_t; purpose : curlsocktype) : Integer; static; cdecl;
    class function OpenSocketFunctionCallback (clientp : Pointer; purpose :
      curlsocktype; address : curl_sockaddr) : curl_socket_t; static; cdecl;
    class function CloseSocketFunctionCallback (clientp : Pointer; item :
      curl_socket_t) : Integer; static; cdecl;
  end;

implementation

{ TModuleSocket }

class function TModuleSocket.CreatedSocketFunctionCallback (clientp : Pointer;
  curlfd : curl_socket_t; purpose : curlsocktype) : Integer; cdecl;
begin
  if Assigned(TModuleSocket(clientp).FSocketCreatedFunction) then
  begin
    Result := TModuleSocket(clientp).FSocketCreatedFunction(curlfd, purpose);
  end else
  begin
    Result := CURL_SOCKOPT_ERROR;
  end;
end;

class function TModuleSocket.OpenSocketFunctionCallback (clientp : Pointer;
  purpose : curlsocktype; address : curl_sockaddr) : curl_socket_t; cdecl;
begin
  if Assigned(TModuleSocket(clientp).FOpenSocketFunction) then
  begin
    Result := TModuleSocket(clientp).FOpenSocketFunction(purpose, address);
  end else
  begin
    Result := CURL_SOCKET_BAD;
  end;
end;

class function TModuleSocket.CloseSocketFunctionCallback (clientp : Pointer;
  item : curl_socket_t) : Integer; cdecl;
begin
  if Assigned(TModuleSocket(clientp).FCloseSocketFunction) then
  begin
    Result := Longint(not TModuleSocket(clientp).FCloseSocketFunction(item));
  end else
  begin
    Result := 1;
  end;
end;

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

procedure TModuleSocket.SetSocketCreatedFunction (ACallback : 
  TSocketCreatedFunction);
begin
  FSocketCreatedFunction := ACallback;

  Option(CURLOPT_SOCKOPTDATA, Pointer(Self));
  Option(CURLOPT_SOCKOPTFUNCTION, @TModuleSocket.CreatedSocketFunctionCallback);
end;

procedure TModuleSocket.SetSocketOpenFunction (ACallback : TOpenSocketFunction);
begin
  FOpenSocketFunction := ACallback;

  Option(CURLOPT_OPENSOCKETDATA, Pointer(Self));
  Option(CURLOPT_OPENSOCKETFUNCTION, @TModuleSocket.OpenSocketFunctionCallback);
end;

procedure TModuleSocket.SetSocketCloseFunction (ACallback : 
  TCloseSocketFunction);
begin
  FCloseSocketFunction := ACallback;

  Option(CURLOPT_CLOSESOCKETDATA, Pointer(Self));
  Option(CURLOPT_CLOSESOCKETFUNCTION,
    @TModuleSocket.CloseSocketFunctionCallback);
end;

end.

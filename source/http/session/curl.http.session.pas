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
  SysUtils, curl.session, curl.http.response,
  curl.http.session.property_modules.protocols, 
  curl.http.session.property_modules.writer,
  curl.http.session.property_modules.request,
  curl.http.session.property_modules.options;

type
  THTTP = class
  public
    type
      TResponse = class(curl.http.response.TResponse);
      TSession = class(curl.session.TSession)
      protected
        FWriter : TModuleWriter;
        FProtocols : TModuleProtocols; 
        FRequest : TModuleRequest;
        FOptions : TModuleOptions;
      public
        constructor Create;
        destructor Destroy; override; 

        { Provide access to CURL library error messages storage. }
        property Errors;

        { Provide the URL to use in the request. }
        property Url;  

        { Get download writer object. }
        property Download : TModuleWriter read FWriter;

        { Set options. }
        property Options : TModuleOptions read FOptions;

        { Send current request using GET method. }
        function Get : TResponse;
      end;
  end;    

implementation

uses
  curl.protocol, curl.http.request.method;

{ THTTP.TSession }

constructor THTTP.TSession.Create;
begin
  inherited Create;
  FProtocols := TModuleProtocols.Create(Handle, ErrorsStorage);
  FWriter := TModuleWriter.Create(Handle, ErrorsStorage, MemoryBuffer);
  FRequest := TModuleRequest.Create(Handle, ErrorsStorage);
  FOptions := TModuleOptions.Create(Handle, ErrorsStorage);
  
  FProtocols.Allowed := [PROTOCOL_HTTP, PROTOCOL_HTTPS];
  FProtocols.AllowedRedirects := [PROTOCOL_HTTP, PROTOCOL_HTTPS];
  FProtocols.Default := PROTOCOL_HTTPS;
end;

destructor THTTP.TSession.Destroy;
begin
  FreeAndNil(FProtocols);
  FreeAndNil(FWriter);
  FreeAndNil(FRequest);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

function THTTP.TSession.Get : TResponse;
begin
  FRequest.Method := TMethod.GET;
  Result := TResponse.Create(Handle, ErrorsStorage, MemoryBuffer);
end;

end.

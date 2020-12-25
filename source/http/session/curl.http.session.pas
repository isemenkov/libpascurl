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

unit curl.http.session;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, curl.session, curl.http.response, 
  curl.utils.headers_list,
  curl.http.session.property_modules.protocols, 
  curl.http.session.property_modules.writer,
  curl.http.session.property_modules.request,
  curl.http.session.property_modules.options,
  curl.http.session.property_modules.header,
  curl.http.session.property_modules.redirect,
  curl.http.session.property_modules.dns,
  curl.http.session.property_modules.tcp,
  curl.http.session.property_modules.cookie,
  curl.http.session.property_modules.auth,
  curl.http.session.property_modules.tls_auth,
  curl.http.session.property_modules.http2,
  curl.http.session.property_modules.proxy,
  curl.http.session.property_modules.reader;

type
  THTTP = class
  public
    type
      TResponse = class(curl.http.response.TResponse);
      TSession = class(curl.session.TSession)
      protected
        FHeadersList : THeadersList;

        FWriter : TModuleWriter;
        FHeader : TModuleHeader;
        FProtocols : TModuleProtocols; 
        FRequest : TModuleRequest;
        FOptions : TModuleOptions;
        FRedirect : TModuleRedirect;
        FDNS : TModuleDNS;
        FTCP : TModuleTCP;
        FCookie : TModuleCookie;
        FAuth : TModuleAuth;
        FTLSAuth : TModuleTLSAuth;
        FHTTP2 : TModuleHTTP2;
        FProxy : TModuleProxy;
        FReader : TModuleReader;

        { Request failure on HTTP response >= 400. }
        procedure SetFailOnError (AFail : Boolean);

        { Keep sending on early HTTP response >= 300. }
        procedure SetKeepSendingOnError (AKeepSending : Boolean);
      public
        constructor Create;
        destructor Destroy; override; 

        { Provide access to CURL library error messages storage. }
        property Errors;

        { Provide the URL to use in the request. }
        property Url;  

        { Request failure on HTTP response >= 400.
          Tells the library to fail the request if the HTTP code returned is 
          equal to or larger than 400. The default action would be to return the 
          page normally, ignoring that code. }
        property FailOnError : Boolean write SetFailOnError default False;

        { Keep sending on early HTTP response >= 300.
          Tells the library to keep sending the request body if the HTTP code 
          returned is equal to or larger than 300. The default action would be 
          to stop sending and close the stream or connection. }
        property KeepSendingOnError : Boolean write SetKeepSendingOnError
          default False;

        { Get request options. }
        property Request : TModuleRequest read FRequest; 

        { Get download writer object. }
        property Download : TModuleWriter read FWriter;

        { Get upload reader object. }
        property Upload : TModuleReader read FReader;

        { Set options. }
        property Options : TModuleOptions read FOptions;

        { Set redirect options. }
        property Redirect : TModuleRedirect read FRedirect;

        { Set DNS property. }
        property DNS : TModuleDNS read FDNS;

        { Set TCP properties. }
        property TCP : TModuleTCP read FTCP;

        { Set cookie properties. }
        property Cookie : TModuleCookie read FCookie;

        { Set auth properties. }
        property Auth : TModuleAuth read FAuth;

        { Set TLS auth properties. }
        property TLSAuth : TModuleTLSAuth read FTLSAuth;

        { Set HTTP/2 protocol properties. }
        property HTTP2 : TModuleHTTP2 read FHTTP2;

        { Set proxy properties. }
        property Proxy : TModuleProxy read FProxy;

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
  FHeadersList := THeadersList.Create;

  FProtocols := TModuleProtocols.Create(Handle, ErrorsStorage);
  FWriter := TModuleWriter.Create(Handle, ErrorsStorage, MemoryBuffer);
  FHeader := TModuleHeader.Create(Handle, ErrorsStorage, @FHeadersList);
  FRequest := TModuleRequest.Create(Handle, ErrorsStorage);
  FOptions := TModuleOptions.Create(Handle, ErrorsStorage);
  FRedirect := TModuleRedirect.Create(Handle, ErrorsStorage);
  FDNS := TModuleDNS.Create(Handle, ErrorsStorage);
  FTCP := TModuleTCP.Create(Handle, ErrorsStorage);
  FCookie := TModuleCookie.Create(Handle, ErrorsStorage);
  FAuth := TModuleAuth.Create(Handle, ErrorsStorage);
  FTLSAuth := TModuleTLSAuth.Create(Handle, ErrorsStorage);
  FHTTP2 := TModuleHTTP2.Create(Handle, ErrorsStorage);
  FProxy := TModuleProxy.Create(Handle, ErrorsStorage);
  FReader := TModuleReader.Create(Handle, ErrorsStorage, MemoryBuffer);
  
  FProtocols.Allowed := [PROTOCOL_HTTP, PROTOCOL_HTTPS];
  FProtocols.AllowedRedirects := [PROTOCOL_HTTP, PROTOCOL_HTTPS];
  FProtocols.Default := PROTOCOL_HTTPS;
end;

destructor THTTP.TSession.Destroy;
begin
  FreeAndNil(FHeadersList);
  
  FreeAndNil(FProtocols);
  FreeAndNil(FWriter);
  FreeAndNil(FRequest);
  FreeAndNil(FOptions);
  FreeAndNil(FRedirect);
  FreeAndNil(FDNS);
  FreeAndNil(FTCP);
  FreeAndNil(FCookie);
  FreeAndNil(FAuth);
  FreeAndNil(FTLSAuth);
  FreeAndNil(FHTTP2);
  FreeAndNil(FProxy);
  FreeAndNil(FReader);

  inherited Destroy;
end;

function THTTP.TSession.Get : TResponse;
begin
  FRequest.Method := TMethod.GET;
  FReader.InitUpload;
  
  Result := TResponse.Create(Handle, ErrorsStorage, MemoryBuffer, 
    @FHeadersList);
end;

procedure THTTP.TSession.SetFailOnError (AFail : Boolean);
begin
  FErrorsStack.Push(curl_easy_setopt(Handle, CURLOPT_FAILONERROR,
    Longint(AFail)));
end;

procedure THTTP.TSession.SetKeepSendingOnError (AKeepSending : Boolean);
begin
  FErrorsStack.Push(curl_easy_setopt(Handle, CURLOPT_KEEP_SENDING_ON_ERROR,
    Longint(AKeepSending)));
end;

end.

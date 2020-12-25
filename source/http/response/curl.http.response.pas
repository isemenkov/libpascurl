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

unit curl.http.response;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, container.memorybuffer, curl.utils.errors_stack,
  curl.response, curl.utils.headers_list, curl.utils.cookies_list,
  curl.http.response.property_modules.content,
  curl.http.response.property_modules.timeout,
  curl.http.response.property_modules.redirect,
  curl.http.response.property_modules.header,
  curl.http.response.property_modules.speed,
  curl.http.response.property_modules.request,
  curl.http.response.property_modules.cookie,
  curl.http.response.property_modules.info;

type
  TResponse = class(curl.response.TResponse)
  public
    type
      PHeadersList = ^THeadersList;
      PCookiesList = ^TCookiesList;

  protected
    FHeadersList : PHeadersList;
    FCookiesList : TCookiesList;

    FInfo : TModuleInfo;
    FContent : TModuleContent;
    FTimeout : TModuleTimeout;
    FRedirect : TModuleRedirect;
    FHeader : TModuleHeader;
    FSpeed : TModuleSpeed;
    FRequest : TModuleRequest;
    FCookies : TModuleCookie;

    { Get headers list. }
    function GetHeaders : THeadersList;

    { Get cookies list. }
    function GetCookies : TCookiesList;
  public
    constructor Create (ACURL : libpascurl.CURL; AErrorsStack : PErrorsStack;
      ABuffer : PMemoryBuffer; AHeadersList : PHeadersList);
    destructor Destroy; override;
    
    { Provide access to CURL error messages storage. }
    property Errors;

    { Get headers list. }
    property HeadersList : THeadersList read GetHeaders;

    { Get cookies list. }
    property CookiesList : TCookiesList read GetCookies;

    { Get session info. }
    property Info : TModuleInfo read FInfo;

    { Get headers info. }
    property Header : TModuleHeader read FHeader;

    { Get content data. }
    property Content : TModuleContent read FContent;

    { Get timeouts info. }
    property Timeout : TModuleTimeout read FTimeout;

    { Get redirects info. }
    property Redirect : TModuleRedirect read FRedirect;

    { Get speed info. }
    property Speed : TModuleSpeed read FSpeed;

    { Get request data. }
    property Request : TModuleRequest read FRequest;

    { Get cookies data. }
    property Cookies : TModuleCookie read FCookies;
  end;

implementation

{ TResponse }

constructor TResponse.Create (ACURL : libpascurl.CURL; AErrorsStack :
  PErrorsStack; ABuffer : PMemoryBuffer; AHeadersList : PHeadersList);
begin
  inherited Create(ACURL, AErrorsStack, ABuffer);
  FHeadersList := AHeadersList;
  FCookiesList := TCookiesList.Create;

  FInfo := TModuleInfo.Create(Handle, ErrorsStorage);
  FContent := TModuleContent.Create(Handle, ErrorsStorage, MemoryBuffer);
  FTimeout := TModuleTimeout.Create(Handle, ErrorsStorage);
  FRedirect := TModuleRedirect.Create(Handle, ErrorsStorage);
  FHeader := TModuleHeader.Create(Handle, ErrorsStorage);
  FSpeed := TModuleSpeed.Create(Handle, ErrorsStorage);
  FRequest := TModuleRequest.Create(Handle, ErrorsStorage);
  FCookies := TModuleCookie.Create(Handle, ErrorsStorage, @FCookiesList);
end;

destructor TResponse.Destroy;
begin
  FreeAndNil(FInfo);
  FreeAndNil(FContent);
  FreeAndNil(FTimeout);
  FreeAndNil(FRedirect);
  FreeAndNil(FHeader);
  FreeAndNil(FSpeed);
  FreeAndNil(FRequest);
  FreeAndNil(FCookies);

  FreeAndNil(FCookiesList);

  inherited Destroy;
end;

function TResponse.GetHeaders : THeadersList;
begin
  Result := FHeadersList^;
end;

function TResponse.GetCookies : TCookiesList;
begin
  Result := FCookiesList;
end;

end.

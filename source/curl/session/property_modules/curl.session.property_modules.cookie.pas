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

unit curl.session.property_modules.cookie;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, curl.utils.errors_stack, curl.utils.cookies_list,
  curl.session.property_module;

type
  TModuleCookie = class(TPropertyModule)
  protected
    FCookies : TCookiesList;

    { Start a new cookie session. }
    procedure SetNewSession (AEnable : Boolean);

    { File name to read cookies from. }
    procedure SetStoredFilename (AFilename : String);

    { File name to store cookies to. }
    procedure SetSaveFilename (AFilename : String);
  public
    constructor Create (ACURL : libpascurl.CURL; AErrorsStack : PErrorsStack);
    destructor Destroy; override;
  protected
    { Start a new cookie session. 
      Pass true to mark this as a new cookie "session". It will force libcurl to 
      ignore all cookies it is about to load that are "session cookies" from the 
      previous session. By default, libcurl always stores and loads all cookies, 
      independent if they are session cookies or not. Session cookies are 
      cookies without expiry date and they are meant to be alive and existing 
      for this "session" only. }
    property NewSession : Boolean write SetNewSession default False;

    { File name to read cookies from. 
      It should point to the file name of your file holding cookie data to read. 
      The cookie data can be in either the old Netscape / Mozilla cookie data 
      format or just regular HTTP headers (Set-Cookie style) dumped to a file. 
      It also enables the cookie engine, making libcurl parse and send cookies 
      on subsequent requests with this handle. 
      This option only reads cookies. }
    property StoredFilename : String write SetStoredFilename;

    { File name to store cookies to. 
      This will make libcurl write all internally known cookies to the specified 
      file. If no cookies are known, no file will be created. Specify "-" as 
      filename to instead have the cookies written to stdout. Using this option 
      also enables cookies for this session, so if you for example follow a 
      location it will make matching cookies get sent accordingly. }
    property SaveFilename : String write SetSaveFilename;

    { Cookies list. }
    property Cookies : TCookiesList read FCookies;
  end;

implementation

{ TModuleCookie }

constructor TModuleCookie.Create(ACURL : libpascurl.CURL; AErrorsStack :
  PErrorsStack);
begin
  inherited Create(ACURL, AErrorsStack);

  FCookies := TCookiesList.Create;    
end;

destructor TModuleCookie.Destroy;
begin
  FreeAndNil(FCookies);

  inherited Destroy;
end;

procedure TModuleCookie.SetNewSession (AEnable : Boolean);
begin
  Option(CURLOPT_COOKIESESSION, AEnable);
end;

procedure TModuleCookie.SetStoredFilename (AFilename : String);
begin
  Option(CURLOPT_COOKIEFILE, AFilename);
end;

procedure TModuleCookie.SetSaveFilename (AFilename : String);
begin
  Option(CURLOPT_COOKIEJAR, AFilename);
end;

end.

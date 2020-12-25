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

unit curl.http.session.property_modules.options;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, curl.utils.errors_stack,
  curl.session.property_modules.options,
  curl.http.session.property_modules.socket;

type
  TModuleOptions = class(curl.session.property_modules.options.TModuleOptions)
  protected
    FSocket : TModuleSocket;  

    { Automatically update the referer header. }
    procedure SetAutoReferer (AAutoRefer : Boolean);

    { Ask for HTTP Transfer Encoding. }
    procedure SetTransferEncoding (AEnable : Boolean);

    { Set the HTTP referer header. }
    procedure SetReferer (AReferer : String);

    { Set HTTP user-agent header. }
    procedure SetUserAgent (AUserAgent : String);
  public
    constructor Create (ACURL : libpascurl.CURL; AErrorsStack : PErrorsStack);
    destructor Destroy; override;

    { Do not handle dot dot sequences. }
    property PathAsIs;

    { Automatically update the referer header.
      When enabled, libcurl will automatically set the Referer: header field in 
      HTTP requests where it follows a Location: redirect. }
    property AutoReferer : Boolean write SetAutoReferer default False;

    { Ask for HTTP Transfer Encoding.
      Adds a request for compressed Transfer Encoding in the outgoing HTTP 
      request. If the server supports this and so desires, it can respond with 
      the HTTP response sent using a compressed Transfer-Encoding that will be 
      automatically uncompressed by libcurl on reception. }
    property TransferEncoding : Boolean write SetTransferEncoding default False;

    { Set the HTTP referer header.
      It will be used to set the Referer: header in the http request sent to the 
      remote server. This can be used to fool servers or scripts. }
    property Referer : String write SetReferer;

    { Set HTTP user-agent header.
      It will be used to set the User-Agent: header in the HTTP request sent to 
      the remote server. This can be used to fool servers or scripts. }
    property UserAgent : String write SetUserAgent;

    { Set socket options. }
    property Socket : TModuleSocket read FSocket;
  end;

implementation

{ TModuleOptions }

constructor TModuleOptions.Create (ACURL : libpascurl.CURL; AErrorsStack :
  PErrorsStack);
begin
  inherited Create(ACURL, AErrorsStack);
  FSocket := TModuleSocket.Create(ACURL, AErrorsStack);
end;

destructor TModuleOptions.Destroy;
begin
  FreeAndNil(FSocket);
  inherited Destroy;
end;

procedure TModuleOptions.SetAutoReferer (AAutoRefer : Boolean);
begin
  Option(CURLOPT_AUTOREFERER, AAutoRefer);
end;

procedure TModuleOptions.SetTransferEncoding (AEnable : Boolean);
begin
  Option(CURLOPT_TRANSFER_ENCODING, AEnable);
end;

procedure TModuleOptions.SetReferer (AReferer : String);
begin
  Option(CURLOPT_REFERER, AReferer);
end;

procedure TModuleOptions.SetUserAgent (AUserAgent : String);
begin
  Option(CURLOPT_USERAGENT, AUserAgent);
end;

end.

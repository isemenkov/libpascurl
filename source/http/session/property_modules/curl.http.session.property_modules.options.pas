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

unit curl.http.session.property_modules.options;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.utils.errors_stack,
  curl.session.property_modules.options,
  curl.http.session.property_modules.socket;

type
  TModuleOptions = class(curl.session.property_modules.options.TModuleOptions)
  protected
    FSocket : TModuleSocket;  
  public
    constructor Create (ACURL : CURL; AErrorsStack : PErrorsStack);
    destructor Destroy;

    { Do not handle dot dot sequences. }
    property PathAsIs;

    { Set socket options. }
    property Socket : TModuleSocket read FSocket;
  end;

implementation

{ TModuleOptions }

constructor TModuleOptions.Create (ACURL : CURL; AErrorsStack : PErrorsStack);
begin
  inherited Create(ACURL, AErrorsStack);
  FSocket := TModuleSocket.Create(ACURL, AErrorsStack);
end;

destructor TModuleOptions.Destroy;
begin
  FreeAndNil(FSocket);
  inherited Destroy;
end;

end.
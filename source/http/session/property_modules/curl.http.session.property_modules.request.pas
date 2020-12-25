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

unit curl.http.session.property_modules.request;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.session.property_modules.request, curl.http.request.method;

type
  TModuleRequest = class(curl.session.property_modules.request.TModuleRequest)
  protected
    { Set request method. }
    procedure SetMethod (AMethod : TMethod);

    { Set custom request method. }
    procedure SetCustomMethod (AMethod : String);
  public
    { Set request method. }
    property Method : TMethod write SetMethod;

    { Set custom request method. }
    property CustomMethod : String write SetCustomMethod;

    { Set callback to be called before a new resolve request is started. }
    property ResolverStartCallback;
  end;

implementation

{ TModuleRequest }

procedure TModuleRequest.SetMethod (AMethod : TMethod);
var
  method_str : String;
begin
  case AMethod of
    GET     : begin method_str := 'GET';     end;
    HEAD    : begin method_str := 'HEAD';    end;
    POST    : begin method_str := 'POST';    end;
    PUT     : begin method_str := 'PUT';     end;
    DELETE  : begin method_str := 'DELETE';  end;
    CONNECT : begin method_str := 'CONNECT'; end;
    OPTIONS : begin method_str := 'OPTIONS'; end;
    TRACE   : begin method_str := 'TRACE';   end;
    PATCH   : begin method_str := 'PATCH';   end;
  end;

  Option(CURLOPT_CUSTOMREQUEST, method_str);
end;

procedure TModuleRequest.SetCustomMethod (AMethod : String);
begin
  Option(CURLOPT_CUSTOMREQUEST, AMethod);
end;

end.

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

unit curl.session.property_modules.request;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.session.property_module;

type
  TModuleRequest = class(TPropertyModule)
  public
    type
      TResolverStartFunction = function (resolver_state : Pointer) : Boolean
        of object;
  protected
    FResolverFunction : TResolverStartFunction;

    { Set callback to be called before a new resolve request is started. }
    procedure SetResolverStartFunction (ACallback : TResolverStartFunction);
  protected
    { Set callback to be called before a new resolve request is started.
      This callback function gets called by libcurl every time before a new 
      resolve request is started. 
      resolver_state points to a backend-specific resolver state. Currently only 
      the ares resolver backend has a resolver state. It can be used to set up 
      any desired option on the ares channel before it's used, for example 
      setting up socket callback options. }
    property ResolverStartCallback : TResolverStartFunction 
      read FResolverFunction write SetResolverStartFunction;

  private
    class function ResolverStartFunctionCallback (resolver_state : Pointer;
      reserved : Pointer; userdata : Pointer) : Integer; static; cdecl;
  end;

implementation

{ TModuleRequest }

class function TModuleRequest.ResolverStartFunctionCallback (resolver_state :
  Pointer; reserved : Pointer; userdata : Pointer) : Integer; cdecl;
begin
  if Assigned(TModuleRequest(userdata).FResolverFunction) then
  begin
    Result := Longint(
        not TModuleRequest(userdata).FResolverFunction(resolver_state));
  end else
  begin
    Result := 1;
  end;
end;

procedure TModuleRequest.SetResolverStartFunction (ACallback : 
  TResolverStartFunction);
begin
  FResolverFunction := ACallback;

  Option(CURLOPT_RESOLVER_START_DATA, Pointer(Self));
  Option(CURLOPT_RESOLVER_START_FUNCTION, 
    @TModuleRequest.ResolverStartFunctionCallback);
end;

end.
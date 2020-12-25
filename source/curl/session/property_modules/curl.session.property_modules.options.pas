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

unit curl.session.property_modules.options;

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
  TModuleOptions = class(TPropertyModule)
  protected
    { Do not handle dot dot sequences. }
    procedure SetPathAsIs (APathAsIs : Boolean);
  protected  
    { Do not handle dot dot sequences. 
      Set the True, to explicitly tell libcurl to not alter the given path 
      before passing it on to the server.
      This instructs libcurl to NOT squash sequences of "/../" or "/./" that may
      exist in the URL's path part and that is supposed to be removed according 
      to RFC 3986 section 5.2.4.
      Some server implementations are known to (erroneously) require the dot dot 
      sequences to remain in the path and some clients want to pass these on in 
      order to try out server implementations.
      By default libcurl will merge such sequences before using the path. }
    property PathAsIs : Boolean write SetPathAsIs default False;
  end;

implementation

{ TModuleOptions }

procedure TModuleOptions.SetPathAsIs (APathAsIs : Boolean);
begin
  Option(CURLOPT_PATH_AS_IS, APathAsIs);
end;

end.

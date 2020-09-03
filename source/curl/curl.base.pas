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

unit curl.base;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.utils.errorsstack;

type
  { Base class for all curl classes. }
  TCURLBase = class
  protected
    { CURL library handle. }
    FCURL : CURL;

    { Store CURL library errors messages. }
    FErrorsStack : curl.utils.errorsstack.TErrorsStack;
  protected
    { Initialize new curl easy session. }
    constructor Create;

    { Destroy and clean curl session. }
    destructor Destroy; override;  
  end;

implementation

{ TCURLBase }

constructor TCURLBase.Create;
begin
  FCURL := curl_easy_init;
  FErrorsStack := TErrorsStack.Create;
end;

destructor TCURLBase.Destroy;
begin
  curl_easy_cleanup(FCURL);
  FreeAndNil(FErrorsStack);
  inherited Destroy;
end;

initialization
  curl_global_init(CURL_GLOBAL_ALL);

finalization
  curl_global_cleanup;

end.
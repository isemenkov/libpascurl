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

unit curl.easy;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, curl.utils.errors_stack;

type
  { Base class for all curl classes. }
  TCURLEasy = class
  protected
    { CURL library handle. }
    FCURL : libpascurl.CURL;

    { Store CURL library errors messages. }
    FErrorsStack : TErrorsStack;

    { Return CURL library errors storage. }
    function GetErrors : TErrorsStack;

    { Return pointer to CURL library errors storage. }
    function GetErrorsStorage : PErrorsStack;
  
    { Provide access to CURL handle. }
    property Handle : libpascurl.CURL read FCURL;

    { Provide access to CURL error messages storage. }
    property Errors : TErrorsStack read GetErrors;

    { Provide pointer to CURL error messages storage. }
    property ErrorsStorage : PErrorsStack read GetErrorsStorage;
  public
    { Initialize new curl easy session. }
    constructor Create;

    { Destroy and clean curl session. }
    destructor Destroy; override; 
  end;

implementation

{ TCURLBase }

constructor TCURLEasy.Create;
begin
  FCURL := curl_easy_init;
  FErrorsStack := TErrorsStack.Create;
  
  FErrorsStack.Push(curl_easy_setopt(FCURL, CURLOPT_ERRORBUFFER,
    FErrorsStack.ErrorBuffer));
end;

destructor TCURLEasy.Destroy;
begin
  curl_easy_cleanup(FCURL);
  FreeAndNil(FErrorsStack);
  inherited Destroy;
end;

function TCURLEasy.GetErrors : TErrorsStack;
begin
  Result := FErrorsStack;
end;

function TCURLEasy.GetErrorsStorage : PErrorsStack;
begin
  Result := @FErrorsStack;
end;

initialization
  curl_global_init(CURL_GLOBAL_ALL);

finalization
  curl_global_cleanup;

end.

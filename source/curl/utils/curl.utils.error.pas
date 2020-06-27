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

unit curl.utils.error;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, errorstack;

type
  { Curl errors storage }
  TError = class
  public
    constructor Create;
    destructor Destroy; override;

    { Return TRUE if has erorrs } 
    function HasErrors : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return all errors stack }
    function Errors : TErrorStack;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    FErrorStack : TErrorStack;
  end;
  
  { Curl errors storage enumerator helper }
  TErrorEnumeratorHelper = class helper for TError
    { Get errors enumerator }
    function GetEnumerator : TErrorStack.TErrorsEnumerator;
  end;

implementation

constructor TError.Create;
begin
  FErrorStack := TErrorStack.Create;
end;

destructor TError.Destroy;
begin
  FreeAndNil(FErrorStack);
  inherited Destroy;
end;

function TError.HasErrors : Boolean;
begin
  Result := FErrorStack.Count > 0;
end;

function TError.Errors : TErrorStack;
begin
  Result := FErrorStack;
end;

function TErrorEnumeratorHelper.GetEnumerator : TErrorStack.TErrorsEnumerator;
begin
  Result := FErrorStack.GetEnumerator;
end;

end.
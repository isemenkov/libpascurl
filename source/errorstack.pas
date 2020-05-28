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
(* Module:          Unit 'pascurl'                                            *)
(* Functionality:                                                             *)
(*                                                                            *)
(*                                                                            *)
(*                                                                            *)
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

unit errorstack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, libpascurl;

type
  { TErrorStack }
  { Store CURL functions errors }
  PErrorStack = ^TErrorStack;
  TErrorStack = class
  public
    type
      { Errors enumerator }
      TErrorsEnumerator = class
      protected
        FErrors : TStringList;
        FPosition : Cardinal;
        function GetCurrent : String;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      public
        constructor Create (AErrors : TStringList);
        function MoveNext : Boolean;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        function GetEnumerator : TErrorsEnumerator;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        property Current : String read GetCurrent;
      end;
  public
    { Create new error stack }
    constructor Create;
    destructor Destroy; override;

    { Add error to stack }
    procedure Push (AError : CURLcode);
    { Return top error and remove it from stack }
    function Pop : String;
    { Stack count elements }
    function Count : Cardinal;

    { Get errors enumerator }
    function GetEnumerator : TErrorsEnumerator;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    FErrors : TStringList;
  end;

const
  ErrorsMessages [0 .. CURLcode.CURL_LAST] of String = (
    { CURLE_OK }
    '',

    { CURLE_UNSUPPORTED_PROTOCOL }
    'The URL you passed to libcurl used a protocol that this libcurl does not '+
    'support.',

    { CURLE_FAILED_INIT }
    'Very early initialization code failed.',

    { CURLE_URL_MALFORMAT }
    'The URL was not properly formatted.',

    { CURLE_NOT_BUILT_IN }
    'A requested feature, protocol or option was not found built-in in this '  +
    'libcurl due to a build-time decision.',

    { CURLE_COULDNT_RESOLVE_PROXY }
    'Couldn''t resolve proxy. The given proxy host could not be resolved.'
  );

implementation

{ TErrorStack }

constructor TErrorStack.Create;
begin
  FErrors := TStringList.Create;
end;

destructor TErrorStack.Destroy;
begin
  FreeAndNil(FErrors);
  inherited Destroy;
end;

procedure TErrorStack.Push(ACode: CURLcode);
begin
  if ACode <> CURLE_OK then
  begin
    // TODO
  end;
end;

function TErrorStack.Pop: String;
begin
  if FErrors.Count > 0 then
  begin
    Result := FErrors.Strings[0];
    FList.Delete(0);
  end;
end;

function TErrorStack.Count: Cardinal;
begin
  Result := FErrors.Count;
end;

function TErrorStack.GetEnumerator : TErrorsEnumerator;
begin
  Result := TErrorsEnumerator.Create(FErrors);
end;

{ TErrorStack.TErrorsEnumerator }

constructor TErrorStack.TErrorsEnumerator.Create (AErrors : TStringList);
begin
  FErrors := AErrors;
  FPosition := 0;
end;

function TErrorStack.TErrorsEnumerator.MoveNext : Boolean;
begin
  Result := FPosition < FErrors.Count;
end;

function TErrorStack.TErrorsEnumerator.GetEnumerator : TErrorsEnumerator;
begin
  Result := Self;
end;

function TErrorStack.TErrorsEnumerator.GetCurrent : String;
begin
  Result := FErrors[FPosition];
  Inc(FPosition);
end;

end.


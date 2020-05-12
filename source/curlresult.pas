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
(* Functionality:   Provide TCurlResult  class which contains result value or *)
(*                  error type like in GO lang                                *)
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

unit curlresult;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  { Result error exception }
  ECurlResultException = class (Exception);

  { Contains result value or error type like in GO lang }
  generic TCurlResult<VALUE_TYPE, ERROR_TYPE> = class
  protected
    type
      TErrorType = (
        IMPOSSIBLE_VALUE,
        IMPOSSIBLE_ERROR
      );

      { OnError event callback }
      TOnErrorEvent = procedure (AErrorType : TErrorType) of object;

      PVALUE_TYPE = ^VALUE_TYPE;
      PERROR_TYPE = ^ERROR_TYPE;

      TValue = record
        Ok : Boolean;
        case Boolean of
          True  : (Value : PVALUE_TYPE);
          False : (Error : PERROR_TYPE);
      end;
  protected
    FValue : TValue;
    FOnError : TOnErrorEvent;

    function _Ok : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}
    function _Value : VALUE_TYPE;
      {$IFNDEF DEBUG}inline;{$ENDIF}
    function _Error : ERROR_TYPE;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    constructor Create (AValue : VALUE_TYPE; AError : ERROR_TYPE; AOk :
      Boolean);
    destructor Destroy; override;

    { Check if result is Ok }
    property Ok : Boolean read _Ok;

    { Get value if exists or EYamlResultException }
    property Value : VALUE_TYPE read _Value;

    { Get error if exists or EYamlResultException }
    property Error : ERROR_TYPE read _Error;

    { OnError event callback }
    property OnError : TOnErrorEvent read FOnError write FOnError;
  end;

implementation

{ TCurlResult generic }

constructor TCurlResult.Create (AValue : VALUE_TYPE; AError : ERROR_TYPE;
  AOk : Boolean);
begin
  FValue.Ok := AOk;

  if AOk then
  begin
    New(FValue.Value);
    FValue.Value^ := AValue;
  end else begin
    New(FValue.Error);
    FValue.Error^ := AError;
  end;
end;

destructor TCurlResult.Destroy;
begin
  if FValue.Ok then
  begin
    FreeAndNil(FValue.Value);
  end else
  begin
    FreeAndNil(FValue.Error);
  end;
end;

function TCurlResult._Ok : Boolean;
begin
  Result := FValue.Ok;
end;

function TCurlResult._Value : VALUE_TYPE;
begin
  if FValue.Ok then
  begin
    Result := FValue.Value^;
  end else
  begin
    if Assigned(FOnError) then
      FOnError(IMPOSSIBLE_VALUE)
    else
      raise ECurlResultException.Create('Impossible value');
  end;
end;

function TCurlResult._Error : ERROR_TYPE;
begin
  if not FValue.Ok then
  begin
    Result := FValue.Error^;
  end else
  begin
    if Assigned(FOnError) then
      FOnError(IMPOSSIBLE_ERROR)
    else
      raise ECurlResultException.Create('Impossible error');
  end;
end;

end.


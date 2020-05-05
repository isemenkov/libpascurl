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
(* Functionality:   Contains result value or error type like in GO lang       *)
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

unit result;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  { Result error exception }
  EResultException = class (Exception);

  { Contains result value or error type like in GO lang }
  generic TResult<VALUE_TYPE, ERROR_TYPE> = class
  protected
    type
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
  public
    constructor Create (AValue : VALUE_TYPE; AError : ERROR_TYPE; AOk :
      Boolean);
    destructor Destroy; override;
    function Ok : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Value : VALUE_TYPE; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Error : ERROR_TYPE; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

implementation

{ TResult generic }

constructor TResult.Create (AValue : VALUE_TYPE; AError : ERROR_TYPE;
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

destructor TResult.Destroy;
begin
  if FValue.Ok then
  begin
    FreeAndNil(FValue.Value);
  end else
  begin
    FreeAndNil(FValue.Error);
  end;
end;

function TResult.Ok : Boolean;
begin
  Result := FValue.Ok;
end;

function TResult.Value : VALUE_TYPE;
begin
  if FValue.Ok then
  begin
    Result := FValue.Value^;
  end else
  begin
    raise EResultException.Create('Impossible value');
  end;
end;

function TResult.Error : ERROR_TYPE;
begin
  if not FValue.Ok then
  begin
    Result := FValue.Error^;
  end else
  begin
    raise EResultException.Create('Impossible error');
  end;
end;

end.


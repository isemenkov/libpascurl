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

unit utils.optional;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses    
  SysUtils;

type
  { None value exception }
  TNoneValueException = class(Exception); 

  { Optional class type which can contains some value or none, like Rust lang }
  generic TOptional<T> = class
  public
    { Create new Optional with None type }
    constructor Create;

    { Create new Optional with Some type }
    constructor Create (AValue : T);

    { Return true if optional contains value }
    function IsSome : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return true if optional contains none }
    function IsNone : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return stored value or raise TNoneValueException exeption if none }
    function Unwrap : T;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  protected
    type
      TValue = record
        Ok : Boolean;
        Value : T;
      end;
  protected
    FValue : TValue;
  end;

implementation

{ TOptional generic }

constructor TOptional.Create;
begin
  FValue.Ok := False;
end;

constructor TOptional.Create (AValue : T);
begin
  FValue.Ok := True;
  FValue.Value := AValue;
end;

function TOptional.IsSome : Boolean;
begin
  Result := FValue.Ok;
end;

function TOptional.IsNone : Boolean;
begin
  Result := not FValue.Ok;
end;

function TOptional.Unwrap : T;
begin
  if IsSome then
  begin
    Result := FValue.Value;
  end else 
  begin
    raise TNoneValueException.Create('Value not exists');
  end;
end;

end.


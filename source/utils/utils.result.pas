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

unit utils.result;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils;

type
  { Value not exists exception }
  TValueNotExistsException = class(Exception);  

  { Error not exists exception }
  TErrorNotExistException = class(Exception);  

  { Contains result value or error type like in GO or Rust lang }
  generic TResult<V, E> = class
  public
    { Create new rsult contains value }
    constructor Create (AValue : V);

    { Create new result contains error }
    constructor Create (AError : E);

    { Return true if result contains value }
    function IsOk : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return true if result contains error }
    function IsErr : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return value if not exists raise TValueNotExistsException }
    function Value : V;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return error if not exists raise TErrorNotExistsException }
    function Error : E;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  protected
    type
      TValue = record
        Ok : Boolean;
        case Boolean of
          True  : (Value : record val : V; end;);
          False : (Error : record err : E; end;);
      end;
  protected
    FValue : TValue;
  end;

implementation

{ TResult generic }

constructor TResult.Create (AValue : V);
begin
  FValue.Ok := True;
  FValue.Value.val := AValue;
end;

constructor TResult.Create (AError : E);
begin
  FValue.Ok := False;
  FValue.Error.err := AError;
end;

function TResult.IsOk : Boolean;
begin
  Result := FValue.Ok;
end;

function TResult.IsErr : Boolean;
begin
  Result := not FValue.Ok;
end;

function TResult.Value : V;
begin
  if IsOk then
  begin
    Result := FValue.Value.val;
  end else 
  begin
    raise TValueNotExistsException.Create;
  end;
end;

function TResult.Error : E;
begin
  if IsErr then
  begin
    Result := FValue.Error.err;
  end else 
  begin
    raise TErrorNotExistException.Create;
  end;
end;

end.


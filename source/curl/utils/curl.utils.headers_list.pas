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

unit curl.utils.headers_list;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, container.arraylist, utils.functor, utils.pair;

type
  THeadersList = class
  public
    type
      THeaderKeyValue = {$IFDEF FPC}type specialize{$ENDIF} TPair<String, 
        String>;
  public
    constructor Create;
    destructor Destroy; override;

    { Append a header to the list. }
    procedure Append (AHeader : String);

    { Remove all headers from list. }
    procedure Clear;

    { Return TRUE if header contains in list. }
    function Has (AHeaderKey : String) : Boolean;

    { Return header value. }
    function Value (AHeaderKey : String) : String;
  protected
    type
      THeaders = {$IFDEF FPC}type specialize{$ENDIF} TArrayList<String, 
        TCompareFunctorString>;
  protected
    function Search (AHeaderKey : String) : String;
    function Parse (AHeader : String) : THeaderKeyValue; 
  protected
    FHeaders : THeaders;
  public
    { Return enumerator for in operator. }
    function GetEnumerator : THeaders.TIterator;
  end;

implementation

constructor THeadersList.Create;
begin
  FHeaders := THeaders.Create;
end;

destructor THeadersList.Destroy;
begin
  FreeAndNil(FHeaders);
  inherited Destroy;
end;

function THeadersList.GetEnumerator : THeaders.TIterator;
begin
  Result := FHeaders.GetEnumerator;
end;

function THeadersList.Parse (AHeader : String) : THeaderKeyValue;
var
  position : Integer;
begin
  position := Pos(':', AHeader);
  if position = 0 then
  begin
    position := Pos(' ', AHeader);
  end;

  Result := THeaderKeyValue.Create(Trim(Copy(AHeader, 0, position - 1)),
    Trim(Copy(AHeader, position + 1, Length(AHeader) - position)));
end;

function THeadersList.Search (AHeaderKey : String) : String;
var
  StrValue : String;
  pair : THeaderKeyValue;
begin
  for StrValue in FHeaders do
  begin
    pair := Parse(StrValue);
    if LowerCase(pair.First) = LowerCase(AHeaderKey) then
    begin
      Exit(pair.Second);
    end;
  end; 

  Result := '';  
end;

procedure THeadersList.Append (AHeader : String);
begin
  FHeaders.Append(AHeader);
end;

procedure THeadersList.Clear;
begin
  FHeaders.Clear;
end;

function THeadersList.Has (AHeaderKey : String) : Boolean;
begin
  Result := Search(AHeaderKey) <> '';
end;

function THeadersList.Value (AHeaderKey : String) : String;
begin
  Result := Search(AHeaderKey);
end;

end.

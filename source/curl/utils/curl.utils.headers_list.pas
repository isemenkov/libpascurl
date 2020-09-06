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

unit curl.utils.headers_list;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, container.arraylist, utils.functor, utils.pair;

type
  THeadersList = class
  public
    constructor Create;
    destructor Destroy; override;

    { Append a header to the list. }
    procedure Append (AHeader : String);

    { Remove all headers from list. }
    procedure Clear;

    { Return TRUE if header contains in list. }
    function Has (AHeader : String) : Boolean;

    { Return header value. }
    function Value (AHeaderKey : String) : String;
  protected
    type
      THeaders = specialize TArrayList<String, TCompareFunctorString>;
      THeaderKeyValue = specialize TPair<String, String>;
  protected
    function Search (AHeaderKey : String) : String;
    function Parse (AHeader : String) : THeaderKeyValue; 
  protected
    FHeaders : THeaders;
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

function THeadersList.Parse (AHeader : String) : THeaderKeyValue;
var
  pos : Integer;
begin
  pos := Pos(':', AHeader);
  Result := THeaderKeyValue.Create(Trim(Copy(AHeader, 0, pos)), 
    Trim(Copy(AHeader, pos, Length(AHeader) - pos)));  
end;

function THeadersList.Search (AHeaderKey : String) : String;
var
  Iterator : THeaders.TIterator;
  pair : THeaderKeyValue;
begin
  for Iterator in FHeaders do
  begin
    pair := Parse(Iterator.Value);
    if pair.First = AHeaderKey then
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

function THeadersList.Has (AHeader : String) : Boolean;
begin
  Result := Search(AHeader) <> '';
end;

function THeadersList.Value (AHeaderKey : String) : String;
begin
  Result := Search(AHeaderKey);
end;

end.
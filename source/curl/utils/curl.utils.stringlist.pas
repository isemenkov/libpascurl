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

unit curl.utils.stringlist;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, utils.optional;

type
  { CURL library string list }
  TStringList = class
  public
    type
      TOptionalString = {$IFDEF FPC}type specialize{$ENDIF} TOptional<String>;
  private
    FList : pcurl_slist;
    FNext : pcurl_slist;
    FEnd : Boolean;
  public
    constructor Create;
    constructor Create (AList : pcurl_slist);
    destructor Destroy; override;

    { Return list next element }
    function Next : TOptionalString;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return TRUE if list if empty }
    function IsEmpty : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Add new string to list }
    procedure Add (AItem : String);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Clear string list }
    procedure Clear;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Convert TStringList to CURL pcurl_slist }
    property ToCurlList : pcurl_slist read FList;
  end;

  { Helper which add enumerator to TStringList class }
  TStringListEnumeratorHelper = class helper for TStringList 
  public
    type
      { TStringList enumerator }
      TStringListEnumerator = class 
      protected
        function GetCurrent : String;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      public
        constructor Create (AList : TStringList);

        function MoveNext : Boolean;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        function GetEnumerator : TStringListEnumerator;
          {$IFNDEF DEBUG}inline;{$ENDIF}
          
        property Current : String read GetCurrent;
      private
        FList : TStringList;
        FCurrent : TStringList.TOptionalString;
      end;
  public
    function GetEnumerator : TStringListEnumerator;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

implementation

{ TStringList }

constructor TStringList.Create;
begin
  FList := nil;
  FNext := nil;
  FEnd := False;
end;

constructor TStringList.Create (AList : pcurl_slist);
begin
  FList := AList;
  FNext := AList;
  FEnd := False;
end;

destructor TStringList.Destroy;
begin
  curl_slist_free_all(FList);
  FNext := nil;
  inherited Destroy;
end;

function TStringList.Next : TOptionalString;
begin
  if (FList <> nil) and (FNext = nil) and (not FEnd) then
  begin
    Result := TOptionalString.Create(FList^.data);
    FNext := FList^.next;
  end else if FNext <> nil then
  begin
    Result := TOptionalString.Create(FNext^.data);
    FNext := FNext^.next;
    if FNext = nil then
    begin
      FEnd := True;
    end;
    Exit;
  end else
  begin
    Result := TOptionalString.Create;
    FEnd := False;
  end;
end;

function TStringList.IsEmpty : Boolean;
begin
  Result := (FList = nil);
end;

procedure TStringList.Add (AItem : String);
begin
  FList := curl_slist_append(FList, PChar(AItem));
end;

procedure TStringList.Clear;
begin
  curl_slist_free_all(FList);
  FList := nil;
  FNext := nil;
end;

{ TStringListEnumeratorHelper }

function TStringListEnumeratorHelper.GetEnumerator : TStringListEnumerator;
begin
  Result := TStringListEnumerator.Create(Self);
end;

{ TStringListEnumeratorHelper.TStringListEnumerator }

constructor TStringListEnumeratorHelper.TStringListEnumerator.Create (AList :
  TStringList);
begin
  FList := AList;
end;

function TStringListEnumeratorHelper.TStringListEnumerator.GetCurrent : String;
begin
  Result := FCurrent.Unwrap;
end;

function TStringListEnumeratorHelper.TStringListEnumerator.MoveNext : Boolean;
begin
  FCurrent := FList.Next;
  Result := FCurrent.IsSome;
end;

function TStringListEnumeratorHelper.TStringListEnumerator.GetEnumerator :
  TStringListEnumerator;
begin
  Result := TStringListEnumerator.Create(FList);
end;

end.


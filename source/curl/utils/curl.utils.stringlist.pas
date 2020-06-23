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

unit curl.utils.stringlist;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl;

type
  { CURL library string list }
  TStringList = class
  public
    constructor Create;
    constructor Create (AList : pcurl_slist);
    destructor Destroy; override;

    { Return list first item }
    function First : String;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return list next element }
    function Next : String;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return TRUE if list if empty }
    function IsEmpty : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Add new string to list }
    procedure Add (AItem : String);
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Convert TStringList to CURL pcurl_slist }
    property ToCurlList : pcurl_slist read FList;
  private
    FList : pcurl_slist;
    FNext : pcurl_slist;
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
        destructor Destroy; override;

        function MoveNext : Boolean;
          {$IFNDEF DEBUG}inline;{$ENDIF}

        function GetEnumerator : TStringListEnumerator;
          {$IFNDEF DEBUG}inline;{$ENDIF}
          
        property Current : String read GetCurrent;
      private
        FList : TStringList;
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
end;

constructor TStringList.Create (AList : pcurl_slist);
begin
  FList := AList;
  FNext := AList;
end;

destructor TStringList.Destroy;
begin
  curl_slist_free_all(FList);
  FNext := nil;
  inherited Destroy;
end;

function TStringList.First : String;
begin
  if FList <> nil then
  begin
    Result := FList^.data;
    FNext := FList^.next;
  end else 
  begin
    Result := '';
  end;
end;

function TStringList.Next : String;
begin
  if FNext <> nil then
  begin
    Result := FNext^.data;
    FNext := FNext^.next;
    Exit;
  end else
  begin
    Result := '';  
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

{ TStringListEnumeratorHelper }

function TStringListEnumeratorHelper.GetEnumerator : TStringListEnumerator;
begin
  Result := TStringListEnumerator.Create(Self);
end;

{ TStringListEnumeratorHelper.TStringListEnumerator }

constructor TStringListEnumeratorHelper.TStringListEnumerator.Create (AErrors :
  TStringList);
begin
  FList := AErrors;
end;

function TStringListEnumeratorHelper.TStringListEnumerator.GetCurrent : String;
begin
  Result := FList.Flist^.data;
end;

function TStringListEnumeratorHelper.TStringListEnumerator.MoveNext : Boolean;
begin
  Result := (FList.FList^.next <> nil);
end;

function TStringListEnumeratorHelper.TStringListEnumerator.GetEnumerator :
  TStringListEnumerator;
begin
  Result := TStringListEnumerator.Create(FList);
end;

end.


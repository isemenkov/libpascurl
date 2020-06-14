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

unit curlstringlist;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils, libpascurl;

type
  { CURL library string list wrapper }
  TCurlStringList = class
  public
    type
      { String list enumerator }
      TCurlStringListEnumerator = class
      protected
        FList : pcurl_slist;
        FPosition : pcurl_slist;

        function GetCurrent : String;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      public
        constructor Create (AList : pcurl_slist);
        function MoveNext : Boolean;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        function GetEnumerator : TCurlStringListEnumerator;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        property Current : String read GetCurrent;
      end;
  public
    constructor Create;
    destructor Destroy; override;

    { Return list first item }
    function First : String;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return next list element }
    function Next : String;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return TRUE if list if empty }
    function IsEmpty : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Add new string to list }
    function Add (AItem : String) : TCurlStringList;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    FList : pcurl_slist;
    FCurrent : pcurl_slist;
  end;

implementation

{ TCurlStringList }

constructor TCurlStringList.Create;
begin
  FList := nil;
  FCurrent := nil;
end;

destructor TCurlStringList.Destroy;
begin
  curl_slist_free_all(FList);
  FCurrent := nil;
  inherited Destroy;
end;

function TCurlStringList.First : String;
begin
  if FList <> nil then
  begin
    FCurrent := FList;
    Result := FCurrent^.data;
    Exit;
  end;
  Result := '';
end;

function TCurlStringList.Next : String;
begin
  if FCurrent <> nil then
  begin
    FCurrent := FCurrent^.next;
    if FCurrent <> nil then
    begin
      Result := FCurrent^.data;
      Exit;
    end;
    Result := '';
    Exit;
  end;
  Result := '';
end;

function TCurlStringList.IsEmpty : Boolean;
begin
  Result := (FList = nil);
end;

function TCurlStringList.Add (AItem : String) : TCurlStringList;
begin
  FList := curl_slist_append(FList, PChar(AItem));
  Result := Self;
end;

{ TCurlStringList.TCurlStringListEnumerator }

constructor TCurlStringList.TCurlStringListEnumerator (AList : pcurl_slist);
begin
  FList := AList;
  FPosition := AList;
end;

function TCurlStringList.TCurlStringListEnumerator.GetCurrent : String;
begin
  Result := FPosition^.data;
end;

function TCurlStringList.TCurlStringListEnumerator.MoveNext : Boolean;
begin
  FPosition := FPosition^.next;
  Result := (FPosition <> nil);
end;

function TCurlStringList.TCurlStringListEnumerator.GetEnumerator :
  TCurlStringListEnumerator;
begin
  Result := TCurlStringListEnumerator.Create(FList);
end;

end.


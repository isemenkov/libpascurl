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
  { CURL library string list }
  TCurlStringList = class
  public
    constructor Create;
    constructor Create (AList : pcurl_slist);
    destructor Destroy; override;

    { Return list first item }
    function First : String;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Return TRUE if list if empty }
    function IsEmpty : Boolean;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Add new string to list }
    function Add (AItem : String) : TCurlStringList;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    FList : pcurl_slist;
    FNext : pcurl_slist;
  end;

implementation

constructor TCurlStringList.Create;
begin
  FList := nil;
  FNext := nil;
end;

constructor TCurlStringList.Create (AList : pcurl_slist);
begin
  FList := AList;
  FNext := AList;
end;

destructor TCurlStringList.Destroy;
begin
  curl_slist_free_all(FList);
  FNext := nil;
  inherited Destroy;
end;

function TCurlStringList.First : String;
begin
  if FList <> nil then
  begin
    Result := FList^.data;
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

end.


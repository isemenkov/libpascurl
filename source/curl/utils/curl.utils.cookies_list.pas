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

unit curl.utils.cookies_list;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, container.arraylist, utils.functor, utils.pair;

type
  TCookiesList = class
  public
    type
      TCookieKeyValue = {$IFDEF FPC}type specialize{$ENDIF} TPair<String,
        String>;
  public
    constructor Create;
    destructor Destroy; override;

    { Append a cookie to the list. }
    procedure Append (ACookie : String); overload;

    { Append a curl library cookies list. }
    procedure Append (ACookies : pcurl_slist); overload;

    { Remove all cookies from list. }
    procedure Clear;

    { Return TRUE if cookie contains in list. }
    function Has (ACookieKey : String) : Boolean;

    { Return cookie value. }
    function Value (ACookieKey : String) : String;

    { Return formatted cookie string. }
    function FormatCookie : String;
  protected
    type
      TCookies = {$IFDEF FPC}specialize{$ENDIF} TArrayList<String,
        TCompareFunctorString>;
  protected
    function Search (ACookieKey : String) : String;
    function Parse (ACookie : String) : TCookieKeyValue; 
  protected
    FCookies : TCookies;
  public
    { Return enumerator for in operator. }
    function GetEnumerator : TCookies.TIterator;
  end;

implementation

constructor TCookiesList.Create;
begin
  FCookies := TCookies.Create;
end;

destructor TCookiesList.Destroy;
begin
  FreeAndNil(FCookies);
  inherited Destroy;
end;

function TCookiesList.GetEnumerator : TCookies.TIterator;
begin
  Result := FCookies.GetEnumerator;
end;

function TCookiesList.Parse (ACookie : String) : TCookieKeyValue;
var
  position : Integer;
begin
  position := Pos('=', ACookie);
  
  Result := TCookieKeyValue.Create(Trim(Copy(ACookie, 0, position - 1)),
    Trim(Copy(ACookie, position + 1, Length(ACookie) - position)));
end;

function TCookiesList.Search (ACookieKey : String) : String;
var
  StrValue : String;
  pair : TCookieKeyValue;
begin
  for StrValue in FCookies do
  begin
    pair := Parse(StrValue);
    if LowerCase(pair.First) = LowerCase(ACookieKey) then
    begin
      Exit(pair.Second);
    end;
  end; 

  Result := '';  
end;

procedure TCookiesList.Append (ACookie : String);
begin
  FCookies.Append(ACookie);
end;

procedure TCookiesList.Append (ACookies : pcurl_slist);
var
  head : pcurl_slist;
begin
  head := ACookies;

  while ACookies <> nil do
  begin
    Append(ACookies^.data);    
    ACookies := ACookies^.next;
  end;

  curl_slist_free_all(head);
end;

procedure TCookiesList.Clear;
begin
  FCookies.Clear;
end;

function TCookiesList.Has (ACookieKey : String) : Boolean;
begin
  Result := Search(ACookieKey) <> '';
end;

function TCookiesList.Value (ACookieKey : String) : String;
begin
  Result := Search(ACookieKey);
end;

function TCookiesList.FormatCookie : String;
var
  CookieStr : String;
begin
  Result := '';

  for CookieStr in FCookies do
  begin
    Result := Result + CookieStr + ';';
  end;  
end;

end.

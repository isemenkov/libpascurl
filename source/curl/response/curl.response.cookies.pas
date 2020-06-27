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

unit curl.response.cookies;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  curl.utils.errorstack, libpascurl;

type
  { Get all known cookies }
  TCookies = class    
  private
    constructor Create (ACurl : CURL; AErrors : PErrorStack);        
  private
    FCurl : CURL;
    FErrors : PErrorStack;
    FList : TCurlStringList;
  end;
  
  TCookiesEnumeratorHelper = class helper for TCookies
  public
    type
      { Cookies enumerator }
      TCookiesEnumerator = class
      protected
        
        function GetCurrent : String;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      public
        constructor Create;
        function MoveNext : Boolean;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        function GetEnumerator : TCookiesEnumerator;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        property Current : String read GetCurrent;
      end;
    public
      function GetEnumerator : TCookiesEnumerator;
        {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

implementation

{ TCookies }

constructor TCookies.Create (ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

{ TCookiesEnumeratorHelper.TCookiesEnumerator }

constructor TCookiesEnumeratorHelper.TCookiesEnumerator.Create;
begin
  // TODO
end;

function TCookiesEnumeratorHelper.TCookiesEnumerator.GetCurrent : String;
begin
  // TODO
end;

function TCookiesEnumeratorHelper.TCookiesEnumerator.MoveNext : Boolean;
begin
  // TODO
end;

function TCookiesEnumeratorHelper.TCookiesEnumerator.GetEnumerator : 
  TCookiesEnumerator;
begin
  Result := TCookiesEnumerator.Create;
end;

{ TCookiesEnumeratorHelper }

function TCookiesEnumeratorHelper.GetEnumerator : TCookiesEnumerator;
begin
  Result := TCookiesEnumerator.Create;
end;

end.
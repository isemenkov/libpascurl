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
(* Functionality:   Provides THTTPResponse class                              *)
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

unit httpresponse;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils, libpascurl, curlresult, timeinterval, datasize, errorstack,
  httpstatuscode;

type
  { HTTP(S) session result response data }
  THTTPResponse = class
  public
    type
      { HTTP(S) session errors }
      TError = class
      public
        { Return TRUE if has errors }
        function HasErrors : Boolean;
          {$IFNDEF DEBUG}inline;{$ENDIF}
        //function ErrorMessage : String;
        //  {$IFNDEF DEBUG}inline;{$ENDIF}
        //function OsErrno : Longint;
        //  {$IFNDEF DEBUG}inline;{$ENDIF}
        function Errors : TErrorStack;
          {$IFNDEF DEBUG}inline;{$ENDIF}
      private
        constructor Create;
        destructor Destroy;
      private
        FErrorStack : TErrorStack;
        FErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
      end;

      TRedirect = class
      public
        function IsRedirected : Boolean;
        function Count : Longint;
        function Url : String;
      private
        constructor Create;
      private
        FCurl : CURL;
      end;
  private
    FError : TError;
  end;

implementation

{ THTTPResponse.TError }

constructor THTTPResponse.TError.Create;
begin
  FErrorStack := TErrorStack.Create;
end;

destructor THTTPResponse.TError.Destroy;
begin
  FreeAndNil(FErrorStack);
end;

function THTTPResponse.TError.HasErrors : Boolean;
begin
  Result := FErrorStack.Count > 0;
end;

function THTTPResponse.TError.Errors : TErrorStack;
begin
  Result := FErrorStack;
end;

{ THTTPResponse.TRedirect }

constructor THTTPResponse.TRedirect.Create;
begin

end;

function THTTPResponse.TRedirect.IsRedirected : Boolean;
begin

end;

function THTTPResponse.TRedirect.Count : Longint;
begin

end;

function THTTPResponse.TRedirect.Url : String;
begin

end;

end.


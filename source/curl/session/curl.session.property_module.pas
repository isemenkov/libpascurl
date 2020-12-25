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

unit curl.session.property_module;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, curl.utils.errors_stack;

type
  { Base class for all curl.session property modules classes. }
  TPropertyModule = class
  public
    { Property module constructor. }
    constructor Create (ACURL : libpascurl.CURL; AErrorsStack : PErrorsStack);
  protected
    { CURL library handle. }
    FCURL : libpascurl.CURL;

    { Errors messages stores. } 
    FErrorsStack : PErrorsStack;
  
    { Set CURL library option. }
    procedure Option (ACURLOption : CURLoption; AValue : Boolean); overload;
    procedure Option (ACURLOption : CURLoption; AValue : Longint); overload;
    procedure Option (ACURLOption : CURLoption; AValue : Int64); overload;
    procedure Option (ACURLOption : CURLoption; AValue : String); overload;
    procedure Option (ACURLOption : CURLoption; AValue : Pointer); overload;
  end;

implementation

{ TPropertyModule }

constructor TPropertyModule.Create (ACURL : libpascurl.CURL; AErrorsStack :
  PErrorsStack);
begin
  FCURL := ACURL;
  FErrorsStack := AErrorsStack;
end;

procedure TPropertyModule.Option (ACURLOption : CURLoption; AValue : Longint);
begin
  FErrorsStack^.Push(curl_easy_setopt(FCURL, ACURLOption, AValue));
end;

procedure TPropertyModule.Option (ACURLOption : CURLoption; AValue : String);
begin
  FErrorsStack^.Push(curl_easy_setopt(FCURL, ACURLOption, PChar(AValue)));
end;

procedure TPropertyModule.Option (ACURLOption : CURLoption; AValue : Pointer);
begin
  FErrorsStack^.Push(curl_easy_setopt(FCURL, ACURLOption, AValue));
end;

procedure TPropertyModule.Option (ACURLOption : CURLoption; AValue : Int64);
begin
  FErrorsStack^.Push(curl_easy_setopt(FCURL, ACURLOption, AValue));
end;

procedure TPropertyModule.Option (ACURLOption : CURLoption; AValue : Boolean);
begin
  FErrorsStack^.Push(curl_easy_setopt(FCURL, ACURLOption, Longint(AValue)));
end;

end.

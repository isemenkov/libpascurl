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

unit curl.http.content;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, libpascurl, curl.http.writer;

type
  TContent = class
  protected
    FWriter : curl.http.writer.PWriter;
  
  protected
    constructor Create (AWriter : curl.http.writer.PWriter);

    function GetAsString : String;
    function GetAsData : PByte;  
  public
    { Get content data as string. }
    property AsString : String read GetAsString;

    { Get content data as pointer to byte. }
    property AsData : PByte read GetAsData;
  end;

implementation

constructor TContent.Create (AWriter : curl.http.writer.PWriter);
begin
  FWriter := AWriter;
end;

function TContent.GetAsString : String;
var
  Stream : TStringStream;
begin
  Stream := TStringStream.Create('');
  Stream.Write(FWriter^.GetBufferData, FWriter^.GetBufferDataSize);
  Result := Stream.DataString;
  UniqueString(Result);
  FreeAndNil(Stream);
end;

function TContent.GetAsData : PByte;
begin
  Result := PByte(FWriter^.GetBufferData);
end;

end.
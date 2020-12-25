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

unit curl.http.response.property_modules.content;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils, libpascurl, utils.datasize, container.memorybuffer, 
  curl.response.property_modules.content;

type
  TModuleContent = class(curl.response.property_modules.content.TModuleContent)
  protected
    FContent : String;

    { Get Content-Type }
    function GetContentType : String;

    { Get content length. }
    function GetContentLength : TDataSize;

    { Get content as string. }
    function GetString : String;
  public
    { Get content buffer length. }
    property BufferLength;

    { Get the number of downloaded bytes. }
    property DownloadedSize;

    { Get Content-Type
      This is the value read from the Content-Type: field. If you get empty,
      it means that the server didn't send a valid Content-Type header. }
    property ContentType : String read GetContentType;

    { Get content-length of content data. }
    property ContentLength : TDataSize read GetContentLength;

    { Get content as string value. }
    property ToString : String read GetString;

    { Get content as memory buffer. }
    property ToBytes : TMemoryBuffer read GetBuffer;
  end;  

implementation

{ TModuleContent }

function TModuleContent.GetContentType : String;
begin
  Result := GetStringValue(CURLINFO_CONTENT_TYPE);
end;

function TModuleContent.GetContentLength : TDataSize;
begin
  Result := TDataSize.Create;
  Result.Bytes := GetInt64Value(CURLINFO_CONTENT_LENGTH_DOWNLOAD,
    CURLINFO_CONTENT_LENGTH_DOWNLOAD_T);
end;

function TModuleContent.GetString : String;
begin
  if FBuffer^.IsEmpty then
  begin
    Exit('');
  end;

  Result := PChar(FBuffer^.GetBufferData);
end;

end.

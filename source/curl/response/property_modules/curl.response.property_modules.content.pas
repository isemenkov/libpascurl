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

unit curl.response.property_modules.content;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, container.memorybuffer, curl.utils.errors_stack, utils.datasize,
  curl.response.property_module;

type
  TModuleContent = class(TPropertyModule)
  protected
    FBuffer : PMemoryBuffer;

    { Get content buffer length. }
    function GetBufferLength : TDataSize;

    { Get the number of downloaded bytes. }
    function GetDownloadedSize : TDataSize;

    { Get content as memory buffer. }
    function GetBuffer : TMemoryBuffer;
  protected
    { Get content as memory buffer. }
    property Buffer : TMemoryBuffer read GetBuffer;

    { Get content buffer length. }
    property BufferLength : TDataSize read GetBufferLength;

    { Get the number of downloaded bytes. }
    property DownloadedSize : TDataSize read GetDownloadedSize;
  public
    constructor Create (ACURL : libpascurl.CURL; AErrorsStack : PErrorsStack;
      ABuffer : PMemoryBuffer);
  end;

implementation

{ TModuleContent }

constructor TModuleContent.Create(ACURL : libpascurl.CURL; AErrorsStack : 
  PErrorsStack; ABuffer : PMemoryBuffer);
begin
  inherited Create(ACURL, AErrorsStack);
  FBuffer := ABuffer;
end;

function TModuleContent.GetBuffer : TMemoryBuffer;
begin
  Result := FBuffer^;
end;

function TModuleContent.GetBufferLength : TDataSize;
begin
  Result := TDataSize.Create;
  Result.Bytes := FBuffer^.GetBufferDataSize;
end;

function TModuleContent.GetDownloadedSize : TDataSize;
begin
  Result := TDataSize.Create;
  Result.Bytes := GetInt64Value(CURLINFO_SIZE_DOWNLOAD, 
    CURLINFO_SIZE_DOWNLOAD_T);
end;

end.

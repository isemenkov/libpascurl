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

unit curl.response.speed;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  curl.utils.errorstack, utils.datasize, libpascurl;

type
  { Response speed data }
  TSpeed = class
  public
    { Get download speed per second }
    function Download : TDataSize;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Get upload speed per second }
    function Upload : TDataSize;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    constructor Create (ACurl : CURL; AErrors : PErrorStack);
  private
    FCurl : CURL;
    FErrors : PErrorStack;
  end;  

implementation

{ TSpeed }

constructor TSpeed.Create (ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

function TSpeed.Download : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_SPEED_DOWNLOAD_T, @bytes);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_SPEED_DOWNLOAD, @dbytes);
    Result.Bytes := ceil(dbytes);
  end;

  FErrors^.Push(CurlResult);
end;

function TSpeed.Upload : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_SPEED_UPLOAD_T, @bytes);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_SPEED_UPLOAD, @dbytes);
    Result.Bytes := ceil(dbytes);
  end;

  FErrors^.Push(CurlResult);
end;

end.
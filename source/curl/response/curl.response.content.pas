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

unit curl.response.content;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, curl.utils.errorstack, utils.datasize, libpascurl;

type
  { Content data }
  TContent = class
  public
    { Get Content-Type
    This is the value read from the Content-Type: field. If you get empty,
    it means that the server didn't send a valid Content-Type header. }
    function ContentType : String;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Get content-length of download }
    function Length : TDataSize;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Get content as string }
    function ToString : String;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    { Get content as bytes array }
    function ToBytes : TMemoryStream;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    constructor Create (ACurl : CURL; AErrors : PErrorStack);
  private
    FCurl : CURL;
    FErrors : PErrorStack;
  end;

implementation


{ TContent }

constructor TContent.Create(ACurl : CURL; AErrors : PErrorStack);
begin
  FCurl := ACurl;
  FErrors := AErrors;
end;

function TContent.ContentType : String;
var
  ctype : PChar;
begin
  New(ctype);
  ctype := '';
  FErrors^.Push(curl_easy_getinfo(FCurl, CURLINFO_CONTENT_TYPE, @ctype));
  Result := ctype;
end;

function TContent.Length : TDataSize;
var
  size : Longword = 0;
  dsize : Double = 0;
begin
  CurlResult := curl_easy_getinfo(FCurl, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T,
    @size);
  Result := TDataSize.Create;
  Result.Bytes := size;

  if CurlResult <> CURLE_OK then
  begin
    CurlResult := curl_easy_getinfo(FCurl, CURLINFO_CONTENT_LENGTH_DOWNLOAD,
      @dsize);
    Result.Bytes := ceil(dsize);
  end;

  FErrors^.Push(CurlResult);
end;

function TContent.ToString : String;
begin
  // TODO
end;

function TContent.ToBytes : TMemoryStream;
begin
  // TODO
end;

end.
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

unit curl.response.property_modules.speed;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  libpascurl, utils.datasize,
  curl.response.property_module;

type
  TModuleSpeed = class(TPropertyModule)
  protected  
    { Get download speed. }
    function GetDownload : TDataSize;

    { Get upload speed. }
    function GetUpload : TDataSize;
  protected
    { Get download speed. 
      The average download speed that curl measured for the complete download. }
    property Download : TDataSize read GetDownload;

    { Get upload speed. 
      The average upload speed that curl measured for the complete upload. }
    property Upload : TDataSize read GetUpload;  
  end;

implementation

{ TModuleSpeed }

function TModuleSpeed.GetDownload : TDataSize;
begin
  Result := TDataSize.Create;
  Result.Bytes := GetInt64Value(CURLINFO_SPEED_DOWNLOAD, 
    CURLINFO_SPEED_DOWNLOAD_T);
end;

function TModuleSpeed.GetUpload : TDataSize;
begin
  Result := TDataSize.Create;
  Result.Bytes := GetInt64Value(CURLINFO_SPEED_UPLOAD,
    CURLINFO_SPEED_UPLOAD_T);
end;

end.
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

unit curl.http.request.method;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

type
  TMethod = (
    { The GET method is used to retrieve information from the given server using 
      a given URI. Requests using GET should only retrieve data and should have 
      no other effect on the data. }    
    GET,

    { The HEAD method is functionally similar to GET, except that the server 
      replies with a response line and headers, but no entity-body. }
    HEAD,

    { A POST request is used to send data to the server, for example, customer 
      information, file upload, etc. using HTML forms. }
    POST,

    { The PUT method is used to request the server to store the included 
      entity-body at a location specified by the given URL. }
    PUT,

    { The DELETE method is used to request the server to delete a file at a 
      location specified by the given URL. }
    DELETE,

    { Establishes a tunnel to the server identified by a given URI. }
    CONNECT,

    { Describes the communication options for the target resource. }
    OPTIONS,

    { Performs a message loop-back test along the path to the target resource. }
    TRACE,

    { A PATCH request is considered a set of instructions on how to modify a 
      resource. Contrast this with PUT; which is a complete representation of a 
      resource. }
    PATCH,

    { Custom request method. }
    CUSTOM
  );

implementation

end.
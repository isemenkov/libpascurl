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

unit curl.http.auth_methods;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

type
  TAuthMethod = (
    { HTTP Basic authentication. This is the default choice, and the only method 
      that is in wide-spread use and supported virtually everywhere. This sends 
      the user name and password over the network in plain text, easily captured 
      by others. }
    AUTH_BASIC,

    { HTTP Digest authentication. Digest authentication is defined in RFC 2617 
      and is a more secure way to do authentication over public networks than 
      the regular old-fashioned Basic method. }
    AUTH_DIGEST,

    { HTTP Digest authentication with an IE flavor. Digest authentication is 
      defined in RFC 2617 and is a more secure way to do authentication over 
      public networks than the regular old-fashioned Basic method. The IE flavor 
      is simply that libcurl will use a special "quirk" that IE is known to have 
      used before version 7 and that some servers require the client to use. }
    AUTH_DIGEST_IE,

    { HTTP Bearer token authentication, used primarily in OAuth 2.0 protocol.
      You can set the Bearer token to use with CURLOPT_XOAUTH2_BEARER. }
    AUTH_BEARER,

    { HTTP Negotiate (SPNEGO) authentication. Negotiate authentication is 
      defined in RFC 4559 and is the most secure way to perform authentication 
      over HTTP.
      You need to build libcurl with a suitable GSS-API library or SSPI on 
      Windows for this to work. }
    AUTH_NEGOTIATE,

    { HTTP NTLM authentication. A proprietary protocol invented and used by 
      Microsoft. It uses a challenge-response and hash concept similar to 
      Digest, to prevent the password from being eavesdropped.
      You need to build libcurl with either OpenSSL, GnuTLS or NSS support for 
      this option to work, or build libcurl on Windows with SSPI support. }
    AUTH_NTLM,

    { NTLM delegating to winbind helper. Authentication is performed by a 
      separate binary application that is executed when needed. The name of the 
      application is specified at compile time but is typically 
      /usr/bin/ntlm_auth.
      Note that libcurl will fork when necessary to run the winbind application 
      and kill it when complete, calling waitpid() to await its exit when done. 
      On POSIX operating systems, killing the process will cause a SIGCHLD 
      signal to be raised (regardless of whether CURLOPT_NOSIGNAL is set), which 
      must be handled intelligently by the application. In particular, the 
      application must not unconditionally call wait() in its SIGCHLD signal 
      handler to avoid being subject to a race condition. This behavior is 
      subject to change in future versions of libcurl. }
    AUTH_NTLM_WB,

    { This is a meta symbol. OR this value together with a single specific auth 
      value to force libcurl to probe for un-restricted auth and if not, only 
      that single auth algorithm is acceptable. }
    AUTH_ONLY
  );

  TAuthMethods = set of TAuthMethod;

const
  { This is a convenience macro that sets all bits and thus makes libcurl pick 
    any it finds suitable. libcurl will automatically select the one it finds 
    most secure. }
  AUTH_ANY_METHOD : TAuthMethods = [AUTH_BASIC, AUTH_DIGEST, AUTH_DIGEST_IE,
    AUTH_BEARER, AUTH_NEGOTIATE, AUTH_NTLM, AUTH_NTLM_WB];

  { This is a convenience macro that sets all bits except Basic and thus makes 
    libcurl pick any it finds suitable. libcurl will automatically select the 
    one it finds most secure. }
  AUTH_ANY_SAFE : TAuthMethods = [AUTH_DIGEST, AUTH_BEARER, AUTH_NEGOTIATE, 
    AUTH_NTLM, AUTH_NTLM_WB];

implementation

end.

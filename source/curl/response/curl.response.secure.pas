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

unit curl.response.secure;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  curl.utils.errorstack, curl.utils.stringlist, libpascurl;

type
  {  }
  TSecure = class
  public
    type
      { SSL verify result code }
      TSSLResult = (
        { The operation was successful }
        ERR_OK                                                           = 0,
        
        { Unable to get issuer certificate" the issuer certificate could not 
          be found: this occurs if the issuer certificate of an untrusted 
          certificate cannot be found }
        ERR_UNABLE_TO_GET_ISSUER_CERT                                    = 2,
          
        { The CRL of a certificate could not be found }
        ERR_UNABLE_TO_GET_CRL { UNUSED }                                 = 3,

        { The certificate signature could not be decrypted. This means that 
          the actual signature value could not be determined rather than it 
          not matching the expected value, this is only meaningful for RSA 
          keys. }
        ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE                             = 4,

        { The CRL signature could not be decrypted: this means that the 
          actual signature value could not be determined rather than it not 
              matching the expected value. }
        ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE { UNUSED }                   = 5,

        { The public key in the certificate SubjectPublicKeyInfo could not 
          be read. }
        ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY                           = 6,

        { The signature of the certificate is invalid. }
        ERR_CERT_SIGNATURE_FAILURE                                       = 7,

        { The signature of the certificate is invalid. }
        ERR_CRL_SIGNATURE_FAILURE { UNUSED }                             = 8,

        { The certificate is not yet valid: the notBefore date is after the 
          current time. }
        ERR_CERT_NOT_YET_VALID                                           = 9, 

        { The certificate has expired: that is the notAfter date is before 
          the current time. }
        ERR_CERT_HAS_EXPIRED                                             = 10,

        { The CRL is not yet valid. }
        ERR_CRL_NOT_YET_VALID { UNUSED }                                 = 11,

        { The CRL has expired. }
        ERR_CRL_HAS_EXPIRED { UNUSED }                                   = 12,

        { The certificate notBefore field contains an invalid time. }
        ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD                               = 13,

        { The certificate notAfter field contains an invalid time. }
        ERR_ERROR_IN_CERT_NOT_AFTER_FIELD                                = 14,

        { The CRL lastUpdate field contains an invalid time. }
        ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD { UNUSED }                    = 15,

        { The CRL nextUpdate field contains an invalid time. }
        ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD { UNUSED }                    = 16,

        { An error occurred trying to allocate memory. This should never 
          happen. }
        ERR_OUT_OF_MEM                                                   = 17, 

        { The passed certificate is self signed and the same certificate 
          cannot be found in the list of trusted certificates. }
        ERR_DEPTH_ZERO_SELF_SIGNED_CERT                                  = 18,

        { The certificate chain could be built up using the untrusted 
          certificates but the root could not be found locally. }
        ERR_SELF_SIGNED_CERT_IN_CHAIN                                    = 19,

        { The issuer certificate of a locally looked up certificate could 
          not be found. This normally means the list of trusted certificates 
          is not complete. }
        ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY                            = 20,

        { No signatures could be verified because the chain contains only 
          one certificate and it is not self signed. }
        ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE                              = 21,

        { The certificate chain length is greater than the supplied maximum 
          depth. }
        ERR_CERT_CHAIN_TOO_LONG { UNUSED }                               = 22,

        { The certificate has been revoked. }
        ERR_CERT_REVOKED { UNUSED }                                      = 23,

        { A CA certificate is invalid. Either it is not a CA or its 
          extensions are not consistent with the supplied purpose. }
        ERR_INVALID_CA                                                   = 24,

        { The basicConstraints pathlength parameter has been exceeded. }
        ERR_PATH_LENGTH_EXCEEDED                                         = 25,

        { The supplied certificate cannot be used for the specified 
          purpose. }
        ERR_INVALID_PURPOSE                                              = 26,

        { The root CA is not marked as trusted for the specified purpose. }
        ERR_CERT_UNTRUSTED                                               = 27,

        { The root CA is marked to reject the specified purpose. }
        ERR_CERT_REJECTED                                                = 28,

        { The current candidate issuer certificate was rejected because its 
          subject name did not match the issuer name of the current 
          certificate. Only displayed when the -issuer_checks option is 
          set. }
        ERR_SUBJECT_ISSUER_MISMATCH                                      = 29,

        { The current candidate issuer certificate was rejected because its 
          subject key identifier was present and did not match the authority 
          key identifier current certificate. Only displayed when the 
          -issuer_checks option is set. }
        ERR_AKID_SKID_MISMATCH                                           = 30,

        { The current candidate issuer certificate was rejected because its 
          issuer name and serial number was present and did not match the 
          authority key identifier of the current certificate. Only 
          displayed when the -issuer_checks option is set. }
        ERR_AKID_ISSUER_SERIAL_MISMATCH                                  = 31,

        { The current candidate issuer certificate was rejected because its 
          keyUsage extension does not permit certificate signing. }
        ERR_KEYUSAGE_NO_CERTSIGN                                         = 32,

        { An application specific error. }
        ERR_APPLICATION_VERIFICATION { UNUSED }                          = 50 
      );

      { HTTP(S) auth methods }
      THTTPAuthMethod = (
        { No HTTP authentication. 
          A request does not contain any authentication information. This is 
          equivalent to granting everyone access to the resource. }
        AUTH_NONE                                          = CURLAUTH_NONE,

        { HTTP Basic authentication (default). 
          Basic authentication sends a Base64-encoded string that contains a 
          user name and password for the client. Base64 is not a form of 
          encryption and should be considered the same as sending the user 
          name and password in clear text. If a resource needs to be 
          protected, strongly consider using an authentication scheme other 
          than basic authentication. }
        AUTH_BASIC                                         = CURLAUTH_BASIC,

        { HTTP Digest authentication. 
          Digest access authentication is one of the agreed-upon methods a 
          web server can use to negotiate credentials, such as username or 
          password, with a user's web browser. This can be used to confirm 
          the identity of a user before sending sensitive information, such 
          as online banking transaction history. It applies a hash function 
          to the username and password before sending them over the network. 
          In contrast, basic access authentication uses the easily 
          reversible Base64 encoding instead of hashing, making it 
          non-secure unless used in conjunction with TLS. }
        AUTH_DIGEST                                        = CURLAUTH_DIGEST,

        { HTTP Negotiate (SPNEGO) authentication }
        AUTH_NEGOTIATE                                     = CURLAUTH_NEGOTIATE,

        { HTTP NTLM authentication }
        AUTH_NTLM                                          = CURLAUTH_NTLM,

        { HTTP Digest authentication with IE flavour }
        AUTH_DIGEST_IE                                     = CURLAUTH_DIGEST_IE,

        { HTTP NTLM authentication delegated to winbind helper }
        AUTH_NTLM_WB                                       = CURLAUTH_NTLM_WB,

        { HTTP Bearer token authentication }
        AUTH_BEARER                                        = CURLAUTH_BEARER
      );
    
  private
    constructor Create (ACurl : CURL; AErrors : PErrorStack);
  public
    {  }
    function SSLEngines : curl.utils.stringlist.TStringList;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    {  }
    function SSLResult : TSSLResult;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    {  }
    function SSLProxyResult : TSSLResult;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    {  }
    //function TLSInfo : TTLSInfoEnumerator;
    //  {$IFNDEF DEBUG}inline;{$ENDIF}

    {  }
    //function TLSChain : TTLSChainEnumerator;
    //  {$IFNDEF DEBUG}inline;{$ENDIF}

  end;

implementation

constructor TSecure.Create (ACurl : CURL; AErrors : PErrorStack);
begin
  // TODO
end;

function TSecure.SSLEngines : TStringList;
begin
  // TODO
end;

function TSecure.SSLResult : TSSLResult;
begin
  // TODO
end;

function TSecure.SSLProxyResult : TSSLResult;
begin
  // TODO
end;

end.
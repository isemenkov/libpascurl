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
(* Module:          Unit 'pascurl'                                            *)
(* Functionality:   Provides THTTPSessionPlain and THTTPRequestPlain classes  *)
(*                  to get plain data by http(s) protocol.                    *)
(*                                                                            *)
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

unit http;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils, libpascurl, curlresult, timeinterval, datasize, errorstack;

type
  { HTTP(S) session result response data }
  THTTPResponse = class
  public
    type
      { Result errors code }
      THTTPErrors = (
        { All fine. Proceed as usual }
        ERROR_NONE,

        { The URL you passed to libcurl used a HTTP(S) protocol that this
          libcurl does not support. The support might be a compile-time option
          that you didn't use. }
        ERROR_UNSUPPORTED_PROTOCOL,

        { Very early initialization code failed. This is likely to be an
          internal error or problem, or a resource problem where something
          fundamental couldn't get done at init time.  }
        ERROR_FAILED_INIT,

        { The URL was not properly formatted.  }
        ERROR_URL_MALFORMAT,

        { A requested protocol was not found built-in in this libcurl due to a
          build-time decision. This means that a protocol was not enabled or
          explicitly disabled when libcurl was built and in order to get it to
          function you have to get a rebuilt libcurl. }
        ERROR_NOT_BUILT_IN,

        { Couldn't resolve proxy. The given proxy host could not be resolved. }
        ERROR_COULDNT_RESOLVE_PROXY,

        { Couldn't resolve host. The given remote host was not resolved. }
        ERROR_COULDNT_RESOLVE_HOST,

        { Failed to connect() to host or proxy. }
        ERROR_COULDNT_CONNECT,

        { A problem was detected in the HTTP2 framing layer. This is somewhat
          generic and can be one out of several problems, see the ErrorMessage
          for details. }
        ERROR_HTTP2,

        { This is returned if the HTTP server returns an error code that
          is >= 400. }
        ERROR_HTTP,

        { An error occurred when writing received data to a local file, or an
          error was returned to libcurl from a write callback. }
        ERROR_WRITE,

        { A memory allocation request failed. This is serious badness and things
          are severely screwed up if this ever occurs. }
        ERROR_OUT_OF_MEMORY,

        { Operation timeout. The specified time-out period was reached according
          to the conditions. }
        ERROR_OPERATION_TIMEDOUT,

        { A problem occurred somewhere in the SSL/TLS handshake. You really want
          the ErrorMessage and read the message there as it pinpoints the
          problem slightly more. Could be certificates (file formats, paths,
          permissions), passwords, and others. }
        ERROR_SSL_CONNECT,

        { Too many redirects. When following redirects, libcurl hit the maximum
          amount. Set your limit with CURLOPT_MAXREDIRS. }
        ERROR_TOO_MANY_REDIRECTS,

        { Nothing was returned from the server, and under the circumstances,
          getting nothing is considered an error. }
        ERROR_GOT_NOTHING,

        { The specified crypto engine wasn't found. }
        ERROR_SSL_ENGINE_NOTFOUND,

        { Failed setting the selected SSL crypto engine as default! }
        ERROR_SSL_ENGINE_SETFAILED,

        { Problem with the local client certificate. }
        ERROR_SSL_CERTPROBLEM,

        { Couldn't use specified cipher. }
        ERROR_SSL_CIPHER,

        { The remote server's SSL certificate or SSH md5 fingerprint was deemed
          not OK. }
        ERROR_PEER_FAILED_VERIFICATION,

        { Unrecognized transfer encoding. }
        ERROR_BAD_CONTENT_ENCODING,

        { Initiating the SSL Engine failed. }
        ERROR_SSL_ENGINE_INITFAILED,

        { Stream error in the HTTP/2 framing layer. }
        ERROR_HTTP2_STREAM,

        { A problem was detected in the HTTP/3 layer. This is somewhat generic
          and can be one out of several problems, see the ErrorMessage for
          details. }
        ERROR_HTTP3,

        { Something another goes wrong. Oppps! }
        ERROR_SOMETHING_WRONG
      );

      THTTPStatusCode = (
        { Error status code, not possible as normal! }
        HTTP_STATUS_UNKNOWN                                              = 0,

        { The server, has received the request headers and the client should
          proceed to send the request body (in the case of a request for
          which a body needs to be sent; for example, a POST request).
          Sending a large request body to a server after a request has been
          rejected for inappropriate headers would be inefficient. To have a
          server check the request's headers, a client must send Expect:
          100-continue as a header in its initial request and receive a 100
          Continue status code in response before sending the body. If the
          client receives an error code such as 403 (Forbidden) or 405
          (Method Not Allowed) then it shouldn't send the request's body. The
          response 417 Expectation Failed indicates that the request should
          be repeated without the Expect header as it indicates that the
          server doesn't support expectations (this is the case, for example,
          of HTTP/1.0 servers). }
        HTTP_CONTINUE                                                    = 100,

        { The requester has asked the server to switch protocols and the
          server has agreed to do so. }
        HTTP_SWITCHING_PROTOCOL                                          = 101,

        { A WebDAV request may contain many sub-requests involving file
          operations, requiring a long time to complete the request. This
          code indicates that the server has received and is processing the
          request, but no response is available yet. This prevents the client
          from timing out and assuming the request was lost. }
        HTTP_PROCESSING                                                  = 102,

        { Used to return some response headers before final HTTP message. }
        HTTP_EARLY_HINTS                                                 = 103,

        { Used in the resumable requests proposal to resume aborted PUT or
          POST requests. }
        { UNOFFICIAL CODE }
        { HTTP_CHECKPOINT                                                = 103,}

        { Standard response for successful HTTP requests. The actual response
          will depend on the request method used. In a GET request, the
          response will contain an entity corresponding to the requested
          resource. In a POST request, the response will contain an entity
          describing or containing the result of the action. }
        HTTP_OK                                                          = 200,

        { The request has been fulfilled, resulting in the creation of a new
          resource. }
        HTTP_CREATED                                                     = 201,

        { The request has been accepted for processing, but the processing
          has not been completed. The request might or might not be
          eventually acted upon, and may be disallowed when processing
          occurs. }
        HTTP_ACCEPTED                                                    = 202,

        { The server is a transforming proxy (e.g. a Web accelerator) that
          received a 200 OK from its origin, but is returning a modified
          version of the origin's response. }
        HTTP_NON_AUTHORITATIVE_INFORMATION                               = 203,

        { The server successfully processed the request and is not returning
          any content. }
        HTTP_NO_CONTENT                                                  = 204,

        { The server successfully processed the request, but is not returning
          any content. Unlike a 204 response, this response requires that the
          requester reset the document view. }
        HTTP_RESET_CONTENT                                               = 205,

        { The server is delivering only part of the resource (byte serving)
          due to a range header sent by the client. The range header is used
          by HTTP clients to enable resuming of interrupted downloads, or
          split a download into multiple simultaneous streams. }
        HTTP_PARTIAL_CONTENT                                             = 206,

        { The message body that follows is by default an XML message and can
          contain a number of separate response codes, depending on how many
          sub-requests were made. }
        HTTP_MULTI_STATUS                                                = 207,

        { The members of a DAV binding have already been enumerated in a
          preceding part of the (multistatus) response, and are not being
          included again. }
        HTTP_ALREADY_REPORTED                                            = 208,

        { Used as a catch-all error condition for allowing response bodies to
          flow through Apache when ProxyErrorOverride is enabled. When
          ProxyErrorOverride is enabled in Apache, response bodies that
          contain a status code of 4xx or 5xx are automatically discarded by
          Apache in favor of a generic response or a custom response
          specified by the ErrorDocument directive. }
        { UNOFFICIAL CODE }
        HTTP_THIS_IS_FINE__APACHE_WEB_SERVER                             = 218,

        { The server has fulfilled a request for the resource, and the
          response is a representation of the result of one or more
          instance-manipulations applied to the current instance. }
        HTTP_IM_USED                                                     = 226,

        { Indicates multiple options for the resource from which the client
          may choose (via agent-driven content negotiation). For example,
          this code could be used to present multiple video format options,
          to list files with different filename extensions, or to suggest
          word-sense disambiguation. }
        HTTP_MULTIPLE_CHOICES                                            = 300,

        { This and all future requests should be directed to the given URI. }
        HTTP_MOVED_PERMANENTLY                                           = 301,

        { Tells the client to look at (browse to) another URL. 302 has been
          superseded by 303 and 307. This is an example of industry practice
          contradicting the standard. The HTTP/1.0 specification (RFC 1945)
          required the client to perform a temporary redirect (the original
          describing phrase was "Moved Temporarily"), but popular browsers
          implemented 302 with the functionality of a 303 See Other.
          Therefore, HTTP/1.1 added status codes 303 and 307 to distinguish
          between the two behaviours. However, some Web applications and
          frameworks use the 302 status code as if it were the 303. }
        HTTP_FOUND                                                       = 302,

        { The response to the request can be found under another URI using
          the GET method. When received in response to a POST
          (or PUT/DELETE), the client should presume that the server has
          received the data and should issue a new GET request to the given
          URI. }
        HTTP_SEE_OTHER                                                   = 303,

        { Indicates that the resource has not been modified since the version
          specified by the request headers If-Modified-Since or
          If-None-Match. In such case, there is no need to retransmit the
          resource since the client still has a previously-downloaded copy. }
        HTTP_NOT_MODIFIED                                                = 304,

        { The requested resource is available only through a proxy, the
          address for which is provided in the response. For security
          reasons, many HTTP clients (such as Mozilla Firefox and Internet
          Explorer) do not obey this status code. }
        HTTP_USE_PROXY                                                   = 305,

        { No longer used. Originally meant "Subsequent requests should use
          the specified proxy." }
        HTTP_SWITCH_PROXY                                                = 306,

        { In this case, the request should be repeated with another URI;
          however, future requests should still use the original URI. In
          contrast to how 302 was historically implemented, the request
          method is not allowed to be changed when reissuing the original
          request. For example, a POST request should be repeated using
          another POST request. }
        HTTP_TEMPORARY_REDIRECT                                          = 307,

        { The request and all future requests should be repeated using
          another URI. 307 and 308 parallel the behaviors of 302 and 301, but
          do not allow the HTTP method to change. So, for example, submitting
          a form to a permanently redirected resource may continue smoothly. }
        HTTP_PERMANENT_REDIRECT                                          = 308,

        { The server cannot or will not process the request due to an
          apparent client error (e.g., malformed request syntax, size too
          large, invalid request message framing, or deceptive request
          routing). }
        HTTP_BAD_REQUEST                                                 = 400,

        { Similar to 403 Forbidden, but specifically for use when
          authentication is required and has failed or has not yet been
          provided. The response must include a WWW-Authenticate header field
          containing a challenge applicable to the requested resource. See
          Basic access authentication and Digest access authentication. 401
          semantically means "unauthorised", the user does not have valid
          authentication credentials for the target resource.
          Note: Some sites incorrectly issue HTTP 401 when an IP address is
          banned from the website (usually the website domain) and that
          specific address is refused permission to access a website. }
        HTTP_UNAUTHORIZED                                                = 401,

        { Reserved for future use. The original intention was that this code
          might be used as part of some form of digital cash or micropayment
          scheme, as proposed, for example, by GNU Taler, but that has not
          yet happened, and this code is not usually used. Google Developers
          API uses this status if a particular developer has exceeded the
          daily limit on requests. Sipgate uses this code if an account does
          not have sufficient funds to start a call. Shopify uses this code
          when the store has not paid their fees and is temporarily disabled. }
        HTTP_PAYMENT_REQUIRED                                            = 402,

        { The request contained valid data and was understood by the server,
          but the server is refusing action. This may be due to the user not
          having the necessary permissions for a resource or needing an
          account of some sort, or attempting a prohibited action (e.g.
          creating a duplicate record where only one is allowed). This code
          is also typically used if the request provided authentication via
          the WWW-Authenticate header field, but the server did not accept
          that authentication. The request SHOULD NOT be repeated. }
        HTTP_FORBIDDEN                                                   = 403,

        { The requested resource could not be found but may be available in
          the future. Subsequent requests by the client are permissible. }
        HTTP_NOT_FOUND                                                   = 404,

        { A request method is not supported for the requested resource; for
          example, a GET request on a form that requires data to be presented
          via POST, or a PUT request on a read-only resource. }
        HTTP_METHOD_NOT_ALLOWED                                          = 405,

        { The requested resource is capable of generating only content not
          acceptable according to the Accept headers sent in the request. }
        HTTP_NOT_ACCEPTABLE                                              = 406,

        { The client must first authenticate itself with the proxy. }
        HTTP_PROXY_AUTHENTIFICATION_REQUIRED                             = 407,

        { The server timed out waiting for the request. According to HTTP
          specifications: "The client did not produce a request within the
          time that the server was prepared to wait. The client MAY repeat
          the request without modifications at any later time." }
        HTTP_REQUEST_TIMEOUT                                             = 408,

        { Indicates that the request could not be processed because of
          conflict in the current state of the resource, such as an edit
          conflict between multiple simultaneous updates. }
        HTTP_CONFLICT                                                    = 409,

        { Indicates that the resource requested is no longer available and
          will not be available again. This should be used when a resource
          has been intentionally removed and the resource should be purged.
          Upon receiving a 410 status code, the client should not request the
          resource in the future. Clients such as search engines should
          remove the resource from their indices. Most use cases do not
          require clients and search engines to purge the resource, and a
          "404 Not Found" may be used instead. }
        HTTP_GONE                                                        = 410,

        { The request did not specify the length of its content, which is
          required by the requested resource. }
        HTTP_LENGTH_REQUIRED                                             = 411,

        { The server does not meet one of the preconditions that the
          requester put on the request header fields. }
        HTTP_PRECONDITION_FAILED                                         = 412,

        { The request is larger than the server is willing or able to
          process. Previously called "Request Entity Too Large". }
        HTTP_PAYLOAD_TOO_LARGE                                           = 413,

        { The URI provided was too long for the server to process. Often the
          result of too much data being encoded as a query-string of a GET
          request, in which case it should be converted to a POST request.
          Called "Request-URI Too Long" previously. }
        HTTP_URI_TOO_LONG                                                = 414,

        { The request entity has a media type which the server or resource
          does not support. For example, the client uploads an image as
          image/svg+xml, but the server requires that images use a different
          format. }
        HTTP_UNSUPPORTED_MEDIA_TYPE                                      = 415,

        { The client has asked for a portion of the file (byte serving), but
          the server cannot supply that portion. For example, if the client
          asked for a part of the file that lies beyond the end of the file.
          Called "Requested Range Not Satisfiable" previously. }
        HTTP_RANGE_NOT_SATISFIABLE                                       = 416,

        { The server cannot meet the requirements of the Expect
          request-header field. }
        HTTP_EXPECTATION_FAILED                                          = 417,

        { This code was defined in 1998 as one of the traditional IETF April
          Fools' jokes, in RFC 2324, Hyper Text Coffee Pot Control Protocol,
          and is not expected to be implemented by actual HTTP servers. The
          RFC specifies this code should be returned by teapots requested to
          brew coffee. This HTTP status is used as an Easter egg in some
          websites, including Google.com. }
        HTTP_IM_A_TEAPOT                                                 = 418,

        { Not a part of the HTTP standard, 419 Authentication Timeout denotes
          that previously valid authentication has expired. It is used as an
          alternative to 401 Unauthorized in order to differentiate from
          otherwise authenticated clients being denied access to specific
          server resources. }
        { UNOFFICIAL CODE }
        { HTTP_AUTHENTICATION_TIMEOUT                                    = 419,}

        { Used by the Laravel Framework when a CSRF Token is missing or
          expired. }
        { UNOFFICIAL CODE }
        HTTP_PAGE_EXPIRED__LARAVEL_FRAMEWORK                             = 419,

        { Not part of the HTTP standard, but defined by Spring in the
          HttpStatus class to be used when a method failed. This status code
          is deprecated by Spring. }
        { UNOFFICIAL CODE }
        HTTP_METHOD_FAILURE__SPRING_FRAMEWORK                            = 420,

        { Not part of the HTTP standard, but returned by version 1 of the
          Twitter Search and Trends API when the client is being rate
          limited. Other services may wish to implement the 429 Too Many
          Requests response code instead. }
        { UNOFFICIAL CODE }
        { HTTP_ENHANCE_YOUR_CALM__TWITTER                                = 420,}

        { The request was directed at a server that is not able to produce a
          response (for example because of connection reuse). }
        HTTP_MISDIRECTED_REQUEST                                         = 421,

        { The request was well-formed but was unable to be followed due to
          semantic errors. }
        HTTP_UNPROCESSABLE_ENTITY                                        = 422,

        { The resource that is being accessed is locked. }
        HTTP_LOCKED                                                      = 423,

        { The request failed because it depended on another request and that
          request failed (e.g., a PROPPATCH). }
        HTTP_FAILED_DEPENDENCY                                           = 424,

        { Indicates that the server is unwilling to risk processing a request
          that might be replayed. }
        HTTP_TOO_EARLY                                                   = 425,

        { The client should switch to a different protocol such as TLS/1.0,
          given in the Upgrade header field. }
        HTTP_UPGRADE_REQUIRED                                            = 426,

        { The origin server requires the request to be conditional. Intended
          to prevent the 'lost update' problem, where a client GETs a
          resource's state, modifies it, and PUTs it back to the server, when
          meanwhile a third party has modified the state on the server,
          leading to a conflict. }
        HTTP_PRECONDITION_REQUIRED                                       = 428,

        { The user has sent too many requests in a given amount of time.
          Intended for use with rate-limiting schemes. }
        HTTP_TOO_MANY_REQUESTS                                           = 429,

        { Used by Shopify, instead of the 429 Too Many Requests response
          code, when too many URLs are requested within a certain time frame. }
        { UNOFFICIAL CODE }
        HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE__SHOPIFY                    = 430,

        { The server is unwilling to process the request because either an
          individual header field, or all the header fields collectively, are
          too large. }
        HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE                             = 431,

        { A Microsoft extension. The client's session has expired and must
          log in again. }
        { UNOFFICIAL CODE }
        HTTP_LOGIN_TIMEOUT__MICROSOFT                                    = 440,

        { A non-standard status code used to instruct nginx to close the
          connection without sending a response to the client, most commonly
          used to deny malicious or malformed requests. }
        { UNOFFICIAL CODE }
        HTTP_CONNECTION_CLOSED_WITHOUT_RESPONSE__NGINX                   = 444,

        { A Microsoft extension. The request should be retried after
          performing the appropriate action. Often search-engines or custom
          applications will ignore required parameters. Where no default
          action is appropriate, the Aviongoo website sends a "HTTP/1.1 Retry
          with valid parameters: param1, param2, . . ." response. The
          applications may choose to learn, or not. }
        { UNOFFICIAL CODE }
        HTTP_RETRY_WITH__MICROSOFT                                       = 449,

        { A Microsoft extension. This error is given when Windows Parental
          Controls are turned on and are blocking access to the given
          webpage. }
        { UNOFFICIAL CODE }
        HTTP_BLOCKED_BY_WINDOWS_PARENTAL_CONTROLS__MICROSOFT             = 450,

        { A server operator has received a legal demand to deny access to a
          resource or to a set of resources that includes the requested
          resource. The code 451 was chosen as a reference to the novel
          Fahrenheit 451 (see the Acknowledgements in the RFC). }
        HTTP_UNAVAILABLE_FOR_LEGAL_REASONS                               = 451,

        { Client closed the connection with the load balancer before the idle
          timeout period elapsed. Typically when client timeout is sooner
          than the Elastic Load Balancer's timeout. }
        { UNOFFICIAL CODE }
        HTTP_460__AWS_ELASTIC_LOAD_BALANCER                              = 460,

        { The load balancer received an X-Forwarded-For request header with
          more than 30 IP addresses. }
        { UNOFFICIAL CODE }
        HTTP_463__AWS_ELASTIC_LOAD_BALANCER                              = 463,

        { Nginx internal code similar to 431 but it was introduced earlier in
          version 0.9.4 (on January 21, 2011). }
        { UNOFFICIAL CODE }
        HTTP_REQUEST_HEADER_TOO_LARGE__NGINX                             = 494,

        { An expansion of the 400 Bad Request response code, used when the
          client has provided an invalid client certificate. }
        { UNOFFICIAL CODE }
        HTTP_SSL_SERTIFICATE_ERROR__NGINX                                = 495,

        { An expansion of the 400 Bad Request response code, used when a
          client  certificate is required but not provided. }
        { UNOFFICIAL CODE }
        HTTP_SSL_SERTIFICATE_REQUIRED__NGINX                             = 496,

        { An expansion of the 400 Bad Request response code, used when the
          client has made a HTTP request to a port listening for HTTPS
          requests. }
        { UNOFFICIAL CODE }
        HTTP_REQUEST_SENT_TO_HTTPS_PORT__NGINX                           = 497,

        { Returned by ArcGIS for Server. A code of 498 indicates an expired
          or otherwise invalid token. }
        { UNOFFICIAL CODE }
        HTTP_INVALID_TOKEN__ESRI                                         = 498,

        { Returned by ArcGIS for Server. Code 499 indicates that a token is
          required but was not submitted. }
        { UNOFFICIAL CODE }
        HTTP_TOKEN_REQUIRED__ESRI                                        = 499,

        { Used when the client has closed the request before the server could
          send a response. }
        { UNOFFICIAL CODE }
        { HTTP_CLIENT_CLOSED_REQUEST__NGINX                              = 499,}

        { A generic error message, given when an unexpected condition was
          encountered and no more specific message is suitable. }
        HTTP_INTERNAL_SERVER_ERROR                                       = 500,

        { The server either does not recognize the request method, or it
          lacks the ability to fulfil the request. Usually this implies
          future availability (e.g., a new feature of a web-service API). }
        HTTP_NOT_IMPLEMENTED                                             = 501,

        { The server was acting as a gateway or proxy and received an invalid
          response from the upstream server. }
        HTTP_BAD_GETEWAY                                                 = 502,

        { The server cannot handle the request (because it is overloaded or
          down for maintenance). Generally, this is a temporary state. }
        HTTP_SERVICE_UNAVAIBLE                                           = 503,

        { The server was acting as a gateway or proxy and did not receive a
          timely response from the upstream server. }
        HTTP_GATEWAY_TIMEOUT                                             = 504,

        { The server does not support the HTTP protocol version used in the
          request. }
        HTTP_VERSION_NOT_SUPPORTED                                       = 505,

        { Transparent content negotiation for the request results in a
          circular reference. }
        HTTP_VARIANT_ALSO_NEGOTIATES                                     = 506,

        { The server is unable to store the representation needed to complete
          the request. }
        HTTP_INSUFFICIENT_STORAGE                                        = 507,

        { The server detected an infinite loop while processing the request
          (sent instead of 208 Already Reported). }
        HTTP_LOOP_DETECTED                                               = 508,

        { The server has exceeded the bandwidth specified by the server
          administrator; this is often used by shared hosting providers to
          limit the bandwidth of customers. }
        { UNOFFICIAL CODE }
        HTTP_BANDWIDTH_LIMIT_EXCEEDED__APACHE_WEB_SERVER                 = 509,

        { Further extensions to the request are required for the server to
          fulfil it. }
        HTTP_NOT_EXTENDED                                                = 510,

        { The client needs to authenticate to gain network access. Intended
          for use by intercepting proxies used to control access to the
          network (e.g., "captive portals" used to require agreement to Terms
          of Service before granting full Internet access via a Wi-Fi
          hotspot). }
        HTTP_NETWORK_AUTHENTICATION_REQUIRED                             = 511,

        { This status code is not specified in any RFCs, but is used by
          CloudFlare's reverse proxies to signal an "unknown connection issue
          between CloudFlare and the origin web server" to a client in front
          of the proxy. }
        { UNOFFICIAL CODE }
        HTTP_ORIGIN_ERROR__CLOUDFLARE                                    = 520,

        { This status code is not specified in any RFCs, but is used by
          CloudFlare's reverse proxies to indicate that the origin webserver
          refused the connection. }
        { UNOFFICIAL CODE }
        HTTP_WEB_SERVER_IS_DOWN__CLOUDFLARE                              = 521,

        { This status code is not specified in any RFCs, but is used by
          CloudFlare's reverse proxies to signal that a server connection
          timed out. }
        { UNOFFICIAL CODE }
        HTTP_CONNECTION_TIMED_OUT__CLOUDFLARE                            = 522,

        { This status code is not specified in any RFCs, but is used by
          CloudFlare's reverse proxies to signal a resource that has been
          blocked by the administrator of the website or proxy itself. }
        { UNOFFICIAL CODE }
        HTTP_PROXY_DECLINED_REQUEST__CLOUDFLARE                          = 523,

        { This status code is not specified in any RFCs, but is used by
          CloudFlare's reverse proxies to signal a network read timeout
          behind the proxy to a client in front of the proxy. }
        { UNOFFICIAL CODE }
        HTTP_A_TIMEOUT_OCCURED__CLOUDFLARE                               = 524,

        { Cloudflare could not negotiate a SSL/TLS handshake with the origin
          server. }
        { UNOFFICIAL CODE }
        HTTP_SSL_HANDSHAKE_FAILED__CLOUDFLARE                            = 525,

        { Used by Cloudflare and Cloud Foundry's gorouter to indicate failure
          to validate the SSL/TLS certificate that the origin server
          presented. }
        { UNOFFICIAL CODE }
        HTTP_INVALID_SSL_CERTIFICATE__CLOUDFLARE                         = 526,

        { Error 527 indicates that the request timed out or failed after the
          WAN connection had been established. }
        { UNOFFICIAL CODE }
        HTTP_RAILGUN_ERROR__CLOUDFLARE                                   = 527,

        { Used by the Pantheon web platform to indicate a site that has been
          frozen due to inactivity. }
        { UNOFFICIAL CODE }
        { HTTP_SITE_IS_FROZEN__PATHEON                                   = 530,}

        { Error 530 indicates that the requested host name could not be  on
          resolved the Cloudflare network to an origin server. }
        { UNOFFICIAL CODE }
        HTTP_ORIGIN_DNS_ERROR__CLOUDFLARE                                = 530,

        { Used by some HTTP proxies to signal a network read timeout behind
          the proxy to a client in front of the proxy. }
        { UNOFFICIAL CODE }
        HTTP_NETWORK_READ_TIMEOUT_ERROR                                  = 598,

        { This status code is not specified in any RFCs, but is used by some
          HTTP proxies to signal a network connect timeout behind the proxy
          to a client in front of the proxy. }
        { UNOFFICIAL CODE }
        HTTP_NETWORK_CONNECT_TIMEOUT_ERROR                               = 599
      );

      { HTTP protocol version }
      THTTPVersion = (
        HTTP_VERSION_UNKNOWN                                             = 0,

        { Enforce HTTP 1.0 requests. }
        HTTP_VERSION_1_0                       = Longint(CURL_HTTP_VERSION_1_0),

        { Enforce HTTP 1.1 requests. }
        HTTP_VERSION_1_1                       = Longint(CURL_HTTP_VERSION_1_1),

        { Attempt HTTP 2 requests. libcurl will fall back to HTTP 1.1 if
          HTTP 2 can't be negotiated with the server. }
        HTTP_VERSION_2_0                       = Longint(CURL_HTTP_VERSION_2_0),

        { Attempt HTTP 2 over TLS (HTTPS) only. libcurl will fall back to
          HTTP 1.1 if HTTP 2 can't be negotiated with the HTTPS server. For
          clear text HTTP servers, libcurl will use 1.1. }
        HTTP_VERSION_2TLS                     = Longint(CURL_HTTP_VERSION_2TLS),

        { Issue non-TLS HTTP requests using HTTP/2 without HTTP/1.1 Upgrade.
          It requires prior knowledge that the server supports HTTP/2
          straight away. HTTPS requests will still do HTTP/2 the standard way
          with negotiated protocol version in the TLS handshake. }
        HTTP_VERSION_2_PRIOR_KNOWLEDGE
                                 = Longint(CURL_HTTP_VERSION_2_PRIOR_KNOWLEDGE),

        { Setting this value will make libcurl attempt to use HTTP/3 directly
          to server given in the URL. Note that this cannot gracefully
          downgrade to earlier HTTP version if the server doesn't support
          HTTP/3. }
        HTTP_VERSION_3_0                         = Longint(CURL_HTTP_VERSION_3)
      );

      TError = class
      public
        constructor Create;
        function HasErrors : Boolean;
        function ErrorMessage : String;
        function OsErrno : Longint;
        function Errors : TErrorStack;
      end;
  public
    destructor Destroy; override;

    { Return TRUE if error is present }
    function HasErrors : Boolean;

    { Return last error message or empty string if none }
    function ErrorMessage : String;

    { Get errno number from last connect failure }
    function OsErrno : Longint;

    { Return all errors }
    function Errors : TErrorStack;

    { Get the last used URL

      Get the last used effective URL. In cases when you've asked libcurl to
      follow redirects, it may very well not be the same value you set. }
    function EffectiveUrl : String;

    { Get the URL a redirect would go to }
    function RedirectUrl : String;

    { Get Content-Type

      This is the value read from the Content-Type: field. If you get empty,
      it means that the server didn't send a valid Content-Type header. }
    function ContentType : String;

    { Get IP address of last connection  }
    function PrimaryIP : String;

    { Get local IP address of last connection }
    function LocalIP : String;

    { Get the last response code }
    function ResponseCode : THTTPStatusCode;

    { Get the response content plain data }
    function Content : String;

    { Get the result of the certificate verification  }
    function VerifySSLResult : Boolean;

    { Get the result of the proxy certificate verification

      This is only used for HTTPS proxies. }
    function VerifySSLProxyResult : Boolean;

    { Get the CONNECT response code
      Last received HTTP proxy response code to a CONNECT request. }
    function ConnectResponseCode : THTTPStatusCode;

    { Get the HTTP version used in the connection }
    function HTTPVersion : THTTPVersion;

    { Get the number of redirects }
    function RedirectCount : Longint;

    { Get the number of uploaded bytes }
    function Uploaded : TDataSize;

    { Get the number of downloaded bytes }
    function Downloaded : TDataSize;

    { Get upload speed per second }
    function UploadSpeed : TDataSize;

    { Get download speed per second }
    function DownloadSpeed : TDataSize;

    { Get size of retrieved headers }
    function HeaderSize : TDataSize;

    { Get size of sent request }
    function RequestSize : TDataSize;

    { Get content-length of download }
    function ContentLengthDownload : TDataSize;

    { Get the specified size of the upload }
    function ContentLengthUpload : TDataSize;

    { Get number of created connections }
    function NumConnects : Longint;

    { Get the latest destination port number }
    function PrimaryPort : Longint;

    { Get the latest local port number }
    function LocalPort : Longint;

    { Get total time of previous transfer }
    function TotalTime : TTimeInterval;

    { Get the name lookup time }
    function NameLookup : TTimeInterval;

    { Get the time until connect }
    function ConnectTime : TTimeInterval;

    { Get the time until the SSL/SSH handshake is completed
      When a redirect is followed, the time from each request is added
      together. }
    function AppConnectTime : TTimeInterval;

    { Get the time until the file transfer start
      When a redirect is followed, the time from each request is added
      together. }
    function PretransferTime : TTimeInterval;

    { Get time until the first byte is received
      When a redirect is followed, the time from each request is added
      together. }
    function StartTransferTime : TTimeInterval;

    { Get the time for all redirection steps
      When a redirect is followed, the time from each request is added
      together. }
    function RedirectTime : TTimeInterval;

    { Returns the Retry-After retry delay
      The information from the "Retry-After:" header. Returns zero delay if
      there was no header. }
    function RetryAfterDelay : TTimeInterval;

    { Get the last socket used

      If the socket is no longer valid, -1 is returned. When you finish working
      with the socket, you must call curl_easy_cleanup() as usual and let
      libcurl close the socket and cleanup other resources associated with the
      handle. }
    function LastSocket : Longint;

    { Get the active socket }
    function ActiveSocket : curl_socket_t;

    { Get info on unmet time conditional

      Receive the TRUE if the condition provided in the previous request didn't
      match. Alas, if this returns a TRUE you know that the reason you didn't
      get data in return is because it didn't fulfill the condition. }
    function ConditionUnmet : Boolean;

    { Get the URL scheme (sometimes called protocol) used in the connection }
    function Scheme : String;
  private
    {%H-}constructor Create (AHandle : CURL; AErrorStack : PErrorStack);

    { Callback for writting received data

      This callback function gets called by libcurl as soon as there is data
      received that needs to be saved. For most transfers, this callback gets
      called many times and each invoke delivers another chunk of data. ptr
      points to the delivered data, and the size of that data is nmemb; size is
      always 1. }
    class function WriteFunctionCallback (ptr : PChar; size : LongWord; nmemb :
      LongWord; data : Pointer) : LongWord; static; cdecl;
    function Write (ptr : PChar; size : LongWord; nmemb : LongWord) :
      LongWord;

    { Convert cURL lib error codes to THTTPErrors }
    class function CurlErrorToRequestError (ACode : CURLcode) : THTTPErrors;
      {$IFNDEF DEBUG}inline;{$ENDIF}
  private
    FHandle : CURL;
    FErrorStack : TErrorStack;
    FBuffer : TMemoryStream;
    FErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
  end;

  { Simple request to get data by HTTP(S) protocol }
  THTTPSessionPlain = class
  public
    const
      { Default used user agent }
      DEFAULT_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) ' +
                           'AppleWebKit/537.36 (KHTML, like Gecko)  '   +
                           'Chrome/71.0.3578.98 Safari/537.36';

    type
      { Request result type }
      THTTPResponseResult = specialize TCurlResult<THTTPResponse,
        THTTPResponse.THTTPErrors>;
  public
    { Create new plain http session }
    constructor Create;

    { Create new plain http session with picked url }
    constructor Create (AURL : String);

    destructor Destroy; override;

    { Set the URL to use in the request

      The parameter should be a string which must be URL-encoded in the
      following format: scheme://host:port/path
      libcurl doesn't validate the syntax or use this variable until the
      transfer is issued.
      The host part of the URL contains the address of the server that you want
      to connect to. This can be the fully qualified domain name of the server,
      the local network name of the machine on your network or the IP address
      of the server or machine represented by either an IPv4 or IPv6 address.
      http://www.example.com/
      http://hostname/
      http://192.168.0.1/
      http://[2001:1890:1112:1::20]/
      It is also possible to specify the user name, password and any supported
      login options as part of the host, for the following protocols, when
      connecting to servers that require authentication:
      http://user:password@www.example.com }
    function URL (AURL : String) : THTTPSessionPlain;

    { Set remote port number to work with

      This option sets number to be the remote port number to connect to,
      instead of the one specified in the URL or the default port for the
      http(s) protocol.
      Usually, you just let the URL decide which port to use but this
      allows the application to override that. }
    function Port (APort : Word) : THTTPSessionPlain;

    { Set HTTP user-agent header

      It will be used to set the User-Agent: header in the HTTP request sent to
      the remote server. This can be used to fool servers or scripts. }
    function UserAgent (AAgent : String = DEFAULT_USER_AGENT) :
      THTTPSessionPlain;

    { Follow HTTP 3XXX redirects

      Tells the library to follow any Location: header that the server
      sends as part of an HTTP header in a 3xx response. The Location:
      header can specify a relative or an absolute URL to follow.
      libcurl will issue another request for the new URL and follow new
      Location: headers all the way until no more such headers are
      returned. }
    function FollowRedirect (AFollow : Boolean = True) : THTTPSessionPlain;

    { Ask for an HTTP GET request

      This forces the HTTP request to get back to using GET.
      When setting Get, it will automatically set NoBody to False and Upload to
      False. }
    function Get : THTTPResponseResult;
  private
    FHandle : CURL;
    FErrorStack : TErrorStack;
  end;

implementation

{ THTTPResponse }

class function THTTPResponse.CurlErrorToRequestError (ACode : CURLcode) :
  THTTPErrors;
begin
  case ACode of
    CURLE_OK :                         Result := ERROR_NONE;
    CURLE_UNSUPPORTED_PROTOCOL :       Result := ERROR_UNSUPPORTED_PROTOCOL;
    CURLE_FAILED_INIT :                Result := ERROR_FAILED_INIT;
    CURLE_URL_MALFORMAT :              Result := ERROR_URL_MALFORMAT;
    CURLE_NOT_BUILT_IN :               Result := ERROR_NOT_BUILT_IN;
    CURLE_COULDNT_RESOLVE_PROXY :      Result := ERROR_COULDNT_RESOLVE_PROXY;
    CURLE_COULDNT_RESOLVE_HOST :       Result := ERROR_COULDNT_RESOLVE_HOST;
    CURLE_COULDNT_CONNECT :            Result := ERROR_COULDNT_CONNECT;
    CURLE_HTTP2 :                      Result := ERROR_HTTP2;
    CURLE_HTTP_RETURNED_ERROR :        Result := ERROR_HTTP;
    CURLE_WRITE_ERROR :                Result := ERROR_WRITE;
    CURLE_OUT_OF_MEMORY :              Result := ERROR_OUT_OF_MEMORY;
    CURLE_OPERATION_TIMEDOUT :         Result := ERROR_OPERATION_TIMEDOUT;
    CURLE_SSL_CONNECT_ERROR :          Result := ERROR_SSL_CONNECT;
    CURLE_TOO_MANY_REDIRECTS :         Result := ERROR_TOO_MANY_REDIRECTS;
    CURLE_GOT_NOTHING :                Result := ERROR_GOT_NOTHING;
    CURLE_SSL_ENGINE_NOTFOUND :        Result := ERROR_SSL_ENGINE_NOTFOUND;
    CURLE_SSL_ENGINE_SETFAILED :       Result := ERROR_SSL_ENGINE_SETFAILED;
    CURLE_SSL_CERTPROBLEM :            Result := ERROR_SSL_CERTPROBLEM;
    CURLE_SSL_CIPHER :                 Result := ERROR_SSL_CIPHER;
    CURLE_PEER_FAILED_VERIFICATION :   Result := ERROR_PEER_FAILED_VERIFICATION;
    CURLE_BAD_CONTENT_ENCODING :       Result := ERROR_BAD_CONTENT_ENCODING;
    CURLE_SSL_ENGINE_INITFAILED :      Result := ERROR_SSL_ENGINE_INITFAILED;
    CURLE_HTTP2_STREAM :               Result := ERROR_HTTP2_STREAM;
    CURLE_HTTP3 :                      Result := ERROR_HTTP3;
    otherwise                          Result := ERROR_SOMETHING_WRONG
  end;
end;

class function THTTPResponse.WriteFunctionCallback (ptr : PChar; size :
  LongWord; nmemb : LongWord; data : Pointer) : LongWord; static; cdecl;
begin
  Result := THTTPResponse(data).Write(ptr, size, nmemb);
end;

function THTTPResponse.Write (ptr : PChar; size : LongWord; nmemb :
  LongWord) : LongWord;
begin
  Result := FBuffer.Write(ptr^, size * nmemb);
end;

constructor THTTPResponse.Create (AHandle : CURL; AErrorStack :
  PErrorStack);
begin
  FHandle := AHandle;
  FErrorStack := TErrorStack.Create;
  FErrorStack := AErrorStack^;
  FBuffer := TMemoryStream.Create;

  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_WRITEDATA, Pointer(Self)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_WRITEFUNCTION,
    @THTTPResponse.WriteFunctionCallback));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_ERRORBUFFER,
    PChar(FErrorBuffer)));
end;

destructor THTTPResponse.Destroy;
begin
  curl_easy_cleanup(FHandle);
  FreeAndNil(FErrorStack);
  FreeAndNil(FBuffer);

  inherited Destroy;
end;

function THTTPResponse.HasErrors : Boolean;
begin
  Result := FErrorStack.Count > 0;
end;

function THTTPResponse.ErrorMessage : String;
begin
  Result := String(FErrorBuffer);
end;

function THTTPResponse.Errors : TErrorStack;
begin
  Result := FErrorStack;
end;

function THTTPResponse.Content : String;
var
  Stream : TStringStream;
begin
  Stream := TStringStream.Create('');
  Stream.Write(FBuffer.Memory^, FBuffer.Size);
  Result := Stream.DataString;
  FreeAndNil(Stream);
end;

function THTTPResponse.EffectiveUrl : String;
var
  url : PChar;
begin
  New(url);
  url := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_EFFECTIVE_URL, @url));
  Result := String(url);
end;

function THTTPResponse.ContentType : String;
var
  content_type : PChar;
begin
  New(content_type);
  content_type := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_CONTENT_TYPE,
    @content_type));
  Result := String(content_type);
end;

function THTTPResponse.RedirectUrl : String;
var
  url : PChar;
begin
  New(url);
  url := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_REDIRECT_URL, @url));
  Result := url;
end;

function THTTPResponse.PrimaryIP : String;
var
  ip : PChar;
begin
  New(ip);
  ip := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_PRIMARY_IP, @ip));
  Result := ip;
end;

function THTTPResponse.LocalIP : String;
var
  ip : PChar;
begin
  New(ip);
  ip := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_LOCAL_IP, @ip));
  Result := ip;
end;

function THTTPResponse.ResponseCode : THTTPStatusCode;
var
  Code : Longint;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_RESPONSE_CODE, @Code));
  Result := THTTPStatusCode(Code);
end;

function THTTPResponse.VerifySSLResult : Boolean;
var
  verify : Longint = 1;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SSL_VERIFYRESULT,
    @verify));
  Result := (verify = 0);
end;

function THTTPResponse.VerifySSLProxyResult : Boolean;
var
  verify : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_PROXY_SSL_VERIFYRESULT,
    @verify));
  Result := Boolean(verify);
end;

function THTTPResponse.ConnectResponseCode : THTTPStatusCode;
var
  code : Longint;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_HTTP_CONNECTCODE,
    @code));
  Result := THTTPStatusCode(code);
end;

function THTTPResponse.HTTPVersion : THTTPVersion;
var
  version : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_HTTP_VERSION, @version));
  Result := THTTPVersion(version);
end;

function THTTPResponse.RedirectCount : Longint;
var
  count : Longint;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_REDIRECT_COUNT,
    @count));
  Result := count;
end;

function THTTPResponse.Downloaded : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_SIZE_DOWNLOAD_T,
    @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SIZE_DOWNLOAD,
      @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.DownloadSpeed : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_SPEED_DOWNLOAD_T,
    @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SPEED_DOWNLOAD,
      @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.HeaderSize : TDataSize;
var
  bytes : LongWord = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_HEADER_SIZE, @bytes));
  Result := TDataSize.Create;
  Result.Bytes := bytes;
end;

function THTTPResponse.RequestSize : TDataSize;
var
  bytes : LongWord = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_REQUEST_SIZE, @bytes));
  Result := TDataSize.Create;
  Result.Bytes := bytes;
end;

function THTTPResponse.Uploaded : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_SIZE_UPLOAD_T, @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SIZE_UPLOAD,
      @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.UploadSpeed : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_SPEED_UPLOAD_T, @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SPEED_UPLOAD,
      @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.ContentLengthDownload : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T,
    @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle,
      CURLINFO_CONTENT_LENGTH_DOWNLOAD, @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.ContentLengthUpload : TDataSize;
var
  bytes : LongWord = 0;
  dbytes : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_CONTENT_LENGTH_UPLOAD_T,
    @bytes);
  FErrorStack.Push(CurlResult);
  Result := TDataSize.Create;
  Result.Bytes := bytes;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_CONTENT_LENGTH_UPLOAD,
      @dbytes));
    Result.Bytes := trunc(dbytes);
  end;
end;

function THTTPResponse.NumConnects : Longint;
var
  connects : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_NUM_CONNECTS,
    @connects));
  Result := connects;
end;

function THTTPResponse.PrimaryPort : Longint;
var
  port : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_PRIMARY_PORT, @port));
  Result := port;
end;

function THTTPResponse.LocalPort : Longint;
var
  port : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_LOCAL_PORT, @port));
  Result := port;
end;

function THTTPResponse.TotalTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_TOTAL_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_TOTAL_TIME, @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.NameLookup : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_NAMELOOKUP_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_NAMELOOKUP_TIME,
      @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.ConnectTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_CONNECT_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_CONNECT_TIME, @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.AppConnectTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_APPCONNECT_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_APPCONNECT_TIME,
      @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.PreTransferTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_PRETRANSFER_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_PRETRANSFER_TIME,
      @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.StartTransferTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_STARTTRANSFER_TIME_T,
    @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_STARTTRANSFER_TIME,
      @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.RedirectTime : TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  CurlResult := curl_easy_getinfo(FHandle, CURLINFO_REDIRECT_TIME_T, @time);
  FErrorStack.Push(CurlResult);
  Result := TTimeInterval.Create;
  Result.Microseconds := time;

  if CurlResult <> CURLE_OK then
  begin
    FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_REDIRECT_TIME,
      @dtime));
    Result.Seconds := trunc(dtime);
    Result.Milliseconds := trunc(frac(dtime) * 1000);
  end;
end;

function THTTPResponse.RetryAfterDelay : TTimeInterval;
var
  delay : LongWord = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_RETRY_AFTER, @delay));
  Result := TTimeInterval.Create;
  Result.Seconds := delay;
end;

function THTTPResponse.OsErrno : Longint;
var
  errno : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_OS_ERRNO, @errno));
  Result := errno;
end;

function THTTPResponse.LastSocket : Longint;
var
  socket : Longint;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_LASTSOCKET, @socket));
  Result := socket;
end;

function THTTPResponse.ActiveSocket : curl_socket_t;
var
  socket : curl_socket_t;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_ACTIVESOCKET, @socket));
  Result := socket;
end;

function THTTPResponse.ConditionUnmet : Boolean;
var
  unmet : Longint = 0;
begin
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_CONDITION_UNMET,
    @unmet));
  Result := Boolean(unmet);
end;

function THTTPResponse.Scheme : String;
var
  sc : PChar;
begin
  New(sc);
  scheme := '';
  FErrorStack.Push(curl_easy_getinfo(FHandle, CURLINFO_SCHEME, @sc));
  Result := sc;
end;

{ THHTPSessionPlain }

constructor THTTPSessionPlain.Create;
begin
  FHandle := curl_easy_init;
  FErrorStack := TErrorStack.Create;

  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_FOLLOWLOCATION,
    Longint(True)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_USERAGENT,
    PChar(DEFAULT_USER_AGENT)));
end;

constructor THTTPSessionPlain.Create (AURL : String);
begin
  FHandle := curl_easy_init;
  FErrorStack := TErrorStack.Create;

  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_FOLLOWLOCATION,
    Longint(True)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_URL, PChar(AURL)));
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_USERAGENT,
    PChar(DEFAULT_USER_AGENT)));
end;

destructor THTTPSessionPlain.Destroy;
begin
  curl_easy_cleanup(FHandle);
  FreeAndNil(FErrorStack);

  inherited Destroy;
end;

function THTTPSessionPlain.URL (AURL : String) : THTTPSessionPlain;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_URL, PChar(AURL)));
  Result := Self;
end;

function THTTPSessionPlain.Port (APort : Word) : THTTPSessionPlain;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_PORT, Longint(APort)));
  Result := Self;
end;

function THTTPSessionPlain.UserAgent (AAgent : String) : THTTPSessionPlain;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_USERAGENT, PChar(AAgent)));
  Result := Self;
end;

function THTTPSessionPlain.FollowRedirect (AFollow : Boolean) :
  THTTPSessionPlain;
begin
  FErrorStack.Push(curl_easy_setopt(FHandle, CURLOPT_FOLLOWLOCATION,
    Longint(AFollow)));
  Result := Self;
end;

function THTTPSessionPlain.Get : THTTPResponseResult;
var
  Handle : CURL;
  Request : THTTPResponse;
  ErrorCode : CURLcode;
begin
  Handle := curl_easy_duphandle(FHandle);
  Request := THTTPResponse.Create(Handle, @FErrorStack);
  ErrorCode := curl_easy_perform(Handle);
  FErrorStack.Push(ErrorCode);
  Result := THTTPResponseResult.Create(Request,
    THTTPResponse.CurlErrorToRequestError(ErrorCode), ErrorCode = CURLE_OK);
end;

end.


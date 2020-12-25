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

unit curl.http.response.status_code;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

type
  THTTPStatusCode = (
    { Error status code, not possible as normal! }
    HTTP_STATUS_UNKNOWN                                                   = 0,

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
    HTTP_CONTINUE                                                         = 100,

    { The requester has asked the server to switch protocols and the
      server has agreed to do so. }
    HTTP_SWITCHING_PROTOCOL                                               = 101,

    { A WebDAV request may contain many sub-requests involving file
      operations, requiring a long time to complete the request. This
      code indicates that the server has received and is processing the
      request, but no response is available yet. This prevents the client
      from timing out and assuming the request was lost. }
    HTTP_PROCESSING                                                       = 102,

    { Used to return some response headers before final HTTP message. }
    HTTP_EARLY_HINTS                                                      = 103,

    { Used in the resumable requests proposal to resume aborted PUT or
      POST requests. }
    { UNOFFICIAL CODE }
    { HTTP_CHECKPOINT                                                    = 103,}

    { Standard response for successful HTTP requests. The actual response
      will depend on the request method used. In a GET request, the
      response will contain an entity corresponding to the requested
      resource. In a POST request, the response will contain an entity
      describing or containing the result of the action. }
    HTTP_OK                                                               = 200,

    { The request has been fulfilled, resulting in the creation of a new
      resource. }
    HTTP_CREATED                                                          = 201,

    { The request has been accepted for processing, but the processing
      has not been completed. The request might or might not be
      eventually acted upon, and may be disallowed when processing
      occurs. }
    HTTP_ACCEPTED                                                         = 202,

    { The server is a transforming proxy (e.g. a Web accelerator) that
      received a 200 OK from its origin, but is returning a modified
      version of the origin's response. }
    HTTP_NON_AUTHORITATIVE_INFORMATION                                    = 203,

    { The server successfully processed the request and is not returning
      any content. }
    HTTP_NO_CONTENT                                                       = 204,

    { The server successfully processed the request, but is not returning
      any content. Unlike a 204 response, this response requires that the
      requester reset the document view. }
    HTTP_RESET_CONTENT                                                    = 205,

    { The server is delivering only part of the resource (byte serving)
      due to a range header sent by the client. The range header is used
      by HTTP clients to enable resuming of interrupted downloads, or
      split a download into multiple simultaneous streams. }
    HTTP_PARTIAL_CONTENT                                                  = 206,

    { The message body that follows is by default an XML message and can
      contain a number of separate response codes, depending on how many
      sub-requests were made. }
    HTTP_MULTI_STATUS                                                     = 207,

    { The members of a DAV binding have already been enumerated in a
      preceding part of the (multistatus) response, and are not being
      included again. }
    HTTP_ALREADY_REPORTED                                                 = 208,

    { Used as a catch-all error condition for allowing response bodies to
      flow through Apache when ProxyErrorOverride is enabled. When
      ProxyErrorOverride is enabled in Apache, response bodies that
      contain a status code of 4xx or 5xx are automatically discarded by
      Apache in favor of a generic response or a custom response
      specified by the ErrorDocument directive. }
    { UNOFFICIAL CODE }
    HTTP_THIS_IS_FINE__APACHE_WEB_SERVER                                  = 218,

    { The server has fulfilled a request for the resource, and the
      response is a representation of the result of one or more
      instance-manipulations applied to the current instance. }
    HTTP_IM_USED                                                          = 226,

    { Indicates multiple options for the resource from which the client
      may choose (via agent-driven content negotiation). For example,
      this code could be used to present multiple video format options,
      to list files with different filename extensions, or to suggest
      word-sense disambiguation. }
    HTTP_MULTIPLE_CHOICES                                                 = 300,

    { This and all future requests should be directed to the given URI. }
    HTTP_MOVED_PERMANENTLY                                                = 301,

    { Tells the client to look at (browse to) another URL. 302 has been
      superseded by 303 and 307. This is an example of industry practice
      contradicting the standard. The HTTP/1.0 specification (RFC 1945)
      required the client to perform a temporary redirect (the original
      describing phrase was "Moved Temporarily"), but popular browsers
      implemented 302 with the functionality of a 303 See Other.
      Therefore, HTTP/1.1 added status codes 303 and 307 to distinguish
      between the two behaviours. However, some Web applications and
      frameworks use the 302 status code as if it were the 303. }
    HTTP_FOUND                                                            = 302,

    { The response to the request can be found under another URI using
      the GET method. When received in response to a POST
      (or PUT/DELETE), the client should presume that the server has
      received the data and should issue a new GET request to the given
      URI. }
    HTTP_SEE_OTHER                                                        = 303,

    { Indicates that the resource has not been modified since the version
      specified by the request headers If-Modified-Since or
      If-None-Match. In such case, there is no need to retransmit the
      resource since the client still has a previously-downloaded copy. }
    HTTP_NOT_MODIFIED                                                     = 304,

    { The requested resource is available only through a proxy, the
      address for which is provided in the response. For security
      reasons, many HTTP clients (such as Mozilla Firefox and Internet
      Explorer) do not obey this status code. }
    HTTP_USE_PROXY                                                        = 305,

    { No longer used. Originally meant "Subsequent requests should use
      the specified proxy." }
    HTTP_SWITCH_PROXY                                                     = 306,

    { In this case, the request should be repeated with another URI;
      however, future requests should still use the original URI. In
      contrast to how 302 was historically implemented, the request
      method is not allowed to be changed when reissuing the original
      request. For example, a POST request should be repeated using
      another POST request. }
    HTTP_TEMPORARY_REDIRECT                                               = 307,

    { The request and all future requests should be repeated using
      another URI. 307 and 308 parallel the behaviors of 302 and 301, but
      do not allow the HTTP method to change. So, for example, submitting
      a form to a permanently redirected resource may continue smoothly. }
    HTTP_PERMANENT_REDIRECT                                               = 308,

    { The server cannot or will not process the request due to an
      apparent client error (e.g., malformed request syntax, size too
      large, invalid request message framing, or deceptive request
      routing). }
    HTTP_BAD_REQUEST                                                      = 400,

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
    HTTP_UNAUTHORIZED                                                     = 401,

    { Reserved for future use. The original intention was that this code
      might be used as part of some form of digital cash or micropayment
      scheme, as proposed, for example, by GNU Taler, but that has not
      yet happened, and this code is not usually used. Google Developers
      API uses this status if a particular developer has exceeded the
      daily limit on requests. Sipgate uses this code if an account does
      not have sufficient funds to start a call. Shopify uses this code
      when the store has not paid their fees and is temporarily disabled. }
    HTTP_PAYMENT_REQUIRED                                                 = 402,

    { The request contained valid data and was understood by the server,
      but the server is refusing action. This may be due to the user not
      having the necessary permissions for a resource or needing an
      account of some sort, or attempting a prohibited action (e.g.
      creating a duplicate record where only one is allowed). This code
      is also typically used if the request provided authentication via
      the WWW-Authenticate header field, but the server did not accept
      that authentication. The request SHOULD NOT be repeated. }
    HTTP_FORBIDDEN                                                        = 403,

    { The requested resource could not be found but may be available in
      the future. Subsequent requests by the client are permissible. }
    HTTP_NOT_FOUND                                                        = 404,

    { A request method is not supported for the requested resource; for
      example, a GET request on a form that requires data to be presented
      via POST, or a PUT request on a read-only resource. }
    HTTP_METHOD_NOT_ALLOWED                                               = 405,

    { The requested resource is capable of generating only content not
      acceptable according to the Accept headers sent in the request. }
    HTTP_NOT_ACCEPTABLE                                                   = 406,

    { The client must first authenticate itself with the proxy. }
    HTTP_PROXY_AUTHENTIFICATION_REQUIRED                                  = 407,

    { The server timed out waiting for the request. According to HTTP
      specifications: "The client did not produce a request within the
      time that the server was prepared to wait. The client MAY repeat
      the request without modifications at any later time." }
    HTTP_REQUEST_TIMEOUT                                                  = 408,

    { Indicates that the request could not be processed because of
      conflict in the current state of the resource, such as an edit
      conflict between multiple simultaneous updates. }
    HTTP_CONFLICT                                                         = 409,

    { Indicates that the resource requested is no longer available and
      will not be available again. This should be used when a resource
      has been intentionally removed and the resource should be purged.
      Upon receiving a 410 status code, the client should not request the
      resource in the future. Clients such as search engines should
      remove the resource from their indices. Most use cases do not
      require clients and search engines to purge the resource, and a
      "404 Not Found" may be used instead. }
    HTTP_GONE                                                             = 410,

    { The request did not specify the length of its content, which is
      required by the requested resource. }
    HTTP_LENGTH_REQUIRED                                                  = 411,

    { The server does not meet one of the preconditions that the
      requester put on the request header fields. }
    HTTP_PRECONDITION_FAILED                                              = 412,

    { The request is larger than the server is willing or able to
      process. Previously called "Request Entity Too Large". }
    HTTP_PAYLOAD_TOO_LARGE                                                = 413,

    { The URI provided was too long for the server to process. Often the
      result of too much data being encoded as a query-string of a GET
      request, in which case it should be converted to a POST request.
      Called "Request-URI Too Long" previously. }
    HTTP_URI_TOO_LONG                                                     = 414,

    { The request entity has a media type which the server or resource
      does not support. For example, the client uploads an image as
      image/svg+xml, but the server requires that images use a different
      format. }
    HTTP_UNSUPPORTED_MEDIA_TYPE                                           = 415,

    { The client has asked for a portion of the file (byte serving), but
      the server cannot supply that portion. For example, if the client
      asked for a part of the file that lies beyond the end of the file.
      Called "Requested Range Not Satisfiable" previously. }
    HTTP_RANGE_NOT_SATISFIABLE                                            = 416,

    { The server cannot meet the requirements of the Expect
      request-header field. }
    HTTP_EXPECTATION_FAILED                                               = 417,

    { This code was defined in 1998 as one of the traditional IETF April
      Fools' jokes, in RFC 2324, Hyper Text Coffee Pot Control Protocol,
      and is not expected to be implemented by actual HTTP servers. The
      RFC specifies this code should be returned by teapots requested to
      brew coffee. This HTTP status is used as an Easter egg in some
      websites, including Google.com. }
    HTTP_IM_A_TEAPOT                                                      = 418,

    { Not a part of the HTTP standard, 419 Authentication Timeout denotes
      that previously valid authentication has expired. It is used as an
      alternative to 401 Unauthorized in order to differentiate from
      otherwise authenticated clients being denied access to specific
      server resources. }
    { UNOFFICIAL CODE }
    { HTTP_AUTHENTICATION_TIMEOUT                                        = 419,}

    { Used by the Laravel Framework when a CSRF Token is missing or
      expired. }
    { UNOFFICIAL CODE }
    HTTP_PAGE_EXPIRED__LARAVEL_FRAMEWORK                                  = 419,

    { Not part of the HTTP standard, but defined by Spring in the
      HttpStatus class to be used when a method failed. This status code
      is deprecated by Spring. }
    { UNOFFICIAL CODE }
    HTTP_METHOD_FAILURE__SPRING_FRAMEWORK                                 = 420,

    { Not part of the HTTP standard, but returned by version 1 of the
      Twitter Search and Trends API when the client is being rate
      limited. Other services may wish to implement the 429 Too Many
      Requests response code instead. }
    { UNOFFICIAL CODE }
    { HTTP_ENHANCE_YOUR_CALM__TWITTER                                    = 420,}

    { The request was directed at a server that is not able to produce a
      response (for example because of connection reuse). }
    HTTP_MISDIRECTED_REQUEST                                              = 421,

    { The request was well-formed but was unable to be followed due to
      semantic errors. }
    HTTP_UNPROCESSABLE_ENTITY                                             = 422,

    { The resource that is being accessed is locked. }
    HTTP_LOCKED                                                           = 423,

    { The request failed because it depended on another request and that
      request failed (e.g., a PROPPATCH). }
    HTTP_FAILED_DEPENDENCY                                                = 424,

    { Indicates that the server is unwilling to risk processing a request
      that might be replayed. }
    HTTP_TOO_EARLY                                                        = 425,

    { The client should switch to a different protocol such as TLS/1.0,
      given in the Upgrade header field. }
    HTTP_UPGRADE_REQUIRED                                                 = 426,

    { The origin server requires the request to be conditional. Intended
      to prevent the 'lost update' problem, where a client GETs a
      resource's state, modifies it, and PUTs it back to the server, when
      meanwhile a third party has modified the state on the server,
      leading to a conflict. }
    HTTP_PRECONDITION_REQUIRED                                            = 428,

    { The user has sent too many requests in a given amount of time.
      Intended for use with rate-limiting schemes. }
    HTTP_TOO_MANY_REQUESTS                                                = 429,

    { Used by Shopify, instead of the 429 Too Many Requests response
      code, when too many URLs are requested within a certain time frame. }
    { UNOFFICIAL CODE }
    HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE__SHOPIFY                         = 430,

    { The server is unwilling to process the request because either an
      individual header field, or all the header fields collectively, are
      too large. }
    HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE                                  = 431,

    { A Microsoft extension. The client's session has expired and must
      log in again. }
    { UNOFFICIAL CODE }
    HTTP_LOGIN_TIMEOUT__MICROSOFT                                         = 440,

    { A non-standard status code used to instruct nginx to close the
      connection without sending a response to the client, most commonly
      used to deny malicious or malformed requests. }
    { UNOFFICIAL CODE }
    HTTP_CONNECTION_CLOSED_WITHOUT_RESPONSE__NGINX                        = 444,

    { A Microsoft extension. The request should be retried after
      performing the appropriate action. Often search-engines or custom
      applications will ignore required parameters. Where no default
      action is appropriate, the Aviongoo website sends a "HTTP/1.1 Retry
      with valid parameters: param1, param2, . . ." response. The
      applications may choose to learn, or not. }
    { UNOFFICIAL CODE }
    HTTP_RETRY_WITH__MICROSOFT                                            = 449,

    { A Microsoft extension. This error is given when Windows Parental
      Controls are turned on and are blocking access to the given
      webpage. }
    { UNOFFICIAL CODE }
    HTTP_BLOCKED_BY_WINDOWS_PARENTAL_CONTROLS__MICROSOFT                  = 450,

    { A server operator has received a legal demand to deny access to a
      resource or to a set of resources that includes the requested
      resource. The code 451 was chosen as a reference to the novel
      Fahrenheit 451 (see the Acknowledgements in the RFC). }
    HTTP_UNAVAILABLE_FOR_LEGAL_REASONS                                    = 451,

    { Client closed the connection with the load balancer before the idle
      timeout period elapsed. Typically when client timeout is sooner
      than the Elastic Load Balancer's timeout. }
    { UNOFFICIAL CODE }
    HTTP_460__AWS_ELASTIC_LOAD_BALANCER                                   = 460,

    { The load balancer received an X-Forwarded-For request header with
      more than 30 IP addresses. }
    { UNOFFICIAL CODE }
    HTTP_463__AWS_ELASTIC_LOAD_BALANCER                                   = 463,

    { Nginx internal code similar to 431 but it was introduced earlier in
      version 0.9.4 (on January 21, 2011). }
    { UNOFFICIAL CODE }
    HTTP_REQUEST_HEADER_TOO_LARGE__NGINX                                  = 494,

    { An expansion of the 400 Bad Request response code, used when the
      client has provided an invalid client certificate. }
    { UNOFFICIAL CODE }
    HTTP_SSL_SERTIFICATE_ERROR__NGINX                                     = 495,

    { An expansion of the 400 Bad Request response code, used when a
      client  certificate is required but not provided. }
    { UNOFFICIAL CODE }
    HTTP_SSL_SERTIFICATE_REQUIRED__NGINX                                  = 496,

    { An expansion of the 400 Bad Request response code, used when the
      client has made a HTTP request to a port listening for HTTPS
      requests. }
    { UNOFFICIAL CODE }
    HTTP_REQUEST_SENT_TO_HTTPS_PORT__NGINX                                = 497,

    { Returned by ArcGIS for Server. A code of 498 indicates an expired
      or otherwise invalid token. }
    { UNOFFICIAL CODE }
    HTTP_INVALID_TOKEN__ESRI                                              = 498,

    { Returned by ArcGIS for Server. Code 499 indicates that a token is
      required but was not submitted. }
    { UNOFFICIAL CODE }
    HTTP_TOKEN_REQUIRED__ESRI                                             = 499,

    { Used when the client has closed the request before the server could
      send a response. }
    { UNOFFICIAL CODE }
    { HTTP_CLIENT_CLOSED_REQUEST__NGINX                                  = 499,}

    { A generic error message, given when an unexpected condition was
      encountered and no more specific message is suitable. }
    HTTP_INTERNAL_SERVER_ERROR                                            = 500,

    { The server either does not recognize the request method, or it
      lacks the ability to fulfil the request. Usually this implies
      future availability (e.g., a new feature of a web-service API). }
    HTTP_NOT_IMPLEMENTED                                                  = 501,

    { The server was acting as a gateway or proxy and received an invalid
      response from the upstream server. }
    HTTP_BAD_GETEWAY                                                      = 502,

    { The server cannot handle the request (because it is overloaded or
      down for maintenance). Generally, this is a temporary state. }
    HTTP_SERVICE_UNAVAIBLE                                                = 503,

    { The server was acting as a gateway or proxy and did not receive a
      timely response from the upstream server. }
    HTTP_GATEWAY_TIMEOUT                                                  = 504,

    { The server does not support the HTTP protocol version used in the
      request. }
    HTTP_VERSION_NOT_SUPPORTED                                            = 505,

    { Transparent content negotiation for the request results in a
      circular reference. }
    HTTP_VARIANT_ALSO_NEGOTIATES                                          = 506,

    { The server is unable to store the representation needed to complete
      the request. }
    HTTP_INSUFFICIENT_STORAGE                                             = 507,

    { The server detected an infinite loop while processing the request
      (sent instead of 208 Already Reported). }
    HTTP_LOOP_DETECTED                                                    = 508,

    { The server has exceeded the bandwidth specified by the server
      administrator; this is often used by shared hosting providers to
      limit the bandwidth of customers. }
    { UNOFFICIAL CODE }
    HTTP_BANDWIDTH_LIMIT_EXCEEDED__APACHE_WEB_SERVER                      = 509,

    { Further extensions to the request are required for the server to
      fulfil it. }
    HTTP_NOT_EXTENDED                                                     = 510,

    { The client needs to authenticate to gain network access. Intended
      for use by intercepting proxies used to control access to the
      network (e.g., "captive portals" used to require agreement to Terms
      of Service before granting full Internet access via a Wi-Fi
      hotspot). }
    HTTP_NETWORK_AUTHENTICATION_REQUIRED                                  = 511,

    { This status code is not specified in any RFCs, but is used by
      CloudFlare's reverse proxies to signal an "unknown connection issue
      between CloudFlare and the origin web server" to a client in front
      of the proxy. }
    { UNOFFICIAL CODE }
    HTTP_ORIGIN_ERROR__CLOUDFLARE                                         = 520,

    { This status code is not specified in any RFCs, but is used by
      CloudFlare's reverse proxies to indicate that the origin webserver
      refused the connection. }
    { UNOFFICIAL CODE }
    HTTP_WEB_SERVER_IS_DOWN__CLOUDFLARE                                   = 521,

    { This status code is not specified in any RFCs, but is used by
      CloudFlare's reverse proxies to signal that a server connection
      timed out. }
    { UNOFFICIAL CODE }
    HTTP_CONNECTION_TIMED_OUT__CLOUDFLARE                                 = 522,

    { This status code is not specified in any RFCs, but is used by
      CloudFlare's reverse proxies to signal a resource that has been
      blocked by the administrator of the website or proxy itself. }
    { UNOFFICIAL CODE }
    HTTP_PROXY_DECLINED_REQUEST__CLOUDFLARE                               = 523,

    { This status code is not specified in any RFCs, but is used by
      CloudFlare's reverse proxies to signal a network read timeout
      behind the proxy to a client in front of the proxy. }
    { UNOFFICIAL CODE }
    HTTP_A_TIMEOUT_OCCURED__CLOUDFLARE                                    = 524,

    { Cloudflare could not negotiate a SSL/TLS handshake with the origin
      server. }
    { UNOFFICIAL CODE }
    HTTP_SSL_HANDSHAKE_FAILED__CLOUDFLARE                                 = 525,

    { Used by Cloudflare and Cloud Foundry's gorouter to indicate failure
      to validate the SSL/TLS certificate that the origin server
      presented. }
    { UNOFFICIAL CODE }
    HTTP_INVALID_SSL_CERTIFICATE__CLOUDFLARE                              = 526,

    { Error 527 indicates that the request timed out or failed after the
      WAN connection had been established. }
    { UNOFFICIAL CODE }
    HTTP_RAILGUN_ERROR__CLOUDFLARE                                        = 527,

    { Used by the Pantheon web platform to indicate a site that has been
      frozen due to inactivity. }
    { UNOFFICIAL CODE }
    { HTTP_SITE_IS_FROZEN__PATHEON                                       = 530,}

    { Error 530 indicates that the requested host name could not be  on
      resolved the Cloudflare network to an origin server. }
    { UNOFFICIAL CODE }
    HTTP_ORIGIN_DNS_ERROR__CLOUDFLARE                                     = 530,

    { Used by some HTTP proxies to signal a network read timeout behind
      the proxy to a client in front of the proxy. }
    { UNOFFICIAL CODE }
    HTTP_NETWORK_READ_TIMEOUT_ERROR                                       = 598,

    { This status code is not specified in any RFCs, but is used by some
      HTTP proxies to signal a network connect timeout behind the proxy
      to a client in front of the proxy. }
    { UNOFFICIAL CODE }
    HTTP_NETWORK_CONNECT_TIMEOUT_ERROR                                    = 599
  );

implementation

end.


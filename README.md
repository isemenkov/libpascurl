libPasCURL
==========
It is delphi and object pascal bindings and wrapper around [cURL library](https://curl.haxx.se/). libcurl is the library is using for transferring data specified with URL syntax,
supporting HTTP, HTTPS, FTP, FTPS, GOPHER, TFTP, SCP, SFTP, SMB, TELNET, DICT, LDAP, LDAPS, FILE, IMAP, SMTP, POP3, RTSP and RTMP.



### Table of contents

* [Requierements](#requirements)
* [Installation](#installation)
* [Usage](#usage)
* [Examples](#examples)
* [Bindings](#bindings)
  * [Usage example](#usage-example)
* [Object wrapper](#object-wrapper)
  * [Base functionality](#base-functionality)
  * [Property modules](#property-modules)
    * [Session modules](#session-modules)
    * [Response modules](#response-modules)
  * [HTTP](#http)
    * [Session modules](#session-modules-1)
    * [Response modules](#response-modules-1)
    * [Usage example](#usage-example-1)



### Requirements

* [Embarcadero (R) Rad Studio](https://www.embarcadero.com)
* [Free Pascal Compiler](http://freepascal.org)
* [Lazarus IDE](http://www.lazarus.freepascal.org/) (optional)



Library is tested for 

- Embarcadero (R) Delphi 10.3 on Windows 7 Service Pack 1 (Version 6.1, Build 7601, 64-bit Edition)
- FreePascal Compiler (3.2.0) and Lazarus IDE (2.0.10) on Ubuntu Linux 5.8.0-33-generic x86_64



### Installation

Get the sources and add the *source* directory to the project search path. For FPC add the *source* directory to the *fpc.cfg* file.



### Usage

Clone the repository `git clone https://github.com/isemenkov/libpascurl`.

Add the unit you want to use to the `uses` clause.



### Examples
1. [RemoteConnectCStyle](https://github.com/isemenkov/libpascurl/tree/master/examples/RemoteConnectCStyle) simple example for use cURL wrapper in C-Style to connect to remote host.
2. [RemoteConnect](https://github.com/isemenkov/libpascurl/tree/master/examples/RemoteConnect) example how to use [curl.http.session.TSession](https://github.com/isemenkov/libpascurl/blob/master/source/http/session/curl.http.session.pas) and [curl.http.response.TResponse](https://github.com/isemenkov/libpascurl/blob/master/source/http/response/curl.http.response.pas) classes to connect to remote host.



### Bindings

[libpascurl.pas](https://github.com/isemenkov/libpascurl/blob/master/source/libpascurl.pas) file contains the cURL translated headers to use this library in pascal programs. You can find C API documentation at [cURL website](https://curl.haxx.se/libcurl/c/).

#### Usage example

```pascal
  uses
    libpascurl;

  var
    handle : CURL;
    effectiveUrl, contentType, ip : PChar;
    responseCode, headerSize : Longint;
    contentLength, totalTime : Longword;
    buffer : TStringStream;

  function WriteFunctionCallback (ptr : PChar; size : LongWord;
    nmemb : LongWord; data : Pointer)
  begin
    buffer.WriteString(string(ptr)); 
  end;

  begin
    curl_global_init(CURL_GLOBAL_ALL);
    curl_easy_setopt(handle, CURLOPT_URL, PChar('https://example.dev');
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, @WriteFunctionCallback);
    buffer := TStringStream.Create('');

    if curl_easy_perform = CURLE_OK then
    begin
      New(effectiveUrl);
      New(contentType);
      New(ip);
  
      curl_easy_getinfo(handle, CURLINFO_EFFECTIVE_URL, @effectiveUrl);
      curl_easy_getinfo(handle, CURLINFO_RESPONSE_CODE, @responseCode);
      curl_easy_getinfo(handle, CURLINFO_HEADER_SIZE, @headerSize);
      curl_easy_getinfo(handle, CURLINFO_CONTENT_TYPE, @contentType);
      curl_easy_getinfo(handle, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T, @contentLength);
      curl_easy_getinfo(handle, CURLINFO_LOCAL_IP, @ip);
      curl_easy_getinfo(handle, CURLINFO_TOTAL_TIME_T, @totalTime);

      writeln('URL: ':20,                 effectiveUrl);
      writeln('Response code: ':20,       responseCode);
      writeln('Header size, kB: ':20,     FormatFloat('0.00', headerSize / 1024));
      writeln('Content type: ',           contentType);
      writeln('Content length, kB: ':20,  FormatFloat('0.00', contentLength / 1024));
      writeln('IP: ':20,                  ip);
      writeln('Total time, ms: ':20,      totalTime);
      writeln('==== Content ====');
      writeln(buffer.DataString);
    end;

    curl_global_cleanup; 

  end;
```



### Object wrapper

#### Base functionality

The library contains a set of classes for creating high-level wrappers around the supported protocols. The `source/curl/` folder contains base components that implements specific functionality.

 Class                                                        | Description                                                  
 ------------------------------------------------------------ | ------------------------------------------------------------ 
 [TCURLEasy](https://github.com/isemenkov/libpascurl/blob/master/source/curl/curl.easy.pas) | It is base class that initialize CURL library and provides error handling functionality. 
 [TSession](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/curl.session.pas) | It is parent class for sessions of all supported protocols. It provides a [TMemoryBuffer](https://github.com/isemenkov/libpasc-algorithms/blob/master/source/container.memorybuffer.pas) for stored downloading/uploading data. 
 [TResponse](https://github.com/isemenkov/libpascurl/blob/master/source/curl/response/curl.response.pas) | It is parent class for server response data.                 
 [TPropertyModule](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/curl.session.property_module.pas) | It is base class for all sessions and responses additional functionality modules. 

#### Property modules

##### Session modules

Module class | Description
-------------|------------
[TModuleDNS](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.dns.pas)| Class provide properties to setup libCurl DNS options.
[TModuleRequest](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.request.pas)| Class provide properties to setup request properties and callbacks.
[TModuleHeader](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.header.pas)| Class provide properties to setup headers. Can be used only with HTTP-like protocols - HTTP(S), FTP(S), POP3(S), IMAP, SMTP. 
[TModuleOptions](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.options.pas)| Class provide properties to setup different libCurl internal options. 
[TModuleProtocols](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.protocols.pas)| Class provide properties to setup libCurl protocol options. 
[TModuleSocket](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.socket.pas)| Class provide properties to socket setup. 
[TModuleTCP](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.tcp.pas)| Class provide properties to setup TCP protocol options. 
[TModuleWriter](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.writer.pas)| Class provide properties to setup download callback function. 
[TModuleReader](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.reader.pas)| Class provide properties to setup upload callback function.
[TModuleAuth](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.auth.pas)| Class provide properties to setup auth options.  
[TModuleTLSAuth](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.tls_auth.pas)| Class provide properties to setup TLS auth authentication options.  
[TModuleProxy](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.proxy.pas)| Class provide properties to setup proxy options. 
[TModuleSock5](https://github.com/isemenkov/libpascurl/blob/master/source/curl/session/property_modules/curl.session.property_modules.sock5.pas)| Class provide properties to setup sock5 proxy options.

##### Response modules

Module class | Description
-------------|------------
[TModuleContent](https://github.com/isemenkov/libpascurl/blob/master/source/curl/response/property_modules/curl.response.property_modules.content.pas)| Class provide properties to get content data buffer. 
[TModuleHeader](https://github.com/isemenkov/libpascurl/blob/master/source/curl/response/property_modules/curl.response.property_modules.header.pas)| Class provide properties to response headers. Can be used only with HTTP-like protocols - HTTP(S), FTP(S), POP3(S), IMAP, SMTP. 
[TModuleRedirect](https://github.com/isemenkov/libpascurl/blob/master/source/curl/response/property_modules/curl.response.property_modules.redirect.pas)| Class provide information about request redirects. 
[TModuleSpeed](https://github.com/isemenkov/libpascurl/blob/master/source/curl/response/property_modules/curl.response.property_modules.speed.pas)| Class provide speed download/upload information. 
[TModuleTimeout](https://github.com/isemenkov/libpascurl/blob/master/source/curl/response/property_modules/curl.response.property_modules.timeout.pas)| Class provide timeouts information. 
[TModuleInfo](https://github.com/isemenkov/libpascurl/blob/master/source/curl/response/property_modules/curl.response.property_modules.info.pas)| Class provide session information. 

#### HTTP

[THTTPSession](https://github.com/isemenkov/libpascurl/blob/master/source/http/session/curl.http.session.pas) and [THTTPResponse](https://github.com/isemenkov/libpascurl/blob/master/source/http/response/curl.http.response.pas) classes implements wrapper about HTTP(S) protocol. This classes extends the functionality of base classes and provided new one that is specific only to this protocol.

This wrapper used or extends the next main modules:
Session modules | Response modules
----------------|:-----------------
:heavy_check_mark: TModuleDNS | :heavy_check_mark: TModuleContent
:heavy_check_mark: TModuleHeader | :heavy_check_mark: TModuleHeader
:heavy_check_mark: TModuleOptions | :heavy_check_mark: TModuleRedirect
:heavy_check_mark: TModuleProtocols | :heavy_check_mark: TModuleSpeed
:heavy_check_mark: TModuleSocket | :heavy_check_mark: TModuleTimeout
:heavy_check_mark: TModuleTCP | :heavy_check_mark: TModuleInfo 
:heavy_check_mark: TModuleWriter |
:heavy_check_mark: TModuleReader |
:heavy_check_mark: TModuleRequest |
:heavy_check_mark: TModuleAuth |
:heavy_check_mark: TModuleTLSAuth |
:heavy_check_mark: TModuleProxy |
:heavy_check_mark: TModuleSock5 |

##### Session modules

Module class | Description
-------------|------------
[TModuleRedirect](https://github.com/isemenkov/libpascurl/blob/master/source/http/session/property_modules/curl.http.session.property_modules.redirect.pas)| Class provide properties to setup http(s) redirect options.
[TModuleHTTP2](https://github.com/isemenkov/libpascurl/blob/master/source/http/session/property_modules/curl.http.session.property_modules.http2.pas)| Class provide properties to setup HTTP/2 protocol options.
[TModuleTimeout](https://github.com/isemenkov/libpascurl/blob/master/source/http/session/property_modules/curl.http.session.property_modules.timeout.pas)| Class provide properties to setup http(s) protocol timeouts options.

##### Response modules

Module class | Description
-------------|------------
[TModuleCookie](https://github.com/isemenkov/libpascurl/blob/master/source/http/response/property_modules/curl.http.response.property_modules.cookie.pas)| Class provide cookies data.

#### Usage example

```pascal
uses 
  curl.http.session, curl.http.response;
  
var
  Session : THTTP.TSession;
  Response : THHTP.TResponse;

begin
  Session.Url := 'https://github.com/isemenkov';
  Session.Redirect.FollowRedirect := True;
  
  Response := Session.Get;
  
  writeln('Url', Response.Request.Url);
  writeln('Response code', Response.Header.ResponseCode);
  writeln('Content-type', Response.Content.ContentType);
  writeln('Content-size', Response.Content.ContentSize);
  writeln('Content', Response.Content.ToString);
end;
```


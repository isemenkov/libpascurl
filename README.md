libPasCURL
==========
It is object pascal bindings and wrapper around [cURL library](https://curl.haxx.se/). libcurl is the library is using for transferring data specified with URL syntax,
supporting HTTP, HTTPS, FTP, FTPS, GOPHER, TFTP, SCP, SFTP, SMB, TELNET, DICT, LDAP, LDAPS, FILE, IMAP, SMTP, POP3, RTSP and RTMP.

Documentation you can find at [wiki page](https://github.com/isemenkov/libpascurl/wiki).



### Table of contents

* [Requierements](#requirements)
* [Installation](#installation)
* [Usage](#usage)
* [Testing](#testing)
* [Examples](#examples)
* [Bindings](#bindings)
  * [Usage example](#usage-example)
* [Object wrapper](#object-wrapper)
  * [Usage example](#usage-example-1)



### Requirements

* [Free Pascal Compiler](http://freepascal.org)
* [Lazarus IDE](http://www.lazarus.freepascal.org/) (optional)

Library is tested with latest stable FreePascal Compiler (currently 3.2.0) and Lazarus IDE (currently 2.0.10).



### Installation

Get the sources and add the *source* directory to the *fpc.cfg* file.



### Usage

Clone the repository `git clone https://github.com/isemenkov/libpascurl`.

Add the unit you want to use to the `uses` clause.



### Testing

A testing framework consists of the following ingredients:
1. Test runner project located in `unit-tests` directory.
2. Test cases (FPCUnit based) for additional helpers classes.  



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











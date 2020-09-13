libPasCURL
==========
It is object pascal bindings and wrapper around [cURL library](https://curl.haxx.se/). libcurl is the library is using for transferring data specified with URL syntax,
supporting HTTP, HTTPS, FTP, FTPS, GOPHER, TFTP, SCP, SFTP, SMB, TELNET, DICT, LDAP, LDAPS, FILE, IMAP, SMTP, POP3, RTSP and RTMP.

Documentation you can find at [wiki page](https://github.com/isemenkov/libpascurl/wiki).

### Table of contents

* [Requierements](#requirements)
* [Installation](#installation)
* [Usage](#usage)
* [Bindings](#bindings)
  * [Usage example](#usage_example)
* [Object wrapper](#object_wrapper)
  * [Usage example](#usage_example)

### Requirements

* [Free Pascal Compiler](http://freepascal.org)
* [Lazarus IDE](http://www.lazarus.freepascal.org/) (optional)

Library is tested with latest stable FreePascal Compiler (currently 3.2.0) and Lazarus IDE (currently 2.0.10).


### Installation

Get the sources and add the *source* directory to the *fpc.cfg* file.

### Usage

Add the unit you want to use to the `uses` clause.

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

[pascurl.pas](https://github.com/isemenkov/libpascurl/blob/master/source/pascurl.pas) file contains the cURL object wrapper.

#### Usage example

```pascal
  uses 
    pascurl;

  var
    FSession : TSession;
    FResponse : TResponse;

    FProtocol : TSession.TProtocolProperty.TProtocol;

  begin
    FSession := TSession.Create;

    FSession.Url := 'https://example.dev';
    
    FResponse := TResponse.Create(FSession);
    if FResponse.Opened and not FResponse.HasErrors then
    begin
      
      FProtocol := FSession.ExtractProtocol(FResponse.EffectiveUrl);
      if FProtocol in [PROTOCOL_HTTP, PROTOCOL_HTTPS] then
      begin
        writeln('Response code :', TSession.THTTPProperty.THTTPStatusCode(FResponse.ResponseCode));
      end else if FProtocol in [PROTOCOL_FTP, PROTOCOL_FTPS] then
      begin
        writeln('Response code :', TSession.TFTPProperty.TFTPStatusCode(FResponse.ResponseCode));
      end;
    
      if FProtocol in [PROTOCOL_HTTP, PROTOCOL_HTTPS] then
      begin
        writeln('HTTP version :', FResponse.HTTPVersion);
      end; 

      writeln('Url :',            FResponse.EffectiveUrl);
      writeln('Redirect count :', FResponse.RedirectCount);
      writeln('Redirect url :',   FResponse.RedirectUrl);
      writeln('Request size :',   FResponse.RequestSize.ToString);
      writeln('Header size :',    FResponse.HeaderSize.ToString);
      writeln('Content size :',   FResponse.Downloaded.ToString);
      writeln('Download speed :', FResponse.DownloadSpeed.ToString('/s'));
      writeln('Total time :',     FResponse.TotalTime.ToString);

      writeln(FResponse.Content); 

      FreeAndNil(FResponse);
      FreeAndNil(FSession);
    end else
      writeln(FResponse.ErrorMessage);
  end;

```

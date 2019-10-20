libPasCURL
==========
object pascal wrapper around cURL library

#### Usage example

```pascal
  program console;

  {$mode objfpc}{$H+}
  
  uses
    Classes, libpascurl;

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
    if ParamCount == 2 then
    begin
      curl_global_init(CURL_GLOBAL_ALL);
      curl_easy_setopt(handle, CURLOPT_URL, PChar(ParamStr(1));
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
  end.
```

#### Usage example

```pascal
  
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

      writeln('Url :', FResponse.EffectiveUrl);
      writeln('Redirect count :', FResponse.RedirectCount);
      writeln('Redirect url :', FResponse.RedirectUrl);
      writeln('Request size :', FResponse.RequestSize.ToString);
      writeln('Header size :', FResponse.HeaderSize.ToString);
      writeln('Content size :', FResponse.Downloaded.ToString);
      writeln('Download speed :', FResponse.DownloadSpeed.ToString('/s'));
      writeln('Total time :', FResponse.TotalTime.ToString);

      writeln(FResponse.Content); 

      FreeAndNil(FResponse);
      FreeAndNil(FSession);
    end

```

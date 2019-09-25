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
  program console;
  
  {$mode objfpc}{$H+}
  
  uses
    pascurl;

  var
    session : TSession;
    info    : TSessionInfo;
  begin
    if ParamCount = 2 then
    begin
      session := TSession.Create;
      session.Url := Param[1];

      session.Protocol.FollowRedirect := True;
      session.Protocol.DefaultProtocol := PROTOCOL_HTTPS;
      session.Protocol.TransferEncoding := True; 
    
      
      info := TSessionInfo.Create(session);
      if info.Opened and not info.HasErrors then
      begin
        writeln('URL: ':20,                 info.EffectiveUrl);
        writeln('Response code: ':20,       info.ResponseCode);
        writeln('Header size, kB: ':20,     info.HeaderSize.Format(dsKiloBytes, '0.00'));
        writeln('Content type: ':20,        info.ContentType);
        writeln('Content length, kB: ':20,  info.Downloaded.Format(dsKiloBytes, '0.00'));
        writeln('IP: ':20,                  info.PrimaryIP);
        writeln('Total time, ms: ':20,      info.TotalTime.Format(tiMicroseconds, '0##'));
        writeln('==== Content ====');
        writeln(info.Content);
      end else
      begin
        writeln(info.ErrorMessage);
      end;
    end;
  end.
```

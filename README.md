libPasCURL
==========
object pascal wrapper around cURL library

#### Usage example

```pascal
  program console;

  {$mode objfpc}{$H+}

  var
    session : TSession;
    info    : TSessionInfo;
  begin
    if ParamCount = 2 then
    begin
      session := TSession.Create;
      with session do
      begin
        Url := ParamStr(1);
      end;
      
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

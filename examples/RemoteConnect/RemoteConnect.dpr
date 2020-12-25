program RemoteConnect;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, curl.http.session, libpascurl,
  curl.utils.headers_list, curl.session.property_modules.header,
  curl.http.response, curl.http.request.method,
  curl.http.response.property_modules.redirect,
  curl.http.response.property_modules.timeout,
  curl.response.property_modules.content;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

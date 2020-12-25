program RemoteConnectCStyle;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Classes, System.AnsiStrings, libpascurl,
  OptionsList in 'OptionsList.pas';

var
  FCURL : libpascurl.CURL;
  FBuffer : TMemoryStream;
  ErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
  OptionsList : TOptions;

procedure PrintHeader;
begin
  writeln(
'(******************************************************************************)'+ sLineBreak +
'(*                                 libPasCURL                                 *)'+ sLineBreak +
'(*                 object pascal wrapper around cURL library                  *)'+ sLineBreak +
'(*                        https://github.com/curl/curl                        *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* Copyright (c) 2019 - 2020                                Ivan Semenkov     *)'+ sLineBreak +
'(* https://github.com/isemenkov/libpascurl                  ivan@semenkov.pro *)'+ sLineBreak +
'(*                                                          Ukraine           *)'+ sLineBreak +
'(******************************************************************************)'
  );
end;

procedure PrintHelp;
begin
  writeln(
'(******************************************************************************)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* Example how  to  connect to  remote host.                                  *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* Usage: RemoteConnect http://example.com/ --show-content                    *)'+ sLineBreak +
'(*    Or: RemoteConnect  ftp://ftp.example.com/ -u Root -p Password -s        *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(*                                                                            *)'+ sLineBreak +
'(* -s            or --show-content        write download content to termainal *)'+ sLineBreak +
'(* -u <username> or --username=<username> set user name to remote host        *)'+ sLineBreak +
'(* -p <password> or --password=<password> set password to remote host         *)'+ sLineBreak +
'(* -a            or --all                 write all response information      *)'+ sLineBreak +
'(*                  --effective-url       write effective url                 *)'+ sLineBreak +
'(*                  --redirect-url        write redirect url if is it         *)'+ sLineBreak +
'(*                  --redirect-count      write redirect counts if is it      *)'+ sLineBreak +
'(*                  --response-code       write response code for HTTP, HTTPS,*)'+ sLineBreak +
'(*                                        FTP, FTPS only                      *)'+ sLineBreak +
'(*                  --content-type        write content type                  *)'+ sLineBreak +
'(*                  --primary-ip          write primary IP address            *)'+ sLineBreak +
'(*                  --local-ip            write local IP address              *)'+ sLineBreak +
'(*                  --http-version        write HTTP version for HTTP, HTTPS  *)'+ sLineBreak +
'(*                                        protocols only                      *)'+ sLineBreak +
'(*                  --request-size        write send request size             *)'+ sLineBreak +
'(*                  --header-size         write response header size          *)'+ sLineBreak +
'(*                  --content-size        write response content size         *)'+ sLineBreak +
'(*                  --download-speed      write download speed                *)'+ sLineBreak +
'(******************************************************************************)'
  );
end;

function WriteFunctionCallback (ptr : PChar; size : LongWord; nmemb : LongWord;
  data : Pointer) : LongWord; cdecl;
begin
  Result := FBuffer.Write(ptr^, size * nmemb);
end;

procedure ParseParam (AOption : String; AValue : String);
begin
  if (AOption = '-h') or (AOption = '--help') then
    OptionsList.Append(TOptionKeyValue.Create('help', ''));

  if (AOption = '-u') or (AOption = '--username') then
    OptionsList.Append(TOptionKeyValue.Create('username', AValue));

  if (AOption = '-p') or (AOption = '--password') then
    OptionsList.Append(TOptionKeyValue.Create('password', AValue));

  if AOption = '--effective-url' then
    OptionsList.Append(TOptionKeyValue.Create('effective-url', AValue));
end;

procedure SetupParam (AParam : TOptionKeyValue);
begin
  if AParam.First = 'help' then
    PrintHelp
  else if AParam.First = 'username' then
    curl_easy_setopt(FCURL, CURLOPT_USERNAME, PAnsiChar(Utf8Encode(
    AParam.Second)))
  else if AParam.First = 'password' then
    curl_easy_setopt(FCURL, CURLOPT_PASSWORD, PAnsiChar(Utf8Encode(
    AParam.Second)))
  else if AParam.First = 'non-option' then
    curl_easy_setopt(FCURL, CURLOPT_URL, PAnsiChar(Utf8Encode(
    AParam.Second)));
end;

procedure ResponseParam (AParam : TOptionKeyValue);
begin

end;

var
  i : Integer;
  iter : TOptionKeyValue;
  Param : String;
  Scheme, StringParam : PChar;
begin
  OptionsList := TOptions.Create;

  FCURL := curl_easy_init;
  curl_easy_setopt(FCURL, CURLOPT_FOLLOWLOCATION, Longint(1));
  curl_easy_setopt(FCURL, CURLOPT_ERRORBUFFER, ErrorBuffer);
  curl_easy_setopt(FCURL, CURLOPT_WRITEFUNCTION, @WriteFunctionCallback);

  PrintHeader;

  i := 0;
  while i < ParamCount do
  begin
    Param := ParamStr(i);

    if Param.StartsWith('-') then
    begin
      ParseParam(Param, ParamStr(i + 1));
      Inc(i, 2);
    end else
    begin
      ParseParam('non-option', ParamStr(i));
      Inc(i);
    end;
  end;

  for iter in OptionsList do
    SetupParam(iter);

  if curl_easy_perform(FCURL) = CURLE_OK then
  begin
    New(Scheme);
    New(StringParam);

    curl_easy_getinfo(FCURL, CURLINFO_SCHEME, @Scheme);

    for iter in OptionsList do
      ResponseParam(iter);
  end;

  curl_easy_cleanup(FCURL);
  FreeAndNil(OptionsList);
end.

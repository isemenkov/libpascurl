unit errorstacktestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, errorstack, libpascurl;

type
  { TErrorStackTestCase }
  TErrorStackTestCase = class(TTestCase)
  published
    procedure TestStartInitialization;
    procedure TestErrorOk;
    procedure TestErrorMessages;
  end;

implementation


{ TErrorStackTestCase }

procedure TErrorStackTestCase.TestStartInitialization;
var
  errors : TErrorStack;
begin
  errors := TErrorStack.Create;
  AssertTrue('Test start initialization count', errors.Count = 0);
  FreeAndNil(errors);
end;

procedure TErrorStackTestCase.TestErrorOk;
var
  errors : TErrorStack;
begin
  errors := TErrorStack.Create;
  errors.Push(CURLE_OK);
  AssertTrue('Test CURLE_OK error', errors.Count = 0);
  FreeAndNil(errors);
end;

procedure TErrorStackTestCase.TestErrorMessages;
var
  errors : TErrorStack;
begin
  errors := TErrorStack.Create;

  errors.Push(CURLE_UNSUPPORTED_PROTOCOL);
  AssertTrue('Test CURLE_UNSUPPORTED_PROTOCOL message',
    errors.Pop = 'The URL you passed to libcurl used a protocol that this '    +
    'libcurl does not support.');

  errors.Push(CURLE_FAILED_INIT);
  AssertTrue('Test CURLE_FAILED_INIT message',
    errors.Pop = 'Very early initialization code failed.');

  errors.Push(CURLE_URL_MALFORMAT);
  AssertTrue('Test CURLE_URL_MALFORMAT message',
    errors.Pop = 'The URL was not properly formatted.');

  AssertTrue('Test errors count', errors.Count = 0);
  FreeAndNil(errors);
end;

initialization
  RegisterTest(TErrorStackTestCase);
end.


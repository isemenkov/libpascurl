unit errorstacktestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, curl.utils.errorstack, libpascurl;

type
  { TErrorStackTestCase }
  TErrorStackTestCase = class(TTestCase)
  published
    procedure TestEmptyErrorStack;
    procedure TestErrorStack;
    procedure TestErrorStackEnumerator;
  end;

implementation

procedure TErrorStackTestCase.TestEmptyErrorStack;
var
  e : TErrorStack;
begin
  e := TErrorStack.Create;

  AssertTrue('Error stack must be empty', e.Count = 0);
  AssertTrue('Error value must be none', e.Pop.IsNone);

  FreeAndNil(e);
end;

procedure TErrorStackTestCase.TestErrorStack;
var
  e : TErrorStack;
  val : TErrorStack.TOptionalString;
begin
  e := TErrorStack.Create;

  e.Push(CURLE_OK);
  AssertTrue('Error stack must be empty', e.Count = 0);
  AssertTrue('Error value must be none', e.Pop.IsNone);

  e.Push(CURLE_URL_MALFORMAT);
  AssertTrue('Error stack count must be 1', e.Count = 1);
  val := e.Pop;
  AssertTrue('Error value must be some', val.IsSome);
  AssertTrue('Incorrect error text value', val.Unwrap =
    'The URL was not properly formatted.');
  AssertTrue('Error stack must be empty', e.Count = 0);

  FreeAndNil(e);
end;

procedure TErrorStackTestCase.TestErrorStackEnumerator;
var
  e : TErrorStack;
  val : String;
  index : Integer;
begin
  e := TErrorStack.Create;

  e.Push(CURLE_OK);
  e.Push(CURLE_COULDNT_CONNECT);
  e.Push(CURLE_OBSOLETE20);
  e.Push('Test error message');

  Index := 0;
  for val in e do
  begin
    case Index of
      0 : begin
        AssertTrue('Incorrect error message CURLE_COULDNT_CONNECT',
          val = 'Failed to connect to host or proxy.');
      end;
      1 : begin
        AssertTrue('Incorrect error message CURLE_OBSOLETE20',
          val = 'These error codes will never be returned.');
      end;
      2 : begin
        AssertTrue('Incorrect custom error message',
          val = 'Test error message');
      end;
      3 : begin
        Fail('Impossible value');
      end;
    end;
    Inc(Index);
  end;

  FreeAndNil(e);
end;

initialization

  RegisterTest(TErrorStackTestCase);

end.


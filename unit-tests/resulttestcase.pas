unit resulttestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, utils.result;

type
  { TResultTestCase }
  TResultTestCase = class(TTestCase)
  public
    type
      TResultIntegerString = specialize TResult<Integer, String>;
  published
    procedure TestValueResult;
    procedure TestErrorResult;
  end;

implementation

procedure TResultTestCase.TestValueResult;
var
  res : TResultIntegerString;
  err : Boolean;
begin
  res := TResultIntegerString.CreateValue(11);
  err := False;

  AssertTrue('Result must be ok', res.IsOk);
  AssertTrue('Result error is not exists', not res.IsErr);
  AssertTrue('Result must contains value', res.Value = 11);

  try
    res.Error;
  except on E: TErrorNotExistException do
    err := True;
  end;
  AssertTrue('Result error must raise exception TErrorNotExistsException', err);

  FreeAndNil(res);
end;

procedure TResultTestCase.TestErrorResult;
var
  res : TResultIntegerString;
  err : Boolean;
begin
  res := TResultIntegerString.CreateError('Error');
  err := False;

  AssertTrue('Result must be err', not res.IsOk);
  AssertTrue('Result value must be not exists', res.IsErr);
  AssertTrue('Result must conitans error', res.Error = 'Error');

  try
    res.Value;
  except on E: TValueNotExistsException do
    err := True;
  end;
  AssertTrue('Result value must raise exception TValueNotExistsException', err);

  FreeAndNil(res);
end;

initialization

  RegisterTest(TResultTestCase);

end.


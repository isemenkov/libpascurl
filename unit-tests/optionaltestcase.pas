unit optionaltestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, utils.optional;

type
  { TOptionalTestCase }
  TOptionalTestCase = class(TTestCase)
  public
    type
      TOptionalInteger = specialize TOptional<Integer>;
      TOptionalString = specialize TOptional<String>;
  published
    procedure OptionalIntegerTestCase;
    procedure OptionalStringTestCase;
  end;

implementation

procedure TOptionalTestCase.OptionalIntegerTestCase;
var
  o : TOptionalInteger;
  err : Boolean;
begin
  o := TOptionalInteger.Create;
  AssertTrue('Optional value must be none', o.IsNone);
  err := False;
  try
    o.Unwrap;
  except on E: TNoneValueException do
    err := True;
  end;
  AssertTrue('Unwrap none is must raise exception', err);
  FreeAndNil(o);

  o := TOptionalInteger.Create(4);
  AssertTrue('Optional must be some', o.IsSome);
  AssertTrue('Incorrect optional integer value', o.Unwrap = 4);
  FreeAndNil(o);
end;

procedure TOptionalTestCase.OptionalStringTestCase;
var
  o : TOptionalString;
  err : Boolean;
begin
  o := TOptionalString.Create;
  AssertTrue('Optional value must be none', o.IsNone);
  err := False;
  try
    o.Unwrap;
  except on E: TNoneValueException do
    err := True;
  end;
  AssertTrue('Unwrap none is must raise exception', err);
  FreeAndNil(o);

  o := TOptionalString.Create('Value');
  AssertTrue('Optional must be some', o.IsSome);
  AssertTrue('Incorrect optional string value', o.Unwrap = 'Value');
  FreeAndNil(o);
end;

initialization

  RegisterTest(TOptionalTestCase);

end.


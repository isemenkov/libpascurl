unit timeintervaltestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, pascurl;

type

  { TTimeIntervalTestCase }

  TTimeIntervalTestCase = class(TTestCase)
  published
    procedure TestTimeIntervalStartInitialization;
  end;

implementation

procedure TTimeIntervalTestCase.TestTimeIntervalStartInitialization;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;
  AssertTrue('Test start initialization value us', t.Microseconds = 0);
  AssertTrue('Test start initialization value ms', t.Milliseconds = 0);
  AssertTrue('Test start initialization value s', t.Seconds = 0);
  AssertTrue('Test start initialization value m', t.Minutes = 0);
  AssertTrue('Test start initialization value h', t.Hours = 0);
  FreeAndNil(t);
end;

initialization
  RegisterTest(TTimeIntervalTestCase);
end.


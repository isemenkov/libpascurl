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
    procedure TestTimeIntervalMicroseconds;
    procedure TestTimeIntervalMilliseconds;
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

procedure TTimeIntervalTestCase.TestTimeIntervalMicroseconds;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;
  t.Microseconds := Low(TTimeInterval.TMicrosecondRange);
  AssertTrue('Test microsecond low range value',
    t.Microseconds = Low(TTimeInterval.TMicrosecondRange));
  AssertTrue('Test microsecond low range value', t.Milliseconds = 0);
  AssertTrue('Test microsecond low range value', t.Seconds = 0);
  AssertTrue('Test microsecond low range value', t.Minutes = 0);
  AssertTrue('Test microsecond low range value', t.Hours = 0);

  t.Microseconds := High(TTimeInterval.TMicrosecondRange);
  AssertTrue('Test microsecond high range value',
    t.Microseconds = High(TTimeInterval.TMicrosecondRange));
  AssertTrue('Test microsecond high range value', t.Milliseconds = 0);
  AssertTrue('Test microsecond high range value', t.Seconds = 0);
  AssertTrue('Test microsecond high range value', t.Minutes = 0);
  AssertTrue('Test microsecond high range value', t.Hours = 0);

  t.Microseconds := 100;
  AssertTrue('Test microsecond custom value', t.Microseconds = 100);
  AssertTrue('Test microsecond custom value', t.Milliseconds = 0);
  AssertTrue('Test microsecond custom value', t.Seconds = 0);
  AssertTrue('Test microsecond custom value', t.Minutes = 0);
  AssertTrue('Test microsecond custom value', t.Hours = 0);

  t.Microseconds := 1000;
  AssertTrue('Test microsecond to millisecond overflow', t.Microseconds = 0);
  AssertTrue('Test microsecond to millisecond overflow', t.Milliseconds = 1);
  AssertTrue('Test microsecond to millisecond overflow', t.Seconds = 0);
  AssertTrue('Test microsecond to millisecond overflow', t.Minutes = 0);
  AssertTrue('Test microsecond to millisecond overflow', t.Hours = 0);

  t.Microseconds := 5296540;
  AssertTrue('Test microsecond to second overflow', t.Microseconds = 540);
  AssertTrue('Test microsecond to second overflow', t.Milliseconds = 296);
  AssertTrue('Test microsecond to second overflow', t.Seconds = 5);
  AssertTrue('Test microsecond to second overflow', t.Minutes = 0);
  AssertTrue('Test microsecond to second overflow', t.Hours = 0);

  t.Microseconds := 100548351;
  AssertTrue('Test microsecond to minute overflow', t.Microseconds = 351);
  AssertTrue('Test microsecond to minute overflow', t.Milliseconds = 548);
  AssertTrue('Test microsecond to minute overflow', t.Seconds = 40);
  AssertTrue('Test microsecond to minute overflow', t.Minutes = 1);
  AssertTrue('Test microsecond to minute overflow', t.Hours = 0);

  t.Microseconds := 192627587998;
  AssertTrue('Test microsecond to hour overflow', t.Microseconds = 998);
  AssertTrue('Test microsecond to hour overflow', t.Milliseconds = 587);
  AssertTrue('Test microsecond to hour overflow', t.Seconds = 27);
  AssertTrue('Test microsecond to hour overflow', t.Minutes = 30);
  AssertTrue('Test microsecond to hour overflow', t.Hours = 53);

  FreeAndNil(t);
end;

procedure TTimeIntervalTestCase.TestTimeIntervalMilliseconds;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;
  t.Milliseconds := Low(TTimeInterval.TMillisecondRange);
  AssertTrue('Test millisecond low range value', t.Microseconds = 0);
  AssertTrue('Test millisecond low range value',
    t.Milliseconds = Low(TTimeInterval.TMillisecondRange));
  AssertTrue('Test millisecond low range value', t.Seconds = 0);
  AssertTrue('Test millisecond low range value', t.Minutes = 0);
  AssertTrue('Test millisecond low range value', t.Hours = 0);

  t.Milliseconds := High(TTimeInterval.TMillisecondRange);
  AssertTrue('Test millisecond high range value', t.Microseconds = 0);
  AssertTrue('Test millisecond high range value',
    t.Milliseconds = High(TTimeInterval.TMillisecondRange));
  AssertTrue('Test millisecond high range value', t.Seconds = 0);
  AssertTrue('Test millisecond high range value', t.Minutes = 0);
  AssertTrue('Test millisecond high range value', t.Hours = 0);

  t.Milliseconds := 47;
  AssertTrue('Test millisecond custom value', t.Microseconds = 0);
  AssertTrue('Test millisecond custom value', t.Milliseconds = 47);
  AssertTrue('Test millisecond custom value', t.Seconds = 0);
  AssertTrue('Test millisecond custom value', t.Minutes = 0);
  AssertTrue('Test millisecond custom value', t.Hours = 0);

  t.Milliseconds := 3021;
  AssertTrue('Test millisecond to second overflow', t.Microseconds = 0);
  AssertTrue('Test millisecond to second overflow', t.Milliseconds = 21);
  AssertTrue('Test millisecond to second overflow', t.Seconds = 3);
  AssertTrue('Test millisecond to second overflow', t.Minutes = 0);
  AssertTrue('Test millisecond to second overflow', t.Hours = 0);

  t.Milliseconds := 481111;
  AssertTrue('Test millisecond to minute overflow', t.Microseconds = 0);
  AssertTrue('Test millisecond to minute overflow', t.Milliseconds = 111);
  AssertTrue('Test millisecond to minute overflow', t.Seconds = 1);
  AssertTrue('Test millisecond to minute overflow', t.Minutes = 8);
  AssertTrue('Test millisecond to minute overflow', t.Hours = 0);

  t.Milliseconds := 28683452;
  AssertTrue('Test millisecond to hour overflow', t.Microseconds = 0);
  AssertTrue('Test millisecond to hour overflow', t.Milliseconds = 452);
  AssertTrue('Test millisecond to hour overflow', t.Seconds = 3);
  AssertTrue('Test millisecond to hour overflow', t.Minutes = 58);
  AssertTrue('Test millisecond to hour overflow', t.Hours = 7);

  FreeAndNil(t);
end;

initialization
  RegisterTest(TTimeIntervalTestCase);
end.


unit timeintervaltestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, timeinterval;

type

  { TTimeIntervalTestCase }

  TTimeIntervalTestCase = class(TTestCase)
  published
    procedure TestStartInitialization;
    procedure TestMicroseconds;
    procedure TestMilliseconds;
    procedure TestSeconds;
    procedure TestMinutes;
    procedure TestHours;

    procedure TestToMicroseconds;
    procedure TestToMilliseconds;
    procedure TestToSeconds;
    procedure TestToMinutes;
    procedure TestToHours;

    procedure TestToString;
  end;

implementation

procedure TTimeIntervalTestCase.TestStartInitialization;
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

procedure TTimeIntervalTestCase.TestMicroseconds;
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

procedure TTimeIntervalTestCase.TestMilliseconds;
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

procedure TTimeIntervalTestCase.TestSeconds;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;
  t.Seconds := Low(TTimeInterval.TSecondRange);
  AssertTrue('Test second low range value', t.Microseconds = 0);
  AssertTrue('Test second low range value', t.Milliseconds = 0);
  AssertTrue('Test second low range value',
    t.Seconds = Low(TTimeInterval.TSecondRange));
  AssertTrue('Test second low range value', t.Minutes = 0);
  AssertTrue('Test second low range value', t.Hours = 0);

  t.Seconds := High(TTimeInterval.TSecondRange);
  AssertTrue('Test second high range value', t.Microseconds = 0);
  AssertTrue('Test second high range value', t.Milliseconds = 0);
  AssertTrue('Test second high range value',
    t.Seconds = High(TTimeInterval.TSecondRange));
  AssertTrue('Test second high range value', t.Minutes = 0);
  AssertTrue('Test second high range value', t.Hours = 0);

  t.Seconds := 54;
  AssertTrue('Test second custom value', t.Microseconds = 0);
  AssertTrue('Test second custom value', t.Milliseconds = 0);
  AssertTrue('Test second custom value', t.Seconds = 54);
  AssertTrue('Test second custom value', t.Minutes = 0);
  AssertTrue('Test second custom value', t.Hours = 0);

  t.Seconds := 1287;
  AssertTrue('Test second to minutes overflow', t.Microseconds = 0);
  AssertTrue('Test second to minutes overflow', t.Milliseconds = 0);
  AssertTrue('Test second to minutes overflow', t.Seconds = 27);
  AssertTrue('Test second to minutes overflow', t.Minutes = 21);
  AssertTrue('Test second to minutes overflow', t.Hours = 0);

  t.Seconds := 17822;
  AssertTrue('Test second to hour overflow', t.Microseconds = 0);
  AssertTrue('Test second to hour overflow', t.Milliseconds = 0);
  AssertTrue('Test second to hour overflow', t.Seconds = 2);
  AssertTrue('Test second to hour overflow', t.Minutes = 57);
  AssertTrue('Test second to hour overflow', t.Hours = 4);

  FreeAndNil(t);
end;

procedure TTimeIntervalTestCase.TestMinutes;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;
  t.Minutes := Low(TTimeInterval.TMinuteRange);
  AssertTrue('Test minute low range value', t.Microseconds = 0);
  AssertTrue('Test minute low range value', t.Milliseconds = 0);
  AssertTrue('Test minute low range value', t.Seconds = 0);
  AssertTrue('Test minute low range value',
    t.Minutes = Low(TTimeInterval.TMinuteRange));
  AssertTrue('Test minute low range value', t.Hours = 0);

  t.Minutes := High(TTimeInterval.TMinuteRange);
  AssertTrue('Test minute high range value', t.Microseconds = 0);
  AssertTrue('Test minute high range value', t.Milliseconds = 0);
  AssertTrue('Test minute high range value', t.Seconds = 0);
  AssertTrue('Test minute high range value',
    t.Minutes = High(TTimeInterval.TMinuteRange));
  AssertTrue('Test minute high range value', t.Hours = 0);

  t.Minutes := 12;
  AssertTrue('Test minute custom value', t.Microseconds = 0);
  AssertTrue('Test minute custom value', t.Milliseconds = 0);
  AssertTrue('Test minute custom value', t.Seconds = 0);
  AssertTrue('Test minute custom value', t.Minutes = 12);
  AssertTrue('Test minute custom value', t.Hours = 0);

  t.Minutes := 539;
  AssertTrue('Test second to hour overflow', t.Microseconds = 0);
  AssertTrue('Test second to hour overflow', t.Milliseconds = 0);
  AssertTrue('Test second to hour overflow', t.Seconds = 0);
  AssertTrue('Test second to hour overflow', t.Minutes = 59);
  AssertTrue('Test second to hour overflow', t.Hours = 8);

  FreeAndNil(t);
end;

procedure TTimeIntervalTestCase.TestHours;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;

  t.Hours := 74;
  AssertTrue('Test hour custom value', t.Microseconds = 0);
  AssertTrue('Test hour custom value', t.Milliseconds = 0);
  AssertTrue('Test hour custom value', t.Seconds = 0);
  AssertTrue('Test hour custom value', t.Minutes = 0);
  AssertTrue('Test hour custom value', t.Hours = 74);

  FreeAndNil(t);
end;

procedure TTimeIntervalTestCase.TestToMicroseconds;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;

  t.Microseconds := 100;
  t.Milliseconds := 0;
  t.Seconds := 0;
  t.Minutes := 0;
  t.Hours := 0;
  AssertTrue('Test microsecond to microsecond value', t.ToMicroseconds = 100);

  t.Microseconds := 0;
  t.Milliseconds := 1;
  t.Seconds := 0;
  t.Minutes := 0;
  t.Hours := 0;
  AssertTrue('Test millisecond to microsecond value', t.ToMicroseconds = 1000);

  t.Microseconds := 540;
  t.Milliseconds := 296;
  t.Seconds := 5;
  t.Minutes := 0;
  t.Hours := 0;
  AssertTrue('Test second to microsecond value', t.ToMicroseconds = 5296540);

  t.Microseconds := 351;
  t.Milliseconds := 548;
  t.Seconds := 40;
  t.Minutes := 1;
  t.Hours := 0;
  AssertTrue('Test minute to microsecond value', t.ToMicroseconds = 100548351);

  t.Microseconds := 998;
  t.Milliseconds := 587;
  t.Seconds := 27;
  t.Minutes := 30;
  t.Hours := 53;
  AssertTrue('Test hour to microsecond value', t.ToMicroseconds = 192627587998);

  FreeAndNil(t);
end;

procedure TTimeIntervalTestCase.TestToMilliseconds;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;

  t.Microseconds := 0;
  t.Milliseconds := 47;
  t.Seconds := 0;
  t.Minutes := 0;
  t.Hours := 0;
  AssertTrue('Test millisecond to millisecond value', t.ToMilliseconds = 47);

  t.Microseconds := 1; { <- It's ok, no effect to result }
  t.Milliseconds := 21;
  t.Seconds := 3;
  t.Minutes := 0;
  t.Hours := 0;
  AssertTrue('Test second to millisecond value', t.ToMilliseconds = 3021);

  t.Microseconds := 432; { <- It's ok, no effect to result }
  t.Milliseconds := 111;
  t.Seconds := 1;
  t.Minutes := 8;
  t.Hours := 0;
  AssertTrue('Test minute to millisecond value', t.ToMilliseconds = 481111);

  t.Microseconds := 23; { <- It's ok, no effect to result }
  t.Milliseconds := 452;
  t.Seconds := 3;
  t.Minutes := 58;
  t.Hours := 7;
  AssertTrue('Test hour to millisecond value', t.ToMilliseconds = 28683452);

  FreeAndNil(t);
end;

procedure TTimeIntervalTestCase.TestToSeconds;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;

  t.Microseconds := 0;
  t.Milliseconds := 0;
  t.Seconds := 54;
  t.Minutes := 0;
  t.Hours := 0;
  AssertTrue('Test second to second value', t.ToSeconds = 54);

  t.Microseconds := 13;  { <- It's ok, no effect to result }
  t.Milliseconds := 992; { <- It's ok, no effect to result }
  t.Seconds := 27;
  t.Minutes := 21;
  t.Hours := 0;
  AssertTrue('Test minutes to second value', t.ToSeconds = 1287);

  t.Microseconds := 421; { <- It's ok, no effect to result }
  t.Milliseconds := 0;
  t.Seconds := 2;
  t.Minutes := 57;
  t.Hours := 4;
  AssertTrue('Test hour to second value', t.ToSeconds = 17822);

  FreeAndNil(t);
end;

procedure TTimeIntervalTestCase.TestToMinutes;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;
  t.Microseconds := 222; { <- It's ok, no effect to result }
  t.Milliseconds := 430; { <- It's ok, no effect to result }
  t.Seconds := 1;        { <- It's ok, no effect to result }
  t.Minutes := 12;
  t.Hours := 0;
  AssertTrue('Test minute to minute value', t.ToMinutes = 12);

  t.Microseconds := 1; { <- It's ok, no effect to result }
  t.Milliseconds := 0;
  t.Seconds := 134;    { <- It's ok, no effect to result }
  t.Minutes := 59;
  t.Hours := 8;
  AssertTrue('Test hour to minute value', t.ToMinutes = 539);

  FreeAndNil(t);
end;

procedure TTimeIntervalTestCase.TestToHours;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;
  t.Microseconds := 120; { <- It's ok, no effect to result }
  t.Milliseconds := 342; { <- It's ok, no effect to result }
  t.Seconds := 32;       { <- It's ok, no effect to result }
  t.Minutes := 1;        { <- It's ok, no effect to result }
  t.Hours := 74;
  AssertTrue('Test hour to hour value', t.ToHours = 74);

  FreeAndNil(t);
end;

procedure TTimeIntervalTestCase.TestToString;
var
  t : TTimeInterval;
begin
  t := TTimeInterval.Create;
  t.Microseconds := 0;
  t.Milliseconds := 430;
  t.Seconds := 12;
  t.Minutes := 37;
  t.Hours := 0;
  AssertTrue('Test to string value', t.ToString = '00:37:12.430000');

  t.Microseconds := 998;
  t.Milliseconds := 431;
  t.Seconds := 58;
  t.Minutes := 1;
  t.Hours := 58;
  AssertTrue('Test to string value', t.ToString = '58:01:58.431998');

  t.Microseconds := 1;
  t.Milliseconds := 3;
  t.Seconds := 7;
  t.Minutes := 9;
  t.Hours := 2;
  AssertTrue('Test to string value', t.ToString = '02:09:07.003001');

  FreeAndNil(t);
end;

initialization
  RegisterTest(TTimeIntervalTestCase);
end.


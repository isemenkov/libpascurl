unit timeintervaltestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, pascurl;

type

  { TTimeIntervalTestCase }

  TTimeIntervalTestCase = class(TTestCase)
  private
    interval : TTimeInterval;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMicrosecond;
  end;

implementation

procedure TTimeIntervalTestCase.SetUp;
begin
  interval := TTimeInterval.Create;
end;

procedure TTimeIntervalTestCase.TearDown;
begin
  FreeAndNil(interval);
end;

procedure TTimeIntervalTestCase.TestMicrosecond;
var
  ms : TTimeInterval.TMicrosecond;
begin
  ms := TTimeInterval.TMicrosecond.Create;
  AssertTrue('Test create initialization value', ms.Value = 0);



  FreeAndNil(ms);
end;

initialization

  RegisterTest(TTimeIntervalTestCase);
end.


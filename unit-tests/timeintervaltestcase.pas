unit timeintervaltestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, pascurl;

type

  { TTimeIntervalTestCase }

  TTimeIntervalTestCase = class(TTestCase)
  private
    interval : TTimeInterval;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMicrosecondStartInitialization;
    procedure TestMicrosecondAssignOperator;
    procedure TestMicrosecondAddOperator;
    procedure TestMicrosecondSubtractOperator;
    procedure TestMicrosecondMultiplyOperator;
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

procedure TTimeIntervalTestCase.TestMicrosecondStartInitialization;
var
  ms : TTimeInterval.TMicrosecond;
begin
  ms := TTimeInterval.TMicrosecond.Create;
  AssertTrue('Test start initialization value', ms.Value = 0);
  FreeAndNil(ms);

  ms := TTimeInterval.TMicrosecond.Create(QWord(1234));
  AssertTrue('Test start initialization value from QWord', ms.Value = 1234);
  FreeAndNil(ms);

  ms := TTimeInterval.TMicrosecond.Create(TTimeInterval.TMicrosecond.Create);
  AssertTrue('Test start initialization value from TMicrosecond',
    ms.Value = 0);
  FreeAndNil(ms);
end;

procedure TTimeIntervalTestCase.TestMicrosecondAssignOperator;
var
  ms : TTimeInterval.TMicrosecond;
begin
  ms := QWord(100);
  AssertTrue('Test assign operator from QWord', ms.Value = 100);
  FreeAndNil(ms);

  ms := TTimeInterval.TMicrosecond.Create(QWord(1000));
  AssertTrue('Test assign operator from TMicrosecond', ms.Value = 1000);
  FreeAndNil(ms);
end;

procedure TTimeIntervalTestCase.TestMicrosecondAddOperator;
var
  ms : TTimeInterval.TMicrosecond;
begin
  ms := TTimeInterval.TMicrosecond.Create + 100;
  AssertTrue('Test add operator for QWord', ms.Value = 100);
  FreeAndNil(ms);

  ms := TTimeInterval.TMicrosecond.Create(42) +
    TTimeInterval.TMicrosecond.Create(58);
  AssertTrue('Test add operator for TMicrosecond', ms.Value = 100);
  FreeAndNil(ms);
end;

procedure TTimeIntervalTestCase.TestMicrosecondSubtractOperator;
var
  ms : TTimeInterval.TMicrosecond;
begin
  ms := TTimeInterval.TMicrosecond.Create(100) - 52;
  AssertTrue('Test subtract operator for Qword', ms.Value = 48);
  FreeAndNil(ms);

  ms := TTimeInterval.TMicrosecond.Create(0) - 52;
  AssertTrue('Test subtract operator for QWord (negative result)',
    ms.Value = 0);
  FreeAndNil(ms);

  ms := TTimeInterval.TMicrosecond.Create(100) - TTimeInterval.TMicrosecond(21);
  AssertTrue('Test subtract operator for TMicrosecond', ms.Value = 79);
  FreeAndNil(ms);

  ms := TTimeInterval.TMicrosecond.Create(0) - TTimeInterval.TMicrosecond(21);
  AssertTrue('Test subtract operator for TMicrosecond (negative result)',
    ms.Value = 0);
  FreeAndNil(ms);
end;

procedure TTimeIntervalTestCase.TestMicrosecondMultiplyOperator;
var
  ms : TTimeInterval.TMicrosecond;
begin
  ms := TTimeInterval.TMicrosecond.Create(1) * 100;
  AssertTrue('Test multiply operator', ms.Value = 100);
  FreeAndNil(ms);

  ms := TTimeInterval.TMicrosecond.Create(0) * 100;
  AssertTrue('Test multiply operator (multiply for zero)', ms.Value = 0);
  FreeAndNil(ms);
end;


initialization
  RegisterTest(TTimeIntervalTestCase);
end.


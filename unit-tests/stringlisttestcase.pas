unit stringlisttestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, curl.utils.stringlist;

type
  { TStringListTestCase }
  TStringListTestCase = class(TTestCase)
  published
    procedure TestEmptyStringList;
    procedure TestStringList;
    procedure TestStringListEnumerator;
  end;

implementation

procedure TStringListTestCase.TestEmptyStringList;
var
  sl : curl.utils.stringlist.TStringList;
begin
  sl := TStringList.Create;

  AssertTrue('Error list must be empty', sl.IsEmpty);
  AssertTrue('Error list first value must be none', sl.Next.IsNone);

  FreeAndNil(sl);
end;

procedure TStringListTestCase.TestStringList;
var
  sl : curl.utils.stringlist.TStringList;
begin
  sl := TStringList.Create;

  sl.Add('item 1');
  AssertTrue('Error list must not be empty', not sl.IsEmpty);
  AssertTrue('Error first item value not  correct', sl.Next.Unwrap = 'item 1');

  sl.Clear;
  AssertTrue('Error list must be empty', sl.IsEmpty);

  sl.Add('item 1');
  sl.Add('item 2');
  sl.Add('item 3');
  AssertTrue('Error list must be not empty', not sl.IsEmpty);
  AssertTrue('Error first item value not correct', sl.Next.Unwrap = 'item 1');
  AssertTrue('Error second item value not correct', sl.Next.Unwrap = 'item 2');
  AssertTrue('Error third item value not correct', sl.Next.Unwrap = 'item 3');
  AssertTrue('Error fourth item value must be empty', sl.Next.IsNone);

  FreeAndNil(sl);
end;

procedure TStringListTestCase.TestStringListEnumerator;
var
  sl : curl.utils.stringlist.TStringList;
  val : String;
  index : Integer;
begin
  sl := TStringList.Create;

  sl.Add('item 1');
  sl.Add('item 2');
  sl.Add('item 3');

  index := 0;
  for val in sl do
  begin
    case Index of
      0 : begin
        AssertTrue('Not correct first list value', val = 'item 1');
      end;
      1 : begin
        AssertTrue('Not correct second list value', val = 'item 2');
      end;
      2 : begin
        AssertTrue('Not correct third list value', val = 'item 3');
      end;
      3 : begin
        Fail('Impossible value');
      end;
    end;
    Inc(index);
  end;

  FreeAndNil(sl);
end;

initialization

  RegisterTest(TStringListTestCase);

end.


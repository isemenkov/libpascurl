unit datasizetestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, utils.datasize;

type

  { TDataSizeTestCase }

  TDataSizeTestCase = class(TTestCase)
  published
    procedure TestStartInitialization;
    procedure TestBytes;
    procedure TestKilobytes;
    procedure TestMegabytes;
    procedure TestGigabytes;
    procedure TestTerabytes;

    procedure TestToBytes;
    procedure TestToKilobytes;
    procedure TestToMegabytes;
    procedure TestToGigabytes;
    procedure TestToTerabytes;

    procedure TestToString;
  end;

implementation

{ TDataSizeTestCase }

procedure TDataSizeTestCase.TestStartInitialization;
var
  s : TDataSize;
begin
  s := TDataSize.Create;
  AssertTrue('Test start initialization value B', s.Bytes = 0);
  AssertTrue('Test start initialization value KiB', s.Kilobytes = 0);
  AssertTrue('Test start initialization value MiB', s.Megabytes = 0);
  AssertTrue('Test start initialization value  GiB', s.Gigabytes = 0);
  AssertTrue('Test start initialization value TiB', s.Terabytes = 0);
  FreeAndNil(s);
end;

procedure TDataSizeTestCase.TestBytes;
var
  s : TDataSize;
begin
  s := TDataSize.Create;
  s.Bytes := Low(TDataSize.TByteRange);
  AssertTrue('Test byte low range value', s.Bytes = Low(TDataSize.TByteRange));
  AssertTrue('Test byte low range value', s.Kilobytes = 0);
  AssertTrue('Test byte low range value', s.Megabytes = 0);
  AssertTrue('Test byte low range value', s.Gigabytes = 0);
  AssertTrue('Test byte low range value', s.Terabytes = 0);

  s.Bytes := High(TDataSize.TByteRange);
  AssertTrue('Test byte high range value',
    s.Bytes = High(TDataSize.TByteRange));
  AssertTrue('Test byte high range value', s.Kilobytes = 0);
  AssertTrue('Test byte high range value', s.Megabytes = 0);
  AssertTrue('Test byte high range value', s.Gigabytes = 0);
  AssertTrue('Test byte high range value', s.Terabytes = 0);

  s.Bytes := 417;
  AssertTrue('Test byte custom value', s.Bytes = 417);
  AssertTrue('Test byte custom value', s.Kilobytes = 0);
  AssertTrue('Test byte custom value', s.Megabytes = 0);
  AssertTrue('Test byte custom value', s.Gigabytes = 0);
  AssertTrue('Test byte custom value', s.Terabytes = 0);

  s.Bytes := 5244;
  AssertTrue('Test byte to kilobyte overflow', s.Bytes = 124);
  AssertTrue('Test byte to kilobyte overflow', s.Kilobytes = 5);
  AssertTrue('Test byte to kilobyte overflow', s.Megabytes = 0);
  AssertTrue('Test byte to kilobyte overflow', s.Gigabytes = 0);
  AssertTrue('Test byte to kilobyte overflow', s.Terabytes = 0);

  s.Bytes := 49752921;
  AssertTrue('Test byte to megabyte overflow', s.Bytes = 857);
  AssertTrue('Test byte to megabyte overflow', s.Kilobytes = 458);
  AssertTrue('Test byte to megabyte overflow', s.Megabytes = 47);
  AssertTrue('Test byte to megabyte overflow', s.Gigabytes = 0);
  AssertTrue('Test byte to megabyte overflow', s.Terabytes = 0);

  s.Bytes := 4008694758;
  AssertTrue('Test byte to gigabyte overflow', s.Bytes = 998);
  AssertTrue('Test byte to gigabyte overflow', s.Kilobytes = 1012);
  AssertTrue('Test byte to gigabyte overflow', s.Megabytes = 750);
  AssertTrue('Test byte to gigabyte overflow', s.Gigabytes = 3);
  AssertTrue('Test byte to gigabyte overflow', s.Terabytes = 0);

  s.Bytes := 9104263105000;
  AssertTrue('Test byte to terabyte overflow', s.Bytes = 488);
  AssertTrue('Test byte to terabyte overflow', s.Kilobytes = 914);
  AssertTrue('Test byte to terabyte overflow', s.Megabytes = 5);
  AssertTrue('Test byte to terabyte overflow', s.Gigabytes = 287);
  AssertTrue('Test byte to terabyte overflow', s.Terabytes = 8);

  FreeAndNil(s);
end;

procedure TDataSizeTestCase.TestKilobytes;
var
  s : TDataSize;
begin
  s := TDataSize.Create;
  s.Kilobytes := Low(TDataSize.TKilobyteRange);
  AssertTrue('Test kilobytes low range value', s.Bytes = 0);
  AssertTrue('Test kilobytes low range value',
    s.Kilobytes = Low(TDataSize.TKilobyteRange));
  AssertTrue('Test kilobytes low range value', s.Megabytes = 0);
  AssertTrue('Test kilobytes low range value', s.Gigabytes = 0);
  AssertTrue('Test kilobytes low range value', s.Terabytes = 0);

  s.Kilobytes := High(TDataSize.TKilobyteRange);
  AssertTrue('Test kilobytes high range value', s.Bytes = 0);
  AssertTrue('Test kilobytes high range value',
    s.Kilobytes = High(TDataSize.TKilobyteRange));
  AssertTrue('Test kilobytes high range value', s.Megabytes = 0);
  AssertTrue('Test kilobytes high range value', s.Gigabytes = 0);
  AssertTrue('Test kilobytes high range value', s.Terabytes = 0);

  s.Kilobytes := 1010;
  AssertTrue('Test kilobytes custom value', s.Bytes = 0);
  AssertTrue('Test kilobytes custom value', s.Kilobytes = 1010);
  AssertTrue('Test kilobytes custom value', s.Megabytes = 0);
  AssertTrue('Test kilobytes custom value', s.Gigabytes = 0);
  AssertTrue('Test kilobytes custom value', s.Terabytes = 0);

  s.Kilobytes := 80901;
  AssertTrue('Test kilobyte to megabyte overflow', s.Bytes = 0);
  AssertTrue('Test kilobyte to megabyte overflow', s.Kilobytes = 5);
  AssertTrue('Test kilobyte to megabyte overflow', s.Megabytes = 79);
  AssertTrue('Test kilobyte to megabyte overflow', s.Gigabytes = 0);
  AssertTrue('Test kilobyte to megabyte overflow', s.Terabytes = 0);

  s.Kilobytes := 50329701;
  AssertTrue('Test kilobyte to gigabyte overflow', s.Bytes = 0);
  AssertTrue('Test kilobyte to gigabyte overflow', s.Kilobytes = 101);
  AssertTrue('Test kilobyte to gigabyte overflow', s.Megabytes = 1022);
  AssertTrue('Test kilobyte to gigabyte overflow', s.Gigabytes = 47);
  AssertTrue('Test kilobyte to gigabyte overflow', s.Terabytes = 0);

  s.Kilobytes := 3812916917000;
  AssertTrue('Test kilobyte to terabyte overflow', s.Bytes = 0);
  AssertTrue('Test kilobyte to terabyte overflow', s.Kilobytes = 776);
  AssertTrue('Test kilobyte to terabyte overflow', s.Megabytes = 956);
  AssertTrue('Test kilobyte to terabyte overflow', s.Gigabytes = 56);
  AssertTrue('Test kilobyte to terabyte overflow', s.Terabytes = 3551);

  FreeAndNil(s);
end;

procedure TDataSizeTestCase.TestMegabytes;
var
  s : TDataSize;
begin
  s := TDataSize.Create;
  s.Megabytes := Low(TDataSize.TMegabyteRange);
  AssertTrue('Test megabytes low range value', s.Bytes = 0);
  AssertTrue('Test megabytes low range value', s.Kilobytes = 0);
  AssertTrue('Test megabytes low range value',
    s.Megabytes = Low(TDataSize.TMegabyteRange));
  AssertTrue('Test megabytes low range value', s.Gigabytes = 0);
  AssertTrue('Test megabytes low range value', s.Terabytes = 0);

  s.Megabytes := High(TDataSize.TMegabyteRange);
  AssertTrue('Test megabytes high range value', s.Bytes = 0);
  AssertTrue('Test megabytes high range value', s.Kilobytes = 0);
  AssertTrue('Test megabytes high range value',
    s.Megabytes = High(TDataSize.TMegabyteRange));
  AssertTrue('Test megabytes high range value', s.Gigabytes = 0);
  AssertTrue('Test megabytes high range value', s.Terabytes = 0);

  s.Megabytes := 842;
  AssertTrue('Test megabytes custom value', s.Bytes = 0);
  AssertTrue('Test megabytes custom value', s.Kilobytes = 0);
  AssertTrue('Test megabytes custom value', s.Megabytes = 842);
  AssertTrue('Test megabytes custom value', s.Gigabytes = 0);
  AssertTrue('Test megabytes custom value', s.Terabytes = 0);

  s.Megabytes := 411695;
  AssertTrue('Test megabytes to gigabyte overflow', s.Bytes = 0);
  AssertTrue('Test megabytes to gigabyte overflow', s.Kilobytes = 0);
  AssertTrue('Test megabytes to gigabyte overflow', s.Megabytes = 47);
  AssertTrue('Test megabytes to gigabyte overflow', s.Gigabytes = 402);
  AssertTrue('Test megabytes to gigabyte overflow', s.Terabytes = 0);

  s.Megabytes := 4755954;
  AssertTrue('Test megabytes to terabyte overflow', s.Bytes = 0);
  AssertTrue('Test megabytes to terabyte overflow', s.Kilobytes = 0);
  AssertTrue('Test megabytes to terabyte overflow', s.Megabytes = 498);
  AssertTrue('Test megabytes to terabyte overflow', s.Gigabytes = 548);
  AssertTrue('Test megabytes to terabyte overflow', s.Terabytes = 4);

  FreeAndNil(s);
end;

procedure TDataSizeTestCase.TestGigabytes;
var
  s : TDataSize;
begin
  s := TDataSize.Create;
  s.Gigabytes := Low(TDataSize.TGigabyteRange);
  AssertTrue('Test gigabytes low range vaue', s.Bytes = 0);
  AssertTrue('Test gigabytes low range vaue', s.Kilobytes = 0);
  AssertTrue('Test gigabytes low range vaue', s.Megabytes = 0);
  AssertTrue('Test gigabytes low range vaue',
    s.Gigabytes = Low(TDataSize.TGigabyteRange));
  AssertTrue('Test gigabytes low range vaue', s.Terabytes = 0);

  s.Gigabytes := High(TDataSize.TGigabyteRange);
  AssertTrue('Test gigabytes high range vaue', s.Bytes = 0);
  AssertTrue('Test gigabytes high range vaue', s.Kilobytes = 0);
  AssertTrue('Test gigabytes high range vaue', s.Megabytes = 0);
  AssertTrue('Test gigabytes high range vaue',
    s.Gigabytes = High(TDataSize.TGigabyteRange));
  AssertTrue('Test gigabytes high range vaue', s.Terabytes = 0);

  s.Gigabytes := 476;
  AssertTrue('Test gigabytes custom value', s.Bytes = 0);
  AssertTrue('Test gigabytes custom value', s.Kilobytes = 0);
  AssertTrue('Test gigabytes custom value', s.Megabytes = 0);
  AssertTrue('Test gigabytes custom value', s.Gigabytes = 476);
  AssertTrue('Test gigabytes custom value', s.Terabytes = 0);

  s.Gigabytes := 1476;
  AssertTrue('Test gigabytes custom value', s.Bytes = 0);
  AssertTrue('Test gigabytes custom value', s.Kilobytes = 0);
  AssertTrue('Test gigabytes custom value', s.Megabytes = 0);
  AssertTrue('Test gigabytes custom value', s.Gigabytes = 452);
  AssertTrue('Test gigabytes custom value', s.Terabytes = 1);

  FreeAndNil(s);
end;

procedure TDataSizeTestCase.TestTerabytes;
var
  s : TDataSize;
begin
  s := TDataSize.Create;

  s.Terabytes := 5532;
  AssertTrue('Test gigabytes custom value', s.Bytes = 0);
  AssertTrue('Test gigabytes custom value', s.Kilobytes = 0);
  AssertTrue('Test gigabytes custom value', s.Megabytes = 0);
  AssertTrue('Test gigabytes custom value', s.Gigabytes = 0);
  AssertTrue('Test gigabytes custom value', s.Terabytes = 5532);

  FreeAndNil(s);
end;

procedure TDataSizeTestCase.TestToBytes;
var
  s : TDataSize;
begin
  s := TDataSize.Create;
  s.Bytes := 417;
  s.Kilobytes := 0;
  s.Megabytes := 0;
  s.Gigabytes := 0;
  s.Terabytes := 0;
  AssertTrue('Test byte to byte value', s.ToBytes = 417);

  s.Bytes := 124;
  s.Kilobytes := 5;
  s.Megabytes := 0;
  s.Gigabytes := 0;
  s.Terabytes := 0;
  AssertTrue('Test kilobyte to byte value', s.ToBytes = 5244);

  s.Bytes := 857;
  s.Kilobytes := 458;
  s.Megabytes := 47;
  s.Gigabytes := 0;
  s.Terabytes := 0;
  AssertTrue('Test megabyte to byte value', s.ToBytes = 49752921);

  s.Bytes := 998;
  s.Kilobytes := 1012;
  s.Megabytes := 750;
  s.Gigabytes := 3;
  s.Terabytes := 0;
  AssertTrue('Test gigabyte to byte value', s.ToBytes = 4008694758);

  s.Bytes := 353;
  s.Kilobytes := 243;
  s.Megabytes := 548;
  s.Gigabytes := 47;
  s.Terabytes := 3;
  AssertTrue('Test terabyte to byte value', s.ToBytes = 3349575618561);

  FreeAndNil(s);
end;

procedure TDataSizeTestCase.TestToKilobytes;
var
  s : TDataSize;
begin
  s := TDataSize.Create;
  s.Bytes := 1; { <- It's ok, no effect to result }
  s.Kilobytes := 1010;
  s.Megabytes := 0;
  s.Gigabytes := 0;
  s.Terabytes := 0;
  AssertTrue('Test kilobyte to kilobyte value', s.ToKilobytes = 1010);

  s.Bytes := 134; { <- It's ok, no effect to result }
  s.Kilobytes := 5;
  s.Megabytes := 79;
  s.Gigabytes := 0;
  s.Terabytes := 0;
  AssertTrue('Test megabyte to kilibyte value', s.ToKilobytes = 80901);

  s.Bytes := 991; { <- It's ok, no effect to result }
  s.Kilobytes := 101;
  s.Megabytes := 1022;
  s.Gigabytes := 47;
  s.Terabytes := 0;
  AssertTrue('Test gigabyte to kilobyte value', s.ToKilobytes = 50329701);

  s.Bytes := 58; { <- It's ok, no effect to result }
  s.Kilobytes := 458;
  s.Megabytes := 588;
  s.Gigabytes := 158;
  s.Terabytes := 2;
  AssertTrue('Test terabyte to kilobyte value', s.ToKilobytes = 2313761226);

  FreeAndNil(s);
end;

procedure TDataSizeTestCase.TestToMegabytes;
var
  s : TDataSize;
begin
  s := TDataSize.Create;
  s.Bytes := 124;     { <- It's ok, no effect to result }
  s.Kilobytes := 330; { <- It's ok, no effect to result }
  s.Megabytes := 842;
  s.Gigabytes := 0;
  s.Terabytes := 0;
  AssertTrue('Test megabyte to megabyte value', s.ToMegabytes = 842);

  s.Bytes := 1003;    { <- It's ok, no effect to result }
  s.Kilobytes := 32;  { <- It's ok, no effect to result }
  s.Megabytes := 47;
  s.Gigabytes := 402;
  s.Terabytes := 0;
  AssertTrue('Test gigabyte to megabyte value', s.ToMegabytes = 411695);

  s.Bytes := 432;     { <- It's ok, no effect to result }
  s.Kilobytes := 412; { <- It's ok, no effect to result }
  s.Megabytes := 435;
  s.Gigabytes := 543;
  s.Terabytes := 1;
  AssertTrue('Test gigabyte to megabyte value', s.ToMegabytes = 1605043);

  FreeAndNil(s);
end;

procedure TDataSizeTestCase.TestToGigabytes;
var
  s : TDataSize;
begin
  s := TDataSize.Create;
  s.Bytes := 854;     { <- It's ok, no effect to result }
  s.Kilobytes := 52;  { <- It's ok, no effect to result }
  s.Megabytes := 456; { <- It's ok, no effect to result }
  s.Gigabytes := 476;
  s.Terabytes := 0;
  AssertTrue('Test gigabyte to gigabyte value', s.ToGigabytes = 476);

  s.Bytes := 1022;     { <- It's ok, no effect to result }
  s.Kilobytes := 532;  { <- It's ok, no effect to result }
  s.Megabytes := 854;  { <- It's ok, no effect to result }
  s.Gigabytes := 464;
  s.Terabytes := 2;
  AssertTrue('Test gigabyte to gigabyte value', s.ToGigabytes = 2512);

  FreeAndNil(s);
end;

procedure TDataSizeTestCase.TestToTerabytes;
var
  s : TDataSize;
begin
  s := TDataSize.Create;
  s.Bytes := 531;      { <- It's ok, no effect to result }
  s.Kilobytes := 613;  { <- It's ok, no effect to result }
  s.Megabytes := 875;  { <- It's ok, no effect to result }
  s.Gigabytes := 546;  { <- It's ok, no effect to result }
  s.Terabytes := 2;
  AssertTrue('Test terabyte to terabyte value', s.ToTerabytes = 2);

  s.Bytes := 531;      { <- It's ok, no effect to result }
  s.Kilobytes := 613;  { <- It's ok, no effect to result }
  s.Megabytes := 875;  { <- It's ok, no effect to result }
  s.Gigabytes := 546;  { <- It's ok, no effect to result }
  s.Terabytes := 23421;
  AssertTrue('Test terabyte to terabyte value', s.ToTerabytes = 23421);

  FreeAndNil(s);
end;

procedure TDataSizeTestCase.TestToString;
var
  s : TDataSize;
begin
  s := TDataSize.Create;
  s.Bytes := 432;
  s.Kilobytes := 0;
  s.Megabytes := 0;
  s.Gigabytes := 0;
  s.Terabytes := 0;
  AssertTrue('Test to string value', s.ToString = '432 B');

  s.Bytes := 432;
  s.Kilobytes := 543;
  s.Megabytes := 0;
  s.Gigabytes := 0;
  s.Terabytes := 0;
  AssertTrue('Test to string value', s.ToString = '543,432 KiB');

  s.Bytes := 432;
  s.Kilobytes := 543;
  s.Megabytes := 43;
  s.Gigabytes := 0;
  s.Terabytes := 0;
  AssertTrue('Test to string value', s.ToString = '43,543 MiB');

  s.Bytes := 432;
  s.Kilobytes := 543;
  s.Megabytes := 43;
  s.Gigabytes := 647;
  s.Terabytes := 0;
  AssertTrue('Test to string value', s.ToString = '647,43 GiB');

  s.Bytes := 642;
  s.Kilobytes := 765;
  s.Megabytes := 425;
  s.Gigabytes := 64;
  s.Terabytes := 20;
  AssertTrue('Test to string value', s.ToString = '20,64 TiB');

  s.Bytes := 432;
  s.Kilobytes := 543;
  s.Megabytes := 43;
  s.Gigabytes := 647;
  s.Terabytes := 0;
  AssertTrue('Test to string value', s.ToString('/s') = '647,43 GiB/s');

  FreeAndNil(s);
end;

initialization

  RegisterTest(TDataSizeTestCase);
end.


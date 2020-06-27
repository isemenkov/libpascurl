(******************************************************************************)
(*                                 libPasCURL                                 *)
(*                 object pascal wrapper around cURL library                  *)
(*                        https://github.com/curl/curl                        *)
(*                                                                            *)
(* Copyright (c) 2020                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpascurl                  ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* This source  is free software;  you can redistribute  it and/or modify  it *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation; either version 3 of the License.                      *)
(*                                                                            *)
(* This code is distributed in the  hope that it will  be useful, but WITHOUT *)
(* ANY  WARRANTY;  without even  the implied  warranty of MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License for *)
(* more details.                                                              *)
(*                                                                            *)
(* A copy  of the  GNU General Public License is available  on the World Wide *)
(* Web at <http://www.gnu.org/copyleft/gpl.html>. You  can also obtain  it by *)
(* writing to the Free Software Foundation, Inc., 51  Franklin Street - Fifth *)
(* Floor, Boston, MA 02110-1335, USA.                                         *)
(*                                                                            *)
(******************************************************************************)

unit utils.datasize;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  SysUtils;

type
  { TDataSize }

  TDataSize = class
  public
    type
      TByteRange = 0 .. 1023;
      TKilobyteRange = 0 .. 1023;
      TMegabyteRange = 0 .. 1023;
      TGigabyteRange = 0 .. 1023;
      TTerabyteRange = type QWord;

      { TByte }

      TByte = class
      private
        FBytes : TByteRange;
      public
        constructor Create;
        constructor Create (ASize : TByteRange);
        constructor Create (ASize : TByte);
        destructor Destroy; override;

        property Value : TByteRange read FBytes write FBytes;
      end;

      { TKilobyte }

      TKilobyte = class
      private
        FKilobytes : TKilobyteRange;
      public
        constructor Create;
        constructor Create (ASize : TKilobyteRange);
        constructor Create (ASize : TKilobyte);
        destructor Destroy; override;

        property Value : TKilobyteRange read FKilobytes write FKilobytes;
      end;

      { TMegabyte }

      TMegabyte = class
      private
        FMegabytes : TMegabyteRange;
      public
        constructor Create;
        constructor Create (ASize : TMegabyteRange);
        constructor Create (ASize : TMegabyte);
        destructor Destroy; override;

        property Value : TMegabyteRange read FMegabytes write FMegabytes;
      end;

      { TGigabyte }

      TGigabyte = class
      private
        FGigabytes : TGigabyteRange;
      public
        constructor Create;
        constructor Create (ASize : TGigabyteRange);
        constructor Create (ASize : TGigabyte);
        destructor Destroy; override;

        property Value : TGigabyteRange read FGigabytes write FGigabytes;
      end;

      { TTerabyte }

      TTerabyte = class
      private
        FTerabytes : TTerabyteRange;
      public
        constructor Create;
        constructor Create (ASize : TTerabyteRange);
        constructor Create (ASize : TTerabyte);
        destructor Destroy; override;

        property Value : TTerabyteRange read FTerabytes write FTerabytes;
      end;

  private
    FBytes : TByte;
    FKilobytes : TKilobyte;
    FMegabytes : TMegabyte;
    FGigabytes : TGigabyte;
    FTerabytes : TTerabyte;

    function GetBytes : QWord; 
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    procedure SetBytes (ASize : QWord); 
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    function GetKilobytes : QWord; 
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    procedure SetKilobytes (ASize : QWord); 
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    function GetMegabytes : QWord; 
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    procedure SetMegabytes (ASize : QWord); 
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    function GetGigabytes : QWord; 
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    procedure SetGigabytes (ASize : QWord); 
      {$IFNDEF DEBUG}inline;{$ENDIF}

    function GetTerabytes : QWord;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    procedure SetTerabytes (ASize : QWord);
      {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function ToBytes : QWord; 
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    function ToKilobytes : QWord; 
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    function ToMegabytes : QWord; 
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    function ToGigabytes : QWord; 
      {$IFNDEF DEBUG}inline;{$ENDIF}

    function ToTerabytes : QWord;
      {$IFNDEF DEBUG}inline;{$ENDIF}
    
    function {%H-}ToString (ASuffix : string = '') : string;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    property Bytes : QWord read GetBytes write SetBytes;
    property b : QWord read GetBytes write SetBytes;
    property Kilobytes : QWord read GetKilobytes write SetKilobytes;
    property KiB : QWord read GetKilobytes write SetKilobytes;
    property Megabytes : QWord read GetMegabytes write SetMegabytes;
    property MiB : QWord read GetMegabytes write SetMegabytes;
    property Gigabytes : QWord read GetGigabytes write SetGigabytes;
    property GiB : QWord read GetGigabytes write SetGigabytes;
    property Terabytes : QWord read GetTerabytes write SetTerabytes;
    property TiB : QWord read GetTerabytes write SetTerabytes;
  end;

implementation

{ TDataSize }

function TDataSize.GetBytes: QWord;
begin
  Result := FBytes.Value;
end;

procedure TDataSize.SetBytes(ASize: QWord);
begin
  if ASize <= High(TByteRange) then
  begin
    FBytes.Value := ASize;
  end else
  begin
    FBytes.Value := ASize mod 1024;
    Kilobytes := ASize div 1024;
  end;
end;

function TDataSize.GetKilobytes: QWord;
begin
  Result := FKilobytes.Value;
end;

procedure TDataSize.SetKilobytes(ASize: QWord);
begin
  if ASize <= High(TKilobyteRange) then
  begin
    FKilobytes.Value := ASize;
  end else
  begin
    FKilobytes.Value := ASize mod 1024;
    Megabytes := ASize div 1024;
  end;
end;

function TDataSize.GetMegabytes: QWord;
begin
  Result := FMegabytes.Value;
end;

procedure TDataSize.SetMegabytes(ASize: QWord);
begin
  if ASize <= High(TMegabyteRange) then
  begin
    FMegabytes.Value := ASize;
  end else
  begin
    FMegabytes.Value := ASize mod 1024;
    Gigabytes := ASize div 1024;
  end;
end;

function TDataSize.GetGigabytes: QWord;
begin
  Result := FGigabytes.Value;
end;

procedure TDataSize.SetGigabytes(ASize: QWord);
begin
  if ASize <= High(TGigabyteRange) then
  begin
    FGigabytes.Value := ASize;
  end else
  begin
    FGigabytes.Value := ASize mod 1024;
    Terabytes := ASize div 1024;
  end;
end;

function TDataSize.GetTerabytes : QWord;
begin
  Result := FTerabytes.Value;
end;

procedure TDataSize.SetTerabytes (ASize : QWord);
begin
  FTerabytes.Value := ASize;
end;

constructor TDataSize.Create;
begin
  FBytes := TByte.Create;
  FKilobytes := TKilobyte.Create;
  FMegabytes := TMegabyte.Create;
  FGigabytes := TGigabyte.Create;
  FTerabytes := TTerabyte.Create;
end;

destructor TDataSize.Destroy;
begin
  FreeAndNil(FBytes);
  FreeAndNil(FKilobytes);
  FreeAndNil(FMegabytes);
  FreeAndNil(FGigabytes);
  FreeAndNil(FTerabytes);

  inherited Destroy;
end;

function TDataSize.ToBytes: QWord;
begin
  Result := 0;
  if FTerabytes.Value > 0 then
    Result := Result + (FTerabytes.Value * 1099511628000);
  if FGigabytes.Value > 0 then
    Result := Result + (FGigabytes.Value * 1073741824);
  if FMegabytes.Value > 0 then
    Result := Result + (FMegabytes.Value * 1048576);
  if FKilobytes.Value > 0 then
    Result := Result + (FKilobytes.Value * 1024);
  if FBytes.Value > 0 then
    Result := Result + FBytes.Value;
end;

function TDataSize.ToKilobytes: QWord;
begin
  Result := 0;
  if FTerabytes.Value > 0 then
    Result := Result + (FTerabytes.Value * 1073741824);
  if FGigabytes.Value > 0 then
    Result := Result + (FGigabytes.Value * 1048576);
  if FMegabytes.Value > 0 then
    Result := Result + (FMegabytes.Value * 1024);
  if FKilobytes.Value > 0 then
    Result := Result + FKilobytes.Value;
end;

function TDataSize.ToMegabytes: QWord;
begin
  Result := 0;
  if FTerabytes.Value > 0 then
    Result := Result + (FTerabytes.Value * 1048576);
  if FGigabytes.Value > 0 then
    Result := Result + (FGigabytes.Value * 1024);
  if FMegabytes.Value > 0 then
    Result := Result + FMegabytes.Value;
end;

function TDataSize.ToGigabytes: QWord;
begin
  Result := 0;
  if FTerabytes.Value > 0 then
    Result := Result + (FTerabytes.Value * 1024);
  if FGigabytes.Value > 0 then  
    Result := Result + FGigabytes.Value;
end;

function TDataSize.ToTerabytes : QWord;
begin
  Result := FTerabytes.Value;
end;

function TDataSize.ToString (ASuffix : string): string;
begin
  if FTerabytes.Value > 0 then
  begin
    Result := Format('%0.2d,%0.2d',
      [FTerabytes.Value, FGigabytes.Value]) + ' TiB' + ASuffix;
  end else
  if FGigabytes.Value > 0 then
  begin
    Result := Format('%0.2d,%0.2d',
      [FGigabytes.Value, FMegabytes.Value]) + ' GiB' + ASuffix;
  end else
  if FMegabytes.Value > 0 then
  begin
    Result := Format('%0.2d,%0.2d',
      [FMegabytes.Value, FKilobytes.Value]) + ' MiB' + ASuffix;
  end else
  if FKilobytes.Value > 0 then
  begin
    Result := Format('%0.2d,%0.2d',
      [FKilobytes.Value, FBytes.Value]) + ' KiB' + ASuffix;
  end else
  begin
    Result := Format('%0.2d', [FBytes.Value]) + ' B' + ASuffix;
  end;
end;

{ TDataSize.TTerabyte }

constructor TDataSize.TTerabyte.Create;
begin
  FTerabytes := 0;
end;

constructor TDataSize.TTerabyte.Create(ASize : TTerabyteRange);
begin
  FTerabytes := ASize; 
end;

constructor TDataSize.TTerabyte.Create(ASize : TTerabyte);
begin
  FTerabytes := ASize.Value;
end;

destructor TDataSize.TTerabyte.Destroy;
begin
  inherited Destroy;
end;

{ TDataSize.TGigabyte }

constructor TDataSize.TGigabyte.Create;
begin
  FGigabytes := 0;
end;

constructor TDataSize.TGigabyte.Create(ASize: TGigabyteRange);
begin
  FGigabytes := ASize;
end;

constructor TDataSize.TGigabyte.Create(ASize: TGigabyte);
begin
  FGigabytes := ASize.Value;
end;

destructor TDataSize.TGigabyte.Destroy;
begin
  inherited Destroy;
end;

{ TDataSize.TMegabyte }

constructor TDataSize.TMegabyte.Create;
begin
  FMegabytes := 0;
end;

constructor TDataSize.TMegabyte.Create(ASize: TMegabyteRange);
begin
  FMegabytes := ASize;
end;

constructor TDataSize.TMegabyte.Create(ASize: TMegabyte);
begin
  FMegabytes := ASize.Value;
end;

destructor TDataSize.TMegabyte.Destroy;
begin
  inherited Destroy;
end;

{ TDataSize.TKilobyte }

constructor TDataSize.TKilobyte.Create;
begin
  FKilobytes := 0;
end;

constructor TDataSize.TKilobyte.Create(ASize: TKilobyteRange);
begin
  FKilobytes := ASize;
end;

constructor TDataSize.TKilobyte.Create(ASize: TKilobyte);
begin
  FKilobytes := ASize.Value;
end;

destructor TDataSize.TKilobyte.Destroy;
begin
  inherited Destroy;
end;

{ TDataSize.TByte }

constructor TDataSize.TByte.Create;
begin
  FBytes := 0;
end;

constructor TDataSize.TByte.Create(ASize: TByteRange);
begin
  FBytes := ASize;
end;

constructor TDataSize.TByte.Create(ASize: TByte);
begin
  FBytes := ASize.Value;
end;

destructor TDataSize.TByte.Destroy;
begin
  inherited Destroy;
end;


end.


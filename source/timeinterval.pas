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
(* Module:          Unit 'pascurl'                                            *)
(* Functionality:                                                             *)
(*                                                                            *)
(*                                                                            *)
(*                                                                            *)
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

unit timeinterval;

{$mode objfpc}{$H+}
{$IFOPT D+}
  {$DEFINE DEBUG}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  { TTimeInterval }

  TTimeInterval = class
  public
    type
      TMicrosecondRange = 0 .. 999;
      TMillisecondRange = 0 .. 999;
      TSecondRange = 0 .. 59;
      TMinuteRange = 0 .. 59;
      THourRange = type QWord;

      { TMicrosecond }

      TMicrosecond = class
      private
        FMicroseconds : TMicrosecondRange;
      public
        constructor Create;
        constructor Create (AInterval : TMicrosecondRange);
        constructor Create (AInterval : TMicrosecond);
        destructor Destroy; override;

        property Value : TMicrosecondRange read FMicroseconds
          write FMicroseconds;
      end;

      { TMillisecond }

      TMillisecond = class
      private
        FMilliseconds : TMillisecondRange;
      public
        constructor Create;
        constructor Create (AInterval : TMillisecondRange);
        constructor Create (AInterval : TMillisecond);
        destructor Destroy; override;

        property Value : TMillisecondRange read FMilliseconds
          write FMilliseconds;
      end;

      { TSecond }

      TSecond = class
      private
        FSeconds : TSecondRange;
      public
        constructor Create;
        constructor Create (AInterval : TSecondRange);
        constructor Create (AInterval : TSecond);
        destructor Destroy; override;

        property Value : TSecondRange read FSeconds write FSeconds;
      end;

      { TMinute }

      TMinute = class
      private
        FMinutes : TMinuteRange;
      public
        constructor Create;
        constructor Create (AInterval : TMinuteRange);
        constructor Create (AInterval : TMinute);
        destructor Destroy; override;

        property Value : TMinuteRange read FMinutes write FMinutes;
      end;

      { THour }

      THour = class
      private
        FHours : THourRange;
      public
        constructor Create;
        constructor Create (AInterval : THourRange);
        constructor Create (AInterval : THour);
        destructor Destroy; override;

        property Value : THourRange read FHours write FHours;
      end;
  private
    FMicroseconds : TMicrosecond;
    FMilliseconds : TMillisecond;
    FSeconds : TSecond;
    FMinutes : TMinute;
    FHours : THour;

    function GetMicroseconds : QWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetMicroseconds (AValue : Qword); {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetMilliseconds : QWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetMilliseconds (AValue : QWord); {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetSeconds : QWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetSeconds (AValue : QWord); {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetMinutes : QWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetMinutes (AValue : QWord); {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetHours : QWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetHours (AValue : QWord); {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function ToMicroseconds : QWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    function ToMilliseconds : QWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    function ToSeconds : QWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    function ToMinutes : QWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    function ToHours : QWord; {$IFNDEF DEBUG}inline;{$ENDIF}
    function {%H-}ToString(ASuffix : string = '') : string;
      {$IFNDEF DEBUG}inline;{$ENDIF}

    property Microseconds : QWord read GetMicroseconds write SetMicroseconds;
    property us : QWord read GetMicroseconds write SetMicroseconds;
    property Milliseconds : QWord read GetMilliseconds write SetMilliseconds;
    property ms : QWord read GetMilliseconds write SetMilliseconds;
    property Seconds : QWord read GetSeconds write SetSeconds;
    property s : QWord read GetSeconds write SetSeconds;
    property Minutes : QWord read GetMinutes write SetMinutes;
    property m : QWord read GetMinutes write SetMinutes;
    property Hours : QWord read GetHours write SetHours;
    property h : QWord read GetHours write SetHours;
  end;

implementation

{ TTimeInterval }

constructor TTimeInterval.Create;
begin
  FMicroseconds := TMicrosecond.Create;
  FMilliseconds := TMillisecond.Create;
  FSeconds := TSecond.Create;
  FMinutes := TMinute.Create;
  FHours := THour.Create;
end;

destructor TTimeInterval.Destroy;
begin
  FreeAndNil(FMicroseconds);
  FreeAndNil(FMilliseconds);
  FreeAndNil(FSeconds);
  FreeAndNil(FMinutes);
  FreeAndNil(FHours);
  inherited Destroy;
end;

function TTimeInterval.GetMicroseconds: QWord;
begin
  Result := FMicroseconds.Value;
end;

procedure TTimeInterval.SetMicroseconds(AValue: Qword);
begin
  if AValue <= High(TMicrosecondRange) then
  begin
    FMicroseconds.Value := AValue;
  end else
  begin
    FMicroseconds.Value := AValue mod 1000;
    Milliseconds := AValue div 1000;
  end;
end;

function TTimeInterval.GetMilliseconds: QWord;
begin
  Result := FMilliseconds.Value;
end;

procedure TTimeInterval.SetMilliseconds(AValue: QWord);
begin
  if AValue <= High(TMillisecondRange) then
  begin
    FMilliseconds.Value := AValue;
  end else
  begin
    FMilliseconds.Value := AValue mod 1000;
    Seconds := AValue div 1000;
  end;
end;

function TTimeInterval.GetSeconds: QWord;
begin
  Result := FSeconds.Value;
end;

procedure TTimeInterval.SetSeconds(AValue: QWord);
begin
  if AValue <= High(TSecondRange) then
  begin
    FSeconds.Value := AValue;
  end else
  begin
    FSeconds.Value := AValue mod 60;
    Minutes := AValue div 60;
  end;
end;

function TTimeInterval.GetMinutes: QWord;
begin
  Result := FMinutes.Value;
end;

procedure TTimeInterval.SetMinutes(AValue: QWord);
begin
  if AValue <= High(TMinuteRange) then
  begin
    FMinutes.Value := AValue;
  end else
  begin
    FMinutes.Value := AValue mod 60;
    Hours := AValue div 60;
  end;
end;

function TTimeInterval.GetHours: QWord;
begin
  Result := FHours.Value;
end;

procedure TTimeInterval.SetHours(AValue: QWord);
begin
  FHours.Value := AValue;
end;

function TTimeInterval.ToString(ASuffix : string): string;
begin
  Result := Format('%0.2d:%0.2d:%0.2d.%0.3d%0.3d',
    [FHours.Value, FMinutes.Value, FSeconds.Value, FMilliseconds.Value,
    FMicroseconds.Value]) + ASuffix;
end;

function TTimeInterval.ToMicroseconds: QWord;
begin
  Result := 0;
  if FHours.Value > 0 then
    Result := Result + (FHours.Value * 3600000000);
  if FMinutes.Value > 0 then
    Result := Result + (FMinutes.Value * 60000000);
  if FSeconds.Value > 0 then
    Result := Result + (FSeconds.Value * 1000000);
  if FMilliseconds.Value > 0 then
    Result := Result + (FMilliseconds.Value * 1000);
  if FMicroseconds.Value > 0 then
    Result := Result + FMicroseconds.Value;
end;

function TTimeInterval.ToMilliseconds: QWord;
begin
  Result := 0;
  if FHours.Value > 0 then
    Result := Result + (FHours.Value * 3600000);
  if FMinutes.Value > 0 then
    Result := Result + (FMinutes.Value * 60000);
  if FSeconds.Value > 0 then
    Result := Result + (FSeconds.Value * 1000);
  if FMilliseconds.Value > 0 then
    Result := Result + FMilliseconds.Value;
end;

function TTimeInterval.ToSeconds: QWord;
begin
  Result := 0;
  if FHours.Value > 0 then
    Result := Result + (FHours.Value * 3600);
  if FMinutes.Value > 0 then
    Result := Result + (FMinutes.Value * 60);
  if FSeconds.Value > 0 then
    Result := Result + FSeconds.Value;
end;

function TTimeInterval.ToMinutes: QWord;
begin
  Result := 0;
  if FHours.Value > 0 then
    Result := Result + (FHours.Value * 60);
  if FMinutes.Value > 0 then
    Result := Result + FMinutes.Value;
end;

function TTimeInterval.ToHours: QWord;
begin
  Result := FHours.Value;
end;

{ TTimeInterval.THour }

constructor TTimeInterval.THour.Create;
begin
  FHours := 0;
end;

constructor TTimeInterval.THour.Create(AInterval: THour);
begin
  FHours := AInterval.FHours;
end;

constructor TTimeInterval.THour.Create(AInterval: THourRange);
begin
  FHours := AInterval;
end;

destructor TTimeInterval.THour.Destroy;
begin
  inherited Destroy;
end;

{ TTimeInterval.TMinute }

constructor TTimeInterval.TMinute.Create;
begin
  FMinutes := 0;
end;

constructor TTimeInterval.TMinute.Create(AInterval: TMinuteRange);
begin
  FMinutes := AInterval;
end;

constructor TTimeInterval.TMinute.Create(AInterval: TMinute);
begin
  FMinutes := AInterval.FMinutes;
end;

destructor TTimeInterval.TMinute.Destroy;
begin
  inherited Destroy;
end;

{ TTimeInterval.TSecond }

constructor TTimeInterval.TSecond.Create;
begin
  FSeconds := 0;
end;

constructor TTimeInterval.TSecond.Create(AInterval: TSecondRange);
begin
  FSeconds := AInterval;
end;

constructor TTimeInterval.TSecond.Create(AInterval: TSecond);
begin
  FSeconds := AInterval.FSeconds;
end;

destructor TTimeInterval.TSecond.Destroy;
begin
  inherited Destroy;
end;

{ TTimeInterval.TMillisecond }

constructor TTimeInterval.TMillisecond.Create;
begin
  FMilliseconds := 0;
end;

constructor TTimeInterval.TMillisecond.Create(AInterval: TMillisecondRange);
begin
  FMilliseconds := AInterval;
end;

constructor TTimeInterval.TMillisecond.Create(AInterval: TMillisecond);
begin
  FMilliseconds := AInterval.FMilliseconds;
end;

destructor TTimeInterval.TMillisecond.Destroy;
begin
  inherited Destroy;
end;

{ TTimeInterval.TMicrosecond }

constructor TTimeInterval.TMicrosecond.Create;
begin
  FMicroseconds := 0;
end;

constructor TTimeInterval.TMicrosecond.Create(AInterval: TMicrosecondRange);
begin
  FMicroseconds := AInterval;
end;

constructor TTimeInterval.TMicrosecond.Create(AInterval: TMicrosecond);
begin
  FMicroseconds := AInterval.FMicroseconds;
end;

destructor TTimeInterval.TMicrosecond.Destroy;
begin
  inherited Destroy;
end;

end.


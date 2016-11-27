unit pxl.boards.mpsse;
{
  This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
  Copyright (c) 2000 - 2016  Yuriy Kotsarenko
  Ultibo port Copyright (c) 2016 Garry Wood <garry@softoz.com.au>

  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
  }

{$mode objfpc}{$H+}
{$INCLUDE PXL.Config.inc}

interface

uses
    SysUtils, D2xxUnit, FTDI_Constants, PXL.TypeDef, PXL.Boards.Types;

type
    { I/O mode typically used in GPIO pins. }
  TPinMode = (
    { Pin set for input / high impedance }
    Input,
    { Pin set for output }
    Output);

  { Digital value of the pin. }
  TPinValue = (
    { Low (0) or zero voltage. }
    Low,
    { High (1) or full voltage. }
    High);

  TPinModeEx = (Input = GPIO_FUNCTION_IN,
                Output = GPIO_FUNCTION_OUT,
                Alt0 = GPIO_FUNCTION_ALT0,
                Alt1 = GPIO_FUNCTION_ALT1,
                Alt2 = GPIO_FUNCTION_ALT2,
                Alt3 = GPIO_FUNCTION_ALT3,
                Alt4 = GPIO_FUNCTION_ALT4,
                Alt5 = GPIO_FUNCTION_ALT5);

  TFTDISystemCore = class(TCustomSystemCore)
  strict private
    FBaseClock: LongWord;
    FPageSize: LongWord;
  public
    property BaseClock:LongWord read FBaseClock;
    property PageSize:LongWord read FPageSize;
  public
    constructor Create;
    destructor Destroy; override;

    { Returns the current value of system timer as 64-bit unsigned integer, in microseconds. }
    function GetTickCount: UInt64; override;

    { Waits for the specified amount of microseconds, calling NanoSleep if waiting time is long enough for the most
      portion of wait time, while the remaining part doing a busy wait to provide greater accuracy. }
    procedure Delay(const MicroSeconds: Cardinal); override;
  end;

  TMpsseGPIO = class(TCustomGPIO)
  strict private
    FSystemCore: TFTDISystemCore;
    FGPIO: PGPIODevice;

    function GetPinModeEx(const Pin: Integer): TPinModeEx;
    procedure SetPinModeEx(const Pin: Integer; const Value: TPinModeEx);
    function Sync_To_MPSSE : boolean;
    function Init_Controller(DName : String) : boolean;
  protected
    function GetPinMode(const Pin: Integer): TPinMode; override;
    procedure SetPinMode(const Pin: Integer; const Mode: TPinMode); override;

    function GetPinValue(const Pin: Integer): TPinValue; override;
    procedure SetPinValue(const Pin: Integer; const Value: TPinValue); override;
  public
    constructor Create(const ASystemCore: TUltiboSystemCore; AGPIO: PGPIODevice = nil);
    destructor Destroy; override;

    { Quickly changes specified pin value (assuming it is set for output).}
    procedure SetFastValue(const Pin: Integer; const Value: TPinValue);

    { Reference to @link(TUltiboSystemCore), which provides timing and delay utilities. }
    property SystemCore: TUltiboSystemCore read FSystemCore;

    { Determine what GPIO device this instance is connected to. }
    property GPIO: PGPIODevice read FGPIO;

    { Provides control and feedback of currently selected mode for the given pin, including alternative functions. }
    property PinModeEx[const Pin: Integer]: TPinModeEx read GetPinModeEx write SetPinModeEx;
  end;

  TMpsseSPI = class(TCustomPortSPI)
  public const
    DefaultChipSelect = 0;
    DefaultFrequency = 8000000;
    DefaultMode = 0;
  strict private
    FSPI: PSPIDevice;

    FClockPhase: LongWord;
    FClockPolarity: LongWord;
  private
    FMode: Integer;
    FFrequency: Integer;
    FChipSelectAttributes: TChipSelectAttributes;
    FChipSelect: Integer;

    procedure SetChipSelect(const Value: Integer);
  protected
    function GetMode: Integer; override;
    procedure SetMode(const Value: Integer); override;
    function GetBitsPerWord: Integer; override;
    procedure SetBitsPerWord(const Value: Integer); override;
    function GetFrequency: Integer; override;
    procedure SetFrequency(const Value: Integer); override;
    function GetChipSelectAttributes: TChipSelectAttributes; override;
    procedure SetChipSelectAttributes(const Value: TChipSelectAttributes); override;
  public
    constructor Create(ASPI: PSPIDevice = nil; const AChipSelect: Integer = DefaultChipSelect;
                       const AFrequency: Integer = DefaultFrequency; const AMode: Integer = DefaultMode);
    destructor Destroy; override;

    function Read(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Write(const Buffer: Pointer; const BufferSize: Integer): Integer; override;
    function Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Integer): Integer; override;

    { Determine what SPI device this instance is connected to. }
    property SPI: PSPIDevice read FSPI;

    { Defines clock polarity and phase for SPI operation. }
    property Mode: Integer read FMode write SetMode;

    { Controls the operating frequency of SPI bus in Hz, default value for SPI is 8 mHz. }
    property Frequency: Integer read FFrequency write SetFrequency;

    { Defines what Chip Select line to enable during transfers. Supported values are SPI_CS_0 to SPI_CS_15. }
    property ChipSelect: Integer read FChipSelect write SetChipSelect;

    { Determines how Chip Select line is handled by protocol. }
    property ChipSelectAttributes: TChipSelectAttributes read FChipSelectAttributes write SetChipSelectAttributes;
  end;

implementation

constructor TFTDISystemCore.Create;
begin
  inherited;

  FBaseClock := $00000h;
  FPageSize := MemoryGetPageSize;
end;

constructor TMpsseGPIO.Create(const ASystemCore: TFTDISystemCore; AGPIO: PGPIODevice);
begin
  inherited Create;

  FSystemCore := ASystemCore;
  if FSystemCore = nil then
    raise ESystemCoreRefRequired.Create(SSystemCoreRefNotProvided);

  FGPIO := AGPIO;
  if FGPIO = nil then
    FGPIO := GPIODeviceGetDefault;

end;

destructor TMpsseGPIO.Destroy;
begin
  inherited;
end;

function TMpsseGPIO.GetPinModeEx(const Pin: Integer): TPinModeEx;
begin
  case GPIODeviceFunctionGet(FGPIO, Pin) of
    GPIO_FUNCTION_IN:
      Result := TPinModeEx.Input;
    GPIO_FUNCTION_OUT:
      Result := TPinModeEx.Output;
    GPIO_FUNCTION_ALT0:
      Result := TPinModeEx.Alt0;
    GPIO_FUNCTION_ALT1:
      Result := TPinModeEx.Alt1;
    GPIO_FUNCTION_ALT2:
      Result := TPinModeEx.Alt2;
    GPIO_FUNCTION_ALT3:
      Result := TPinModeEx.Alt3;
    GPIO_FUNCTION_ALT4:
      Result := TPinModeEx.Alt4;
    GPIO_FUNCTION_ALT5:
      Result := TPinModeEx.Alt5;
  else
   raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));
  end;
end;

procedure TMpsseGPIO.SetPinModeEx(const Pin: Integer; const Value: TPinModeEx);
begin
  GPIODeviceFunctionSelect(FGPIO, Pin, LongWord(Value));
end;

function TMpsseGPIO.GetPinMode(const Pin: Integer): TPinMode;
begin
  case GetPinModeEx(Pin) of
    TPinModeEx.Input:
      Result := TPinMode.Input;
    TPinModeEx.Output:
      Result := TPinMode.Output;
  else
    raise EGPIOAlternateFunctionPin.Create(Format(SGPIOSpecifiedPinAlternativeMode, [Pin]));
  end;
end;

procedure TMpsseGPIO.SetPinMode(const Pin: Integer; const Mode: TPinMode);
begin
  if Mode = TPinMode.Output then
    SetPinModeEx(Pin, TPinModeEx.Output)
  else
    SetPinModeEx(Pin, TPinModeEx.Input);
end;

function TMpsseGPIO.GetPinValue(const Pin: Integer): TPinValue;
begin
  case GPIODeviceInputGet(FGPIO, Pin) of
    GPIO_LEVEL_LOW:
      Result := TPinValue.Low;
    GPIO_LEVEL_HIGH:
      Result := TPinValue.High;
  else
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));
  end;
end;

procedure TMpsseGPIO.SetPinValue(const Pin: Integer; const Value: TPinValue);
begin
  if Value = TPinValue.Low then
    GPIODeviceOutputSet(FGPIO, Pin, GPIO_LEVEL_LOW)
  else
    GPIODeviceOutputSet(FGPIO, Pin, GPIO_LEVEL_HIGH);
end;

function TMpsseGPIO.Sync_To_MPSSE : boolean;
//
// This should satisfy outstanding commands.
//
// We will use $AA and $AB as commands which
// are invalid so that the MPSSE block should echo these
// back to us preceded with an $FA
//
var res : FT_Result;
i,j : integer;
Done : boolean;
begin
Sync_To_MPSSE := false;
res := Get_USB_Device_QueueStatus;  {from D2XXunit}
if res <> FT_OK then exit;
if (FT_Q_Bytes > 0) then
  i := Read_USB_Device_Buffer(FT_Q_Bytes);  {from D2XXunit}
  repeat
  OutIndex := 0;
  AddToBuffer($AA); // bad command
  SendBytes(OutIndex);
  res := Get_USB_Device_QueueStatus;
  until (FT_Q_Bytes > 0) or (res <> FT_OK); // or timeout
if res <> FT_OK then exit;
i := Read_USB_Device_Buffer(FT_Q_Bytes);
j := 0;
Done := False;
  repeat
  if (FT_In_Buffer[j] = $FA) then
    begin
    if (j < (i-2)) then
      begin
      if (FT_In_Buffer[j+1] = $AA) then Done := true;
      end;
    end;
  j := j + 1;
  until (j=i) or Done;
OutIndex := 0;
AddToBuffer($AB); // bad command
SendBytes(OutIndex);
  repeat
  res := Get_USB_Device_QueueStatus;
  until (FT_Q_Bytes > 0) or (res <> FT_OK); // or timeout
if res <> FT_OK then exit;
i := Read_USB_Device_Buffer(FT_Q_Bytes);
j := 0;
Done := False;
  repeat
  if (FT_In_Buffer[j] = $FA) then
    begin
    if (j <= (i-2)) then
      begin
      if (FT_In_Buffer[j+1] = $AB) then Done := true;
      end;
    end;
  j := j + 1;
  until (j=i) or Done;

if Done then Sync_To_MPSSE := true;
end;

function TMpsseGPIO.Init_Controller(DName : String) : boolean;
var passed : boolean;
res : FT_Result;
begin
Init_Controller := false;
passed := OpenPort(DName);
if passed then
  begin
  res := Set_USB_Device_LatencyTimer(16);
  res := Set_USB_Device_BitMode($00,$00); // reset controller
  res := Set_USB_Device_BitMode($00,$02); // enable JTAG controller
  passed := Sync_To_MPSSE;
  end;
if passed then
  begin
  OutIndex := 0;
//  sleep(20); // wait for all the USB stuff to complete
  AddToBuffer($80); // set SK,DO,CS as out
  AddToBuffer($00); // all low
  Saved_Port_Value := $00;
  AddToBuffer($0B); // inputs on GPIO11-14
  AddToBuffer($82); // outputs on GPIO21-24
  AddToBuffer($0F);
  AddToBuffer($0F);
  AddToBuffer($86); // set clk divisor
  AddToBuffer(speed AND $FF);
  AddToBuffer(speed SHR 8);
  // turn off loop back
  AddToBuffer($85);
  SendBytes(OutIndex); // send off the command
  Init_Controller := true;
  end;
end;

end.


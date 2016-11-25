unit FTDI_Constants;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, D2XXUnit, myDSSPI;

implementation
const

ft2232h_A_msb = [clkOut , sdOut, sdIn, ssOut, gpioL0, gpioL1, gpioL2, gpioL3];
ft2232h_A_lsb = [clkOut, sdOut, sdIn, ssOut, gpioL0, gpioL1, gpioL2, gpioL3];
ft2232h_B_msb = [BUS0, BUS1, BUS2, BUS3, BUS4, BUS5, BUS6, BUS7];
ft2232h_B_lsb = [BUS0, BUS1, BUS2, BUS3, BUS4, BUS5, BUS6, BUS7];

SetClockDivisor = 086h; //0x86,0xValueL,0xValueH
                        //TCK/SK period = 60MHz / (( 1 +[(0xValueH * 256) OR 0xValueL] ) * 2)

MSB_CmdOps = [MSB_BytesOutRising     = $010h,
              MSB_BytesOutFalling    = $011h,
              MSB_BitsOutRising      = $012h,
              MSB_BitsOutFalling     = $013h
              MSB_BytesInRising      = $020h,
              MSB_BytesInFalling     = $024h,
              MSB_BitsInRising       = $022h,
              MSB_BitsInFalling      = $026h,
              MSB_BytesInOutRising   = $031h,
              MSB_BytesInOutFalling  = $034h,
              MSB_BitsInOutRising    = $033h,
              MSB_BitsInOutFalling   = $036h];

LSB_CmdOps = [LSB_BytesOutRising     = $018h,
              LSB_BytesOutFalling    = $019h,
              LSB_BitsOutRising      = $01ah,
              LSB_BitsOutFalling     = $01bh,
              LSB_BytesInRising      = $028h,
              LSB_BytesInFalling     = $02ch,
              LSB_BitsInRising       = $02ah,
              LSB_BitsInFalling      = $02eh,
              LSB_BytesInOutRising   = $039h,
              LSB_BytesInOutFalling  = $03ch,
              LSB_BitsInOutRising    = $03bh,
              LSB_BitsInOutFalling   = $03eh];

Pin_CmdOps = [Set_Lbyte = $080h;
              Set_Hbyte = $082h;
              Read_Lbyte = $081h;
              read_Hbyte = $083h;]



begin
end.


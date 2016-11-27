unit FTDI_Constants;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, D2XXUnit;

implementation
type

//SetClockDivisor = $086h; //0x86,0xValueL,0xValueH
                        //TCK/SK period = 60MHz / (( 1 +[(0xValueH * 256) OR 0xValueL] ) * 2)

MSB_CmdOps = (MSB_BytesOutRising     = $010,
              MSB_BytesOutFalling    = $011,
              MSB_BitsOutRising      = $012,
              MSB_BitsOutFalling     = $013,
              MSB_BytesInRising      = $020,
              MSB_BytesInFalling     = $024,
              MSB_BitsInRising       = $022,
              MSB_BitsInFalling      = $026,
              MSB_BytesInOutRising   = $031,
              MSB_BytesInOutFalling  = $034,
              MSB_BitsInOutRising    = $033,
              MSB_BitsInOutFalling   = $036);

LSB_CmdOps = (LSB_BytesOutRising     = $018,
              LSB_BytesOutFalling    = $019,
              LSB_BitsOutRising      = $01a,
              LSB_BitsOutFalling     = $01b,
              LSB_BytesInRising      = $028,
              LSB_BytesInFalling     = $02c,
              LSB_BitsInRising       = $02a,
              LSB_BitsInFalling      = $02e,
              LSB_BytesInOutRising   = $039,
              LSB_BytesInOutFalling  = $03c,
              LSB_BitsInOutRising    = $03b,
              LSB_BitsInOutFalling   = $03e);

Pin_CmdOps = (Set_Lbyte = $080,
              Set_Hbyte = $082,
              Read_Lbyte = $081,
              read_Hbyte = $083);

type

Tchanelmask = (clkOut, sdOut, sdIn, ssOut, gpioL0, gpioL1, gpioL2, gpioL3);
ft2232h_A_lsb = (clkOut, sdOut, sdIn, ssOut, gpioL0, gpioL1, gpioL2, gpioL3);
ft2232h_B_msb = (BUS0, BUS1, BUS2, BUS3, BUS4, BUS5, BUS6, BUS7);
ft2232h_B_lsb = (BUS0, BUS1, BUS2, BUS3, BUS4, BUS5, BUS6, BUS7);



begin
end.


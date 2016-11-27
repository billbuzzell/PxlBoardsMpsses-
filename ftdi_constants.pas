unit FTDI_Constants;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, D2XXUnit;

const

  SetClockDivisor  = $086; //0x86,0xValueL,0xValueH
                           //TCK/SK period = 60MHz / (( 1 +[(0xValueH * 256) OR 0xValueL] ) * 2)
  EnableClkDivide5 = $08B; //enable clock divide by 5
  Enable3PhaseClk  = $08C;
  Disable3PhaseClk = $08D;
  ClkNoDataBit     = $08E;
  ClkNoDataByte    = $08F;
  GPIOL1_ClkNoDat1 = $094;
  GPIOL1_ClkNoDat0 = $095;
  En_AdaptiveClk   = $096;
  Dis_AdaptiveClk  = $097;
  Clk_NbitsHigh    = $09C;
  Clk_NbitsLow     = $09D;

type

  Pin_CmdOps = (Set_Lbyte = $080,
                Read_Lbyte = $081,
                Set_Hbyte = $082,
                Read_Hbyte = $083);

  MSB_CmdOps = (MSB_BytesOutRising     = $010,
                MSB_BytesOutFalling    = $011,
                MSB_BitsOutRising      = $012,
                MSB_BitsOutFalling     = $013,
                MSB_BytesInRising      = $020,
                MSB_BitsInRising       = $022,
                MSB_BytesInFalling     = $024,
                MSB_BitsInFalling      = $026,
                MSB_BytesInOutRising   = $031,
                MSB_BitsInOutRising    = $033,
                MSB_BytesInOutFalling  = $034,
                MSB_BitsInOutFalling   = $036);

  LSB_CmdOps = (LSB_BytesOutRising     = $018,
                LSB_BytesOutFalling    = $019,
                LSB_BitsOutRising      = $01a,
                LSB_BitsOutFalling     = $01b,
                LSB_BytesInRising      = $028,
                LSB_BitsInRising       = $02a,
                LSB_BytesInFalling     = $02c,
                LSB_BitsInFalling      = $02e,
                LSB_BytesInOutRising   = $039,
                LSB_BitsInOutRising    = $03b,
                LSB_BytesInOutFalling  = $03c,
                LSB_BitsInOutFalling   = $03e);


  ft2232h_A_msb = (clkOut_m, sdOut_m, sdIn_m, ssOut_m, gpioL0_m, gpioL1_m, gpioL2_m, gpioL3_m);
  ft2232h_A_lsb = (clkOut_l, sdOut_l, sdIn_l, ssOut_l, gpioL0_l, gpioL1_l, gpioL2_l, gpioL3_l);
  ft2232h_B_msb = (BUS0_m, BUS1_m, BUS2_m, BUS3_m, BUS4_m, BUS5_m, BUS6_m, BUS7_m);
  ft2232h_B_lsb = (BUS0_l, BUS1_l, BUS2_l, BUS3_l, BUS4_l, BUS5_l, BUS6_l, BUS7_l);

implementation

begin
end.


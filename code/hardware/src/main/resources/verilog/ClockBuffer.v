/**
* Wrapper for Xilinx BUFCE (glitchless clock gating cell)
*/
module ClockBuffer (
    input  wire CE,  // clock enable (active low)
    input  wire I,   // input (source) clock
    output wire O    // output clock
);

`ifdef VERILATOR
  // simulation purpose only
  wire input_clk;
  reg  output_clk;
  assign input_clk = I;
  always @(input_clk) begin
    if (input_clk == 1'b1 && CE == 1'b0) begin
      output_clk = 1'b1;
    end else if (input_clk == 1'b0) begin
      output_clk = 1'b0;
    end
  end
  assign O = output_clk;
`else

  wire iclk;
  // the actual deal
  BUFGCE #(
      .CE_TYPE("SYNC"),  // SYNC, ASYNC
      .IS_CE_INVERTED(1'b1),  // Programmable inversion on CE
      .IS_I_INVERTED(1'b0)  // Programmable inversion on I
  ) impl (
      .O (O),   // 1-bit output: Buffer
      .CE(CE),  // 1-bit input: Buffer enable
      .I (iclk)    // 1-bit input: Buffer
  );

  // MMCME3_ADV: Advanced Mixed Mode Clock Manager (MMCM)
  // UltraScale
  // Xilinx HDL Language Template, version 2018.1
  MMCME3_ADV #(
      .BANDWIDTH("OPTIMIZED"),  // Jitter programming (HIGH, LOW, OPTIMIZED)
      .CLKFBOUT_MULT_F(5.0),  // Multiply value for all CLKOUT (2.000-64.000)
      .CLKFBOUT_PHASE(0.0),  // Phase offset in degrees of CLKFB (-360.000-360.000)
      // CLKIN_PERIOD: Input clock period in ns units, ps resolution (i.e. 33.333 is 30 MHz).
      .CLKIN1_PERIOD(5.0),
      .CLKIN2_PERIOD(5.0),

      .CLKOUT0_DIVIDE_F  (1.0),  // Divide amount for CLKOUT0 (1.000-128.000)
      // CLKOUT0_DUTY_CYCLE - CLKOUT6_DUTY_CYCLE: Duty cycle for CLKOUT outputs (0.001-0.999).
      .CLKOUT0_DUTY_CYCLE(0.5),
      .CLKOUT1_DUTY_CYCLE(0.5),
      .CLKOUT2_DUTY_CYCLE(0.5),

      .CLKOUT3_DUTY_CYCLE(0.5),

      .CLKOUT4_DUTY_CYCLE(0.5),

      .CLKOUT5_DUTY_CYCLE(0.5),
      .CLKOUT6_DUTY_CYCLE(0.5),
      // CLKOUT0_PHASE - CLKOUT6_PHASE: Phase offset for CLKOUT outputs (-360.000-360.000).
      .CLKOUT0_PHASE(0.0),

      .CLKOUT1_PHASE(0.0),

      .CLKOUT2_PHASE(0.0),

      .CLKOUT3_PHASE(0.0),
      .CLKOUT4_PHASE(0.0),
      .CLKOUT5_PHASE(0.0),

      .CLKOUT6_PHASE(0.0),

      // CLKOUT1_DIVIDE - CLKOUT6_DIVIDE: Divide amount for CLKOUT (1-128)

      .CLKOUT1_DIVIDE(1),
      .CLKOUT2_DIVIDE(1),
      .CLKOUT3_DIVIDE(1),
      .CLKOUT4_CASCADE("FALSE"),
      .CLKOUT4_DIVIDE(1),
      .CLKOUT5_DIVIDE(1),
      .CLKOUT6_DIVIDE(1),
      .COMPENSATION("AUTO"),  // AUTO, BUF_IN, EXTERNAL, INTERNAL, ZHOLD
      .DIVCLK_DIVIDE(1),  // Master division value (1-106)
      // Programmable Inversion Attributes: Specifies built-in programmable inversion on specific pins

      .IS_CLKFBIN_INVERTED(1'b0),  // Optional inversion for CLKFBIN
      .IS_CLKIN1_INVERTED(1'b0),  // Optional inversion for CLKIN1
      .IS_CLKIN2_INVERTED(1'b0),  // Optional inversion for CLKIN2
      .IS_CLKINSEL_INVERTED(1'b0),  // Optional inversion for CLKINSEL
      .IS_PSEN_INVERTED(1'b0),  // Optional inversion for PSEN
      .IS_PSINCDEC_INVERTED(1'b0),  // Optional inversion for PSINCDEC
      .IS_PWRDWN_INVERTED(1'b0),  // Optional inversion for PWRDWN
      .IS_RST_INVERTED(1'b0),  // Optional inversion for RST

      // REF_JITTER: Reference input jitter in UI (0.000-0.999).
      .REF_JITTER1(0.0),

      .REF_JITTER2(0.0),

      .STARTUP_WAIT("FALSE"),  // Delays DONE until MMCM is locked (FALSE, TRUE)
      // Spread Spectrum: Spread Spectrum Attributes
      .SS_EN("FALSE"),  // Enables spread spectrum (FALSE, TRUE)
      .SS_MODE("CENTER_HIGH"),  // CENTER_HIGH, CENTER_LOW, DOWN_HIGH, DOWN_LOW

      .SS_MOD_PERIOD(10000),  // Spread spectrum modulation period (ns) (4000-40000)
      // USE_FINE_PS: Fine phase shift enable (TRUE/FALSE)
      .CLKFBOUT_USE_FINE_PS("FALSE"),

      .CLKOUT0_USE_FINE_PS("FALSE"),

      .CLKOUT1_USE_FINE_PS("FALSE"),

      .CLKOUT2_USE_FINE_PS("FALSE"),

      .CLKOUT3_USE_FINE_PS("FALSE"),

      .CLKOUT4_USE_FINE_PS("FALSE"),

      .CLKOUT5_USE_FINE_PS("FALSE"),

      .CLKOUT6_USE_FINE_PS("FALSE")
  ) MMCME3_ADV_inst (
      // Clock Outputs outputs: User configurable clock outputs
      .CLKOUT0(iclk),  // 1-bit output: CLKOUT0
      .CLKIN1(I)  // 1-bit input: Primary clock
  );
`endif

endmodule

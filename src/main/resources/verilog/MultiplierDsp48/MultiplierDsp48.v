// A pipelined 16x16=16 multiplier with a latency of 2 cycles.
//
// The DSP48 is a pipelined DSP unit with 3 stages.
//
//   1. Optional pre-adder
//   2. Multiplier
//   3. ALU
//
// The in0 and in1 inputs are registered with 2 registers each (AREG and BREG parameters set to 2, respectively).
//  - Note that setting these parameters to 1 does not result in any pipelining! Hence why I use 2.
// The intermediate output of the multiplier is registered with 1 register (MREG parameter).
// The output of the ALU is not registered (PREG parameter).
//
// This gives us a latency of 2 cycles.
//
// Note that all pipeline registers and their clock enables are active to minimize configuration errors
// since we are not using their outputs.
// The only regsiter that we explicitly disable is PREG as we are using the P output wire.

// `timescale 1 ns

module MultiplierDsp48 (
  input               clock,
  input  [16 - 1 : 0] in0,
  input  [16 - 1 : 0] in1,
  output [16 - 1 : 0] out,
  // These ports are here to make simulations easier to understand.
  input               valid_in,
  output              valid_out
);

// Pipeline signal for simulations.
reg valid_d1, valid_d2;
assign valid_out = valid_d2;
always @(posedge clock) begin
  valid_d1 <= valid_in;
  valid_d2 <= valid_d1;
end

`ifdef VERILATOR

  reg [16 - 1 : 0] res_d1, res_d2;
  assign out = res_d2;
  always @(posedge clock) begin
    res_d1 <= in0 * in1;
    res_d2 <= res_d1;
  end

`else

  // DSP48E2 instance.
  wire              CLK;
  wire [30 - 1 : 0] A;
  wire [18 - 1 : 0] B;
  wire [48 - 1 : 0] P;
  wire [4 - 1 : 0] ALUMODE;
  wire [5 - 1 : 0] INMODE;
  wire [9 - 1 : 0] OPMODE;

  assign CLK = clock;
  assign A = in0;
  assign B = in1;

  assign ALUMODE = 4'b0000; // Z + W + X + Y + CIN (cf. UG579 Table 2-7)

  assign INMODE = 5'b00000;
  //                 |   ^ Select A1/A2 (instead of non-delayed "A0", called A)
  //                 ^     Select B1/B2 (instead of non-delayed "B0", called B)

  assign OPMODE = 9'b000000101;
  //                 |||||||^^ X multiplexer M (must be same as Y, cf. UG579 Table 2-4)
  //                 |||||^^   Y multiplexer M (must be same as X, cf. UG579 Table 2-5)
  //                 ||^^^     Z multiplexer 0
  //                 ^^        W multiplexer 0

  assign out = P;

  DSP48E2 #(
    // Feature Control Attributes: Data Path Selection
    .AMULTSEL("A"),                    // Selects A input to multiplier (A, AD)
    .A_INPUT("DIRECT"),                // Selects A input source, "DIRECT" (A port) or "CASCADE" (ACIN port)
    .BMULTSEL("B"),                    // Selects B input to multiplier (AD, B)
    .B_INPUT("DIRECT"),                // Selects B input source, "DIRECT" (B port) or "CASCADE" (BCIN port)
    .PREADDINSEL("A"),                 // Selects input to pre-adder (A, B)
    .RND(48'h000000000000),            // Rounding Constant
    .USE_MULT("MULTIPLY"),             // Select multiplier usage (DYNAMIC, MULTIPLY, NONE)
    .USE_SIMD("ONE48"),                // SIMD selection (FOUR12, ONE48, TWO24)
    .USE_WIDEXOR("FALSE"),             // Use the Wide XOR function (FALSE, TRUE)
    .XORSIMD("XOR24_48_96"),           // Mode of operation for the Wide XOR (XOR12, XOR24_48_96)
    // Pattern Detector Attributes: Pattern Detection Configuration
    .AUTORESET_PATDET("NO_RESET"),     // NO_RESET, RESET_MATCH, RESET_NOT_MATCH
    .AUTORESET_PRIORITY("RESET"),      // Priority of AUTORESET vs. CEP (CEP, RESET).
    .MASK(48'h3fffffffffff),           // 48-bit mask value for pattern detect (1=ignore)
    .PATTERN(48'h000000000000),        // 48-bit pattern match for pattern detect
    .SEL_MASK("MASK"),                 // C, MASK, ROUNDING_MODE1, ROUNDING_MODE2
    .SEL_PATTERN("PATTERN"),           // Select pattern value (C, PATTERN)
    .USE_PATTERN_DETECT("NO_PATDET"),  // Enable pattern detect (NO_PATDET, PATDET)
    // Programmable Inversion Attributes: Specifies built-in programmable inversion on specific pins
    .IS_ALUMODE_INVERTED(4'b0000),     // Optional inversion for ALUMODE
    .IS_CARRYIN_INVERTED(1'b0),        // Optional inversion for CARRYIN
    .IS_CLK_INVERTED(1'b0),            // Optional inversion for CLK
    .IS_INMODE_INVERTED(5'b00000),     // Optional inversion for INMODE
    .IS_OPMODE_INVERTED(9'b000000000), // Optional inversion for OPMODE
    .IS_RSTALLCARRYIN_INVERTED(1'b0),  // Optional inversion for RSTALLCARRYIN
    .IS_RSTALUMODE_INVERTED(1'b0),     // Optional inversion for RSTALUMODE
    .IS_RSTA_INVERTED(1'b0),           // Optional inversion for RSTA
    .IS_RSTB_INVERTED(1'b0),           // Optional inversion for RSTB
    .IS_RSTCTRL_INVERTED(1'b0),        // Optional inversion for RSTCTRL
    .IS_RSTC_INVERTED(1'b0),           // Optional inversion for RSTC
    .IS_RSTD_INVERTED(1'b0),           // Optional inversion for RSTD
    .IS_RSTINMODE_INVERTED(1'b0),      // Optional inversion for RSTINMODE
    .IS_RSTM_INVERTED(1'b0),           // Optional inversion for RSTM
    .IS_RSTP_INVERTED(1'b0),           // Optional inversion for RSTP
    // Register Control Attributes: Pipeline Register Configuration
    .ACASCREG(2),                      // Number of pipeline stages between A/ACIN and ACOUT (0-2)
    .ADREG(0),                         // Pipeline stages for pre-adder (0-1)
    .ALUMODEREG(0),                    // Pipeline stages for ALUMODE (0-1)
    .AREG(2),                          // Pipeline stages for A (0-2)
    .BCASCREG(2),                      // Number of pipeline stages between B/BCIN and BCOUT (0-2)
    .BREG(2),                          // Pipeline stages for B (0-2)
    .CARRYINREG(0),                    // Pipeline stages for CARRYIN (0-1)
    .CARRYINSELREG(0),                 // Pipeline stages for CARRYINSEL (0-1)
    .CREG(0),                          // Pipeline stages for C (0-1)
    .DREG(0),                          // Pipeline stages for D (0-1)
    .INMODEREG(0),                     // Pipeline stages for INMODE (0-1)
    .MREG(1),                          // Multiplier pipeline stages (0-1)
    .OPMODEREG(0),                     // Pipeline stages for OPMODE (0-1)
    .PREG(0)                           // Number of pipeline stages for P (0-1)
  )
  DSP48E2_inst (
    // Cascade outputs: Cascade Ports
    .ACOUT(),                          // 30-bit output: A port cascade
    .BCOUT(),                          // 18-bit output: B cascade
    .CARRYCASCOUT(),                   // 1-bit output: Cascade carry
    .MULTSIGNOUT(),                    // 1-bit output: Multiplier sign cascade
    .PCOUT(),                          // 48-bit output: Cascade output
    // Control outputs: Control Inputs/Status Bits
    .OVERFLOW(),                       // 1-bit output: Overflow in add/acc
    .PATTERNBDETECT(),                 // 1-bit output: Pattern bar detect
    .PATTERNDETECT(),                  // 1-bit output: Pattern detect
    .UNDERFLOW(),                      // 1-bit output: Underflow in add/acc
    // Data outputs: Data Ports
    .CARRYOUT(),                       // 4-bit output: Carry
    .P(P),                             // 48-bit output: Primary data
    .XOROUT(),                         // 8-bit output: XOR data
    // Cascade inputs: Cascade Ports
    .ACIN(30'b0),                      // 30-bit input: A cascade data
    .BCIN(18'b0),                      // 18-bit input: B cascade
    .CARRYCASCIN(1'b0),                // 1-bit input: Cascade carry
    .MULTSIGNIN(1'b0),                 // 1-bit input: Multiplier sign cascade
    .PCIN(48'b0),                      // 48-bit input: P cascade
    // Control inputs: Control Inputs/Status Bits
    .ALUMODE(ALUMODE),                 // 4-bit input: ALU control
    .CARRYINSEL(3'b0),                 // 3-bit input: Carry select
    .CLK(CLK),                         // 1-bit input: Clock
    .INMODE(INMODE),                   // 5-bit input: INMODE control
    .OPMODE(OPMODE),                   // 9-bit input: Operation mode
    // Data inputs: Data Ports
    .A(A),                             // 30-bit input: A data
    .B(B),                             // 18-bit input: B data
    .C(48'b0),                         // 48-bit input: C data
    .CARRYIN(1'b0),                    // 1-bit input: Carry-in
    .D(27'b0),                         // 27-bit input: D data
    // Reset/Clock Enable inputs: Reset/Clock Enable Inputs
    .CEA1(1'b1),                       // 1-bit input: Clock enable for 1st stage AREG
    .CEA2(1'b1),                       // 1-bit input: Clock enable for 2nd stage AREG
    .CEAD(1'b1),                       // 1-bit input: Clock enable for ADREG
    .CEALUMODE(1'b1),                  // 1-bit input: Clock enable for ALUMODE
    .CEB1(1'b1),                       // 1-bit input: Clock enable for 1st stage BREG
    .CEB2(1'b1),                       // 1-bit input: Clock enable for 2nd stage BREG
    .CEC(1'b1),                        // 1-bit input: Clock enable for CREG
    .CECARRYIN(1'b1),                  // 1-bit input: Clock enable for CARRYINREG
    .CECTRL(1'b1),                     // 1-bit input: Clock enable for OPMODEREG and CARRYINSELREG
    .CED(1'b1),                        // 1-bit input: Clock enable for DREG
    .CEINMODE(1'b1),                   // 1-bit input: Clock enable for INMODEREG
    .CEM(1'b1),                        // 1-bit input: Clock enable for MREG
    .CEP(1'b1),                        // 1-bit input: Clock enable for PREG
    .RSTA(1'b0),                       // 1-bit input: Reset for AREG
    .RSTALLCARRYIN(1'b0),              // 1-bit input: Reset for CARRYINREG
    .RSTALUMODE(1'b0),                 // 1-bit input: Reset for ALUMODEREG
    .RSTB(1'b0),                       // 1-bit input: Reset for BREG
    .RSTC(1'b0),                       // 1-bit input: Reset for CREG
    .RSTCTRL(1'b0),                    // 1-bit input: Reset for OPMODEREG and CARRYINSELREG
    .RSTD(1'b0),                       // 1-bit input: Reset for DREG and ADREG
    .RSTINMODE(1'b0),                  // 1-bit input: Reset for INMODEREG
    .RSTM(1'b0),                       // 1-bit input: Reset for MREG
    .RSTP(1'b0)                        // 1-bit input: Reset for PREG
  );
`endif

endmodule
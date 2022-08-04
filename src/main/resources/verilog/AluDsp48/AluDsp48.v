// A pipelined Standard ALU using DSP48E2
//
// - Determining values of ALUMODE, OPCODE, and INMODE for all operations we have to do with the DSP.
//   - OPMODE controls the W, X, Y, Z multiplexers.
//   - ALUMODE controls the function of the ALU that comes after the W, X, Y, Z multiplexers.
//   - INMODE controls the delay of A/B through multiple stages of registers dynamically.
//   - The manual is confusing about what values to use for OPMODE and ALUMODE to implement logic operations as there are
//     multiple combinations that produce the same outcome. For example, you can compute `X XOR Z` in the following ways:
//       OPMODE[3:2] | ALUMODE[3:0]
//                00 |         0100
//                00 |         0111
//                10 |         0101
//                10 |         0110
//     XOR is the only function that has so many valid configurations. The other functions only have 1 valid configuration.
//     Pg 20:
//         ```
//         When not using the first stage multiplier, the 48-bit, dual input, bit-wise logic function
//         implements AND, OR, NOT, NAND, NOR, XOR, and XNOR. The inputs to these functions are:
//         • All 0s on the W multiplexer
//         • A:B or P on the X multiplexer
//         • Either all 1s or all 0s on the Y multiplexer depending on logic operation
//         • Either C, P, or PCIN on the Z multiplexer.
//         ```
//     From the above I see that:
//     - W should always output 0, so OPMODE[8:7] = 00
//     - All logic operations must use X and Z. Multiplexer Z should be connected to C, so OPMODE[6:4] = 011
//     - A:B together is 48 bits. C is 48 bits. The result P is 48 bits. We must select the relevant parts ourselves.
//       - We can set A[26:0] = 0, B[17:16] = 0, B[15:0] = arg1
//       - We can set C[47:16] = 0, C[15:0] = arg2
//
// - Final table of control signals:
//   INMODE[4:0]     = 10001 (selects A1 and B1 when configuring AREG(2) and BREG(2))
//   CARRYINSEL[2:0] = 000   (always selects carry-in signal)
//   ```
//                 | OPMODE[8:0] | ALUMODE[3:0] | Notes
//   --------------|-------------|--------------|------------------------------
//                 |  876543210  |     3210     |
//                 |  vvvvvvvvv  |     vvvv     |
//   and(b,c)      |  000110011  |     1100     | ug579 pg 29, 38 // W = 0, X = A:B, Y = 0, Z = C // P = X AND Z
//   or(b,c)       |  000111011  |     1100     | ug579 pg 29, 38 // W = 0, X = A:B, Y = 1, Z = C // P = X OR Z
//   xor(b,c)      |  000110011  |     0100     | ug579 pg 29, 38 // W = 0, X = A:B, Y = 0, Z = C // P = X XOR Z
//   add(b,c)      |  000110011  |     0000     | ug579 pg 30, 32 // W = 0, X = A:B, Y = 0, Z = C // P = Z + W + X + Y + CIN
//   addc(b,c,cin) |  000110011  |     0000     | ug579 pg 30, 32 // W = 0, X = A:B, Y = 0, Z = C // P = Z + W + X + Y + CIN
//   sub(b,c)      |  000110011  |     0011     | ug579 pg 30, 32 // W = 0, X = A:B, Y = 0, Z = C // P = Z - (W + X + Y + CIN)
//   seq(b,c)      |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.
//   sltu(b,c)     |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.
//   slts(b,c)     |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.
//   ```
//   - SLL, SRL, SRA are handled by a dedicated unit outside the DSP.

module AluDsp48 (
  input               clock,
  input  [16 - 1 : 0] in0,
  input  [16 - 1 : 0] in1,
  input               carryin,
  input   [9 - 1 : 0] opmode,
  input   [4 - 1 : 0] alumode,
  input   [2 - 1 : 0] setinst, 
  // 0: non-set instruction
  // 1: SEQ instruction
  // 2: SLTU instruction
  // 3: SLTS instruction
  output [16 - 1 : 0] out,
  output              carryout,
  // // These ports are here to make simulations easier to understand.
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
  wire [16 - 1 : 0] result;
  wire signed [16 - 1 : 0] in0s, in1s;

  assign out = res_d2;
  assign in0s = in0;
  assign in1s = in1;
  assign result = 
    (opmode == 9'b000110011 && alumode == 4'b1100) ? in0 & in1 :
    (opmode == 9'b000111011 && alumode == 4'b1100) ? in0 | in1 :
    (opmode == 9'b000110011 && alumode == 4'b0100) ? in0 ^ in1 :
    (opmode == 9'b000110011 && alumode == 4'b0000) ? in0 + in1 + carryin :
    (setinst == 2'b01) ? {15'b0, in0 == in1} :
    (setinst == 2'b10) ? {15'b0, in0 < in1} :
    (setinst == 2'b11) ? {15'b0, in0s < in1s} :
    in0 - in1; // opmode == 9'b000110011 && alumode == 4'b0011
  always @(posedge clock) begin
    // Extend to 32 bits to ensure full-precision multiplication result.
    res_d1 <= result;
    res_d2 <= res_d1;
  end

`else
  wire [18 - 1 : 0] b_in;
  wire [48 - 1 : 0] c_in;
  wire [48 - 1 : 0] p_out;
  wire  [4 - 1 : 0] carryout4;
  wire              ismatch;

  assign b_in = 
    setinst == 2'b11 ? { {2{in0[15]}}, in0 } : // sign extension required for SLTS
    {2'b0, in0};
  assign c_in = 
    setinst == 2'b11 ? { {32{in1[15]}}, in1 } : // sign extension required for SLTS
    {32'b0, in1};

  assign out = 
    setinst == 2'b00 ? p_out[16 - 1 : 0] : // non-set inst
    setinst == 2'b01 ? {15'b0, ismatch}  : // SEQ
    {15'b0, p_out[47]};                    // SLTU, SLTS
  assign carryout = carryout4[3];

  DSP48E2 #(
    // Feature Control Attributes: Data Path Selection
    .AMULTSEL("A"),                    // Selects A input to multiplier (A, AD)
    .A_INPUT("DIRECT"),                // Selects A input source, "DIRECT" (A port) or "CASCADE" (ACIN port)
    .BMULTSEL("B"),                    // Selects B input to multiplier (AD, B)
    .B_INPUT("DIRECT"),                // Selects B input source, "DIRECT" (B port) or "CASCADE" (BCIN port)
    .PREADDINSEL("A"),                 // Selects input to pre-adder (A, B)
    .RND(48'h000000000000),            // Rounding Constant
    .USE_MULT("NONE"),                 // Select multiplier usage (DYNAMIC, MULTIPLY, NONE)
    .USE_SIMD("ONE48"),                // SIMD selection (FOUR12, ONE48, TWO24)
    .USE_WIDEXOR("FALSE"),             // Use the Wide XOR function (FALSE, TRUE)
    .XORSIMD("XOR24_48_96"),           // Mode of operation for the Wide XOR (XOR12, XOR24_48_96)
    // Pattern Detector Attributes: Pattern Detection Configuration
    .AUTORESET_PATDET("NO_RESET"),     // NO_RESET, RESET_MATCH, RESET_NOT_MATCH
    .AUTORESET_PRIORITY("RESET"),      // Priority of AUTORESET vs. CEP (CEP, RESET).
    .MASK(48'h3fffffff0000),           // 48-bit mask value for pattern detect (1=ignore)
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
    .ACASCREG(0),                      // Number of pipeline stages between A/ACIN and ACOUT (0-2)
    .ADREG(0),                         // Pipeline stages for pre-adder (0-1)
    .ALUMODEREG(0),                    // Pipeline stages for ALUMODE (0-1)
    .AREG(0),                          // Pipeline stages for A (0-2)
    .BCASCREG(1),                      // Number of pipeline stages between B/BCIN and BCOUT (0-2)
    .BREG(1),                          // Pipeline stages for B (0-2)
    .CARRYINREG(1),                    // Pipeline stages for CARRYIN (0-1)
    .CARRYINSELREG(1),                 // Pipeline stages for CARRYINSEL (0-1)
    .CREG(1),                          // Pipeline stages for C (0-1)
    .DREG(0),                          // Pipeline stages for D (0-1)
    .INMODEREG(0),                     // Pipeline stages for INMODE (0-1)
    .MREG(0),                          // Multiplier pipeline stages (0-1)
    .OPMODEREG(1),                     // Pipeline stages for OPMODE (0-1)
    .PREG(1)                           // Number of pipeline stages for P (0-1)
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
    .PATTERNDETECT(ismatch),           // 1-bit output: Pattern detect
    .UNDERFLOW(),                      // 1-bit output: Underflow in add/acc
    // Data outputs: Data Ports
    .CARRYOUT(carryout4),              // 4-bit output: Carry
    .P(p_out),                         // 48-bit output: Primary data
    .XOROUT(),                         // 8-bit output: XOR data
    // Cascade inputs: Cascade Ports
    .ACIN(30'b0),                      // 30-bit input: A cascade data
    .BCIN(18'b0),                      // 18-bit input: B cascade
    .CARRYCASCIN(1'b0),                // 1-bit input: Cascade carry
    .MULTSIGNIN(1'b0),                 // 1-bit input: Multiplier sign cascade
    .PCIN(48'b0),                      // 48-bit input: P cascade
    // Control inputs: Control Inputs/Status Bits
    .ALUMODE(alumode),                 // 4-bit input: ALU control
    .CARRYINSEL(3'b000),               // 3-bit input: Carry select
    .CLK(clock),                       // 1-bit input: Clock
    .INMODE(5'b10001),                 // 5-bit input: INMODE control
    .OPMODE(opmode),                   // 9-bit input: Operation mode
    // Data inputs: Data Ports
    .A(30'b0),                         // 30-bit input: A data
    .B(b_in),                          // 18-bit input: B data
    .C(c_in),                          // 48-bit input: C data
    .CARRYIN(carryin),                 // 1-bit input: Carry-in
    .D(27'b0),                         // 27-bit input: D data
    // Reset/Clock Enable inputs: Reset/Clock Enable Inputs
    .CEA1(1'b1),                       // 1-bit input: Clock enable for 1st stage AREG
    .CEA2(1'b1),                       // 1-bit input: Clock enable for 2nd stage AREG
    .CEAD(1'b0),                       // 1-bit input: Clock enable for ADREG
    .CEALUMODE(1'b1),                  // 1-bit input: Clock enable for ALUMODE
    .CEB1(1'b1),                       // 1-bit input: Clock enable for 1st stage BREG
    .CEB2(1'b1),                       // 1-bit input: Clock enable for 2nd stage BREG
    .CEC(1'b1),                        // 1-bit input: Clock enable for CREG
    .CECARRYIN(1'b1),                  // 1-bit input: Clock enable for CARRYINREG
    .CECTRL(1'b1),                     // 1-bit input: Clock enable for OPMODEREG and CARRYINSELREG
    .CED(1'b1),                        // 1-bit input: Clock enable for DREG
    .CEINMODE(1'b0),                   // 1-bit input: Clock enable for INMODEREG
    .CEM(1'b0),                        // 1-bit input: Clock enable for MREG
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
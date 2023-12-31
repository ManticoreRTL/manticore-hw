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
//         ? All 0s on the W multiplexer
//         ? A:B or P on the X multiplexer
//         ? Either all 1s or all 0s on the Y multiplexer depending on logic operation
//         ? Either C, P, or PCIN on the Z multiplexer.
//         ```
//     From the above I see that:
//     - W should always output 0, so OPMODE[8:7] = 00
//     - All logic operations must use X and Z. Multiplexer Z should be connected to C, so OPMODE[6:4] = 011
//     - A:B together is 48 bits. C is 48 bits. The result P is 48 bits. We must select the relevant parts ourselves.
//       - We can set A[26:0] = 0, B[17:16] = 0, B[15:0] = arg1
//       - We can set C[47:16] = 0, C[15:0] = arg2
//
// - Final table of control signals:
//   INMODE[4:0]     = 11100
//   INMODE[4]       = 1 select B1 for BMULT // use B2 for BMUX, i.e. BREG = 2
//   INMODE[3]       = 1 select A1 to be routed to PREADDINSEL mux
//   INMODE[2]       = 1 not to zero out D which is fed to BMULT
//   INMODE[1]       = 0 not to zero out B1 (requires tying A to zero)
//   INMODE[0]       = 0 to add D and A1, but since A1 is tied to zero the result is D
//   These configurations enable us to perform B1*D which takes a total of 3 cycles with MREG=1
//   For ALU opertaions we set BREG=2 and AREG=2 to for A:B as one of the ALU operands. The extra delay is due
//   to the lack of MREG before the multiplexer stage. The second operand comes from the C input, which can only
//   have a single internal register (CREG=1). Therefore, we manually insert and extra one outside.
//   Likewise the ALUMODE and OPMODE inputs need extra registers outside.
//
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
//   mul(a,b)      |  000000101  |     0000     | ug579 pg 29     // W = 0, X = M  , Y = M, Z = 0 // P = X * Y
//                 |             |              | (ALUMODE does not matter and we set it to ADD)
//   seq(b,c)      |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.
//   sltu(b,c)     |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.
//   slts(b,c)     |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.
//   ```
//   - SLL, SRL, SRA are handled by a dedicated unit outside the DSP.

module AluDsp48 (
  input               clock,
  input  [16 - 1 : 0] in0, // C
  input  [16 - 1 : 0] in1, // B
  input               carryin,
  input   [9 - 1 : 0] opmode,
  input   [4 - 1 : 0] alumode,
  input   [2 - 1 : 0] setinst,
  // 0: non-set instruction
  // 1: SEQ instruction
  // 2: SLTU instruction
  // 3: SLTS instruction
  output [16 - 1 : 0] out,
  output [32 - 1 : 0] mul_out, // multiplication has wider bit width than others
  output              carryout
);


`ifdef VERILATOR





  reg [16 - 1 : 0] res_reg1, res_reg2, res_reg3;
  reg [32 - 1 : 0] prod_reg1, prod_reg2, prod_reg3;
  reg carry_reg1, carry_reg2, carry_reg3;
  wire [16 - 1 : 0] result;
  wire [32 - 1 : 0] result_mul;
  wire signed [16 - 1 : 0] in0s, in1s;
  wire [16 : 0] sum;

  assign out = res_reg3;
  assign mul_out = prod_reg3;
  assign carryout = carry_reg3;
  assign in0s = in0;
  assign in1s = in1;
  assign result =
    (opmode == 9'b000000101 && alumode == 4'b0000) ? result_mul[15:0]:
    (opmode == 9'b000110011 && alumode == 4'b1100) ? in0 & in1 :
    (opmode == 9'b000111011 && alumode == 4'b1100) ? in0 | in1 :
    (opmode == 9'b000110011 && alumode == 4'b0100) ? in0 ^ in1 :
    (opmode == 9'b000110011 && alumode == 4'b0000) ? in0 + in1 + carryin :
    (setinst == 2'b01) ? {15'b0, in0 == in1} :
    (setinst == 2'b10) ? {15'b0, in0 < in1} :
    (setinst == 2'b11) ? {15'b0, in0s < in1s} :
    in0 - in1; // opmode == 9'b000110011 && alumode == 4'b0011
  assign result_mul = in1 * in0;
  assign sum = in0 + in1 + carryin;
  always @(posedge clock) begin
    // Extend to 32 bits to ensure full-precision multiplication result.
    res_reg1 <= result;
    res_reg2 <= res_reg1;
    res_reg3 <= res_reg2;

    carry_reg1 <= sum[16];
    carry_reg2 <= carry_reg1;
    carry_reg3 <= carry_reg2;


    prod_reg1 <= result_mul;
    prod_reg2 <= prod_reg1;
    prod_reg3 <= prod_reg2;
  end

`else


  reg [9 - 1 : 0] opmode_reg;
  reg [4 - 1 : 0] alumode_reg;
  reg [16 - 1 : 0] c_extra_reg;
  reg [1 - 1 : 0] carryin_extra_reg;
  reg [2 - 1 : 0] setinst_reg0, setinst_reg1, setinst_reg2;

  reg             in0_neg_in1_pos, in0_neg_in1_pos_reg1, in0_neg_in1_pos_reg2;
  reg             in0_in1_diff_sign, in0_in1_diff_sign_reg1, in0_in1_diff_sign_reg2;

  wire [48 - 1 : 0] p_out;
  wire              ismatch;
  wire              res_slts;

  always @(posedge clock) begin
    opmode_reg <= opmode;
    alumode_reg <= alumode;
    c_extra_reg <= in0; // c_in needs an extra reg to balance the ALU and MUL operations
    carryin_extra_reg <= carryin;


    setinst_reg0 <=  setinst;
    setinst_reg1 <= setinst_reg0;
    setinst_reg2 <= setinst_reg1;


    in0_neg_in1_pos <= in0[15] & !in1[15];
    in0_in1_diff_sign <= !in0[15] ^ in1[15];

    in0_neg_in1_pos_reg1 <= in0_neg_in1_pos;
    in0_neg_in1_pos_reg2 <= in0_neg_in1_pos_reg1;

    in0_in1_diff_sign_reg1 <= in0_in1_diff_sign;
    in0_in1_diff_sign_reg2 <= in0_in1_diff_sign_reg1;


  end

  assign res_slts = in0_neg_in1_pos_reg2 | (p_out[15] & in0_in1_diff_sign_reg2);

  // random values will change the result of non-mul so the
  // alu module should make sure in2 is zero if not MULH or MUL
  wire [18 - 1 : 0] b_in = {2'b0, in1};
  wire [48 - 1 : 0] c_in = {32'b0, c_extra_reg};
  wire [30 - 1 : 0] a_in = 0;
  wire [27 - 1 : 0] d_in = {11'b0, in0};


  assign out =
    setinst_reg2 == 2'b00 ? p_out[16 - 1 : 0]  : // non-set inst
    setinst_reg2 == 2'b01 ? {15'b0, ismatch}   : // SEQ
    setinst_reg2 == 2'b10 ? {15'b0, p_out[47]} : // SLTU
    {15'b0, res_slts};;



  assign mul_out = p_out[32 - 1 : 0];
  assign carryout = p_out[16];





  DSP48E2 #(
    // Feature Control Attributes: Data Path Selection
    .AMULTSEL("AD"),                   // Selects A input to multiplier (A, AD)
    .A_INPUT("DIRECT"),                // Selects A input source, "DIRECT" (A port) or "CASCADE" (ACIN port)
    .BMULTSEL("B"),                    // Selects B input to multiplier (AD, B)
    .B_INPUT("DIRECT"),                // Selects B input source, "DIRECT" (B port) or "CASCADE" (BCIN port)
    .PREADDINSEL("A"),                 // Selects input to pre-adder (A, B)
    .RND(48'h000000000000),            // Rounding Constant
    .USE_MULT("DYNAMIC"),              // Select multiplier usage (DYNAMIC, MULTIPLY, NONE)
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
    .USE_PATTERN_DETECT("PATDET"),     // Enable pattern detect (NO_PATDET, PATDET)
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
    .ACASCREG(1),                      // Number of pipeline stages between A/ACIN and ACOUT (0-2)
    .ADREG(0),                         // Pipeline stages for pre-adder (0-1)
    .ALUMODEREG(1),                    // Pipeline stages for ALUMODE (0-1)
    .AREG(2),                          // Pipeline stages for A (0-2)
    .BCASCREG(1),                      // Number of pipeline stages between B/BCIN and BCOUT (0-2)
    .BREG(2),                          // Pipeline stages for B (0-2)
    .CARRYINREG(1),                    // Pipeline stages for CARRYIN (0-1)
    .CARRYINSELREG(1),                 // Pipeline stages for CARRYINSEL (0-1)
    .CREG(1),                          // Pipeline stages for C (0-1)
    .DREG(1),                          // Pipeline stages for D (0-1)
    .INMODEREG(0),                     // Pipeline stages for INMODE (0-1)
    .MREG(1),                          // Multiplier pipeline stages (0-1)
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
    .CARRYOUT(),                       // 4-bit output: Carry
    .P(p_out),                         // 48-bit output: Primary data
    .XOROUT(),                         // 8-bit output: XOR data
    // Cascade inputs: Cascade Ports
    .ACIN(30'b0),                      // 30-bit input: A cascade data
    .BCIN(18'b0),                      // 18-bit input: B cascade
    .CARRYCASCIN(1'b0),                // 1-bit input: Cascade carry
    .MULTSIGNIN(1'b0),                 // 1-bit input: Multiplier sign cascade
    .PCIN(48'b0),                      // 48-bit input: P cascade
    // Control inputs: Control Inputs/Status Bits
    .ALUMODE(alumode_reg),             // 4-bit input: ALU control
    .CARRYINSEL(3'b000),               // 3-bit input: Carry select
    .CLK(clock),                       // 1-bit input: Clock
    .INMODE(5'b11100),                 // 5-bit input: INMODE control
    .OPMODE(opmode_reg),               // 9-bit input: Operation mode
    // Data inputs: Data Ports
    .A(a_in),                          // 30-bit input: A data
    .B(b_in),                          // 18-bit input: B data
    .C(c_in),                          // 48-bit input: C data
    .CARRYIN(carryin_extra_reg),       // 1-bit input: Carry-in
    .D(d_in),                          // 27-bit input: D data
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
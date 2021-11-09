/**
* Wrapper for Xilinx BUFCE (glitchless clock gating cell)
*/
module ClockDistribution (
    input wire root_clock,  // from shell
    output wire compute_clock,  // to the compute cores
    output wire control_clock,  // to the control units
    input wire compute_clock_en_n  // from the control units
);
  // assign O = I;
`ifdef VERILATOR
  // simulation purpose only
  assign control_clock = root_clock;
  reg output_clk;
  always @(root_clock) begin
    if (root_clock == 1'b1 && compute_clock_en_n == 1'b0) begin
      output_clk = 1'b1;
    end else if (root_clock == 1'b0) begin
      output_clk = 1'b0;
    end
  end
  assign compute_clock = output_clk;
`else


  // Input buffering
  //------------------------------------
  wire clk_in1_clock_regen;
  wire clk_in2_clock_regen;
  IBUF clkin1_ibuf (
      .O(clk_in1_clock_regen),
      .I(root_clock)
  );




  // Clocking PRIMITIVE
  //------------------------------------

  // Instantiation of the MMCM PRIMITIVE
  //    * Unused inputs are tied off
  //    * Unused outputs are labeled unused

  wire        clk_out1_clock_regen;
  wire        clk_out2_clock_regen;
  wire        clk_out3_clock_regen;
  wire        clk_out4_clock_regen;
  wire        clk_out5_clock_regen;
  wire        clk_out6_clock_regen;
  wire        clk_out7_clock_regen;

  wire [15:0] do_unused;
  wire        drdy_unused;
  wire        psdone_unused;
  wire        locked_int;
  wire        clkfbout_clock_regen;
  wire        clkfbout_buf_clock_regen;
  wire        clkfboutb_unused;
  wire        clkout0b_unused;
  wire        clkout1_unused;
  wire        clkout1b_unused;
  wire        clkout2_unused;
  wire        clkout2b_unused;
  wire        clkout3_unused;
  wire        clkout3b_unused;
  wire        clkout4_unused;
  wire        clkout5_unused;
  wire        clkout6_unused;
  wire        clkfbstopped_unused;
  wire        clkinstopped_unused;



  // Auto Instantiation//



  MMCME4_ADV #(
      .BANDWIDTH           ("OPTIMIZED"),
      .CLKOUT4_CASCADE     ("FALSE"),
      .COMPENSATION        ("AUTO"),
      .STARTUP_WAIT        ("FALSE"),
      .DIVCLK_DIVIDE       (1),
      .CLKFBOUT_MULT_F     (6.000),
      .CLKFBOUT_PHASE      (0.000),
      .CLKFBOUT_USE_FINE_PS("FALSE"),
      .CLKOUT0_DIVIDE_F    (6.000),
      .CLKOUT0_PHASE       (0.000),
      .CLKOUT0_DUTY_CYCLE  (0.500),
      .CLKOUT0_USE_FINE_PS ("FALSE"),
      .CLKIN1_PERIOD       (5.000)
  ) mmcme4_adv_inst
  // Output clocks
  (
      .CLKFBOUT    (clkfbout_clock_regen),
      .CLKFBOUTB   (clkfboutb_unused),
      .CLKOUT0     (clk_out1_clock_regen),
      .CLKOUT0B    (clkout0b_unused),
      .CLKOUT1     (clkout1_unused),
      .CLKOUT1B    (clkout1b_unused),
      .CLKOUT2     (clkout2_unused),
      .CLKOUT2B    (clkout2b_unused),
      .CLKOUT3     (clkout3_unused),
      .CLKOUT3B    (clkout3b_unused),
      .CLKOUT4     (clkout4_unused),
      .CLKOUT5     (clkout5_unused),
      .CLKOUT6     (clkout6_unused),
      // Input clock control
      .CLKFBIN     (clkfbout_buf_clock_regen),
      .CLKIN1      (clk_in1_clock_regen),
      .CLKIN2      (1'b0),
      // Tied to always select the primary input clock
      .CLKINSEL    (1'b1),
      // Ports for dynamic reconfiguration
      .DADDR       (7'h0),
      .DCLK        (1'b0),
      .DEN         (1'b0),
      .DI          (16'h0),
      .DO          (do_unused),
      .DRDY        (drdy_unused),
      .DWE         (1'b0),
      .CDDCDONE    (),
      .CDDCREQ     (1'b0),
      // Ports for dynamic phase shift
      .PSCLK       (1'b0),
      .PSEN        (1'b0),
      .PSINCDEC    (1'b0),
      .PSDONE      (psdone_unused),
      // Other control and status signals
      .LOCKED      (locked_int),
      .CLKINSTOPPED(clkinstopped_unused),
      .CLKFBSTOPPED(clkfbstopped_unused),
      .PWRDWN      (1'b0),
      .RST         (1'b0)
  );



  // Clock Monitor clock assigning
  //--------------------------------------
  // Output buffering
  //-----------------------------------

  BUFG clkf_buf (
      .O(clkfbout_buf_clock_regen),
      .I(clkfbout_clock_regen)
  ); // for compenstation

  BUFGCE #(
      .CE_TYPE("SYNC"),  // SYNC, ASYNC
      .IS_CE_INVERTED(1'b1),  // Programmable inversion on CE
      .IS_I_INVERTED(1'b0)  // Programmable inversion on I
  ) compute_clock_impl (
      .O(compute_clock),
      .CE(compute_clock_en_n),
      .I(clk_out1_clock_regen)
  );
  // connect the control clock to the clock tree resources
  BUFG control_clock_impl (
      .O(control_clock),
      .I(clk_out1_clock_regen)
  );



`endif

endmodule




// file: clk_wiz_0.v
//
// (c) Copyright 2008 - 2013 Xilinx, Inc. All rights reserved.
//
// This file contains confidential and proprietary information
// of Xilinx, Inc. and is protected under U.S. and
// international copyright and other intellectual property
// laws.
//
// DISCLAIMER
// This disclaimer is not a license and does not grant any
// rights to the materials distributed herewith. Except as
// otherwise provided in a valid license issued to you by
// Xilinx, and to the maximum extent permitted by applicable
// law: (1) THESE MATERIALS ARE MADE AVAILABLE "AS IS" AND
// WITH ALL FAULTS, AND XILINX HEREBY DISCLAIMS ALL WARRANTIES
// AND CONDITIONS, EXPRESS, IMPLIED, OR STATUTORY, INCLUDING
// BUT NOT LIMITED TO WARRANTIES OF MERCHANTABILITY, NON-
// INFRINGEMENT, OR FITNESS FOR ANY PARTICULAR PURPOSE; and
// (2) Xilinx shall not be liable (whether in contract or tort,
// including negligence, or under any other theory of
// liability) for any loss or damage of any kind or nature
// related to, arising under or in connection with these
// materials, including for any direct, or any indirect,
// special, incidental, or consequential loss or damage
// (including loss of data, profits, goodwill, or any type of
// loss or damage suffered as a result of any action brought
// by a third party) even if such damage or loss was
// reasonably foreseeable or Xilinx had been advised of the
// possibility of the same.
//
// CRITICAL APPLICATIONS
// Xilinx products are not designed or intended to be fail-
// safe, or for use in any application requiring fail-safe
// performance, such as life-support or safety devices or
// systems, Class III medical devices, nuclear facilities,
// applications related to the deployment of airbags, or any
// other applications that could lead to death, personal
// injury, or severe property or environmental damage
// (individually and collectively, "Critical
// Applications"). Customer assumes the sole risk and
// liability of any use of Xilinx products in Critical
// Applications, subject only to applicable laws and
// regulations governing limitations on product liability.
//
// THIS COPYRIGHT NOTICE AND DISCLAIMER MUST BE RETAINED AS
// PART OF THIS FILE AT ALL TIMES.
//
//----------------------------------------------------------------------------
// User entered comments
//----------------------------------------------------------------------------
// None
//
//----------------------------------------------------------------------------
//  Output     Output      Phase    Duty Cycle   Pk-to-Pk     Phase
//   Clock     Freq (MHz)  (degrees)    (%)     Jitter (ps)  Error (ps)
//----------------------------------------------------------------------------
// clk_out1__250.00000______0.000______50.0_______85.736_____79.008
// clk_out2__250.00000______0.000______50.0_______85.736_____79.008
//
//----------------------------------------------------------------------------
// Input Clock   Freq (MHz)    Input Jitter (UI)
//----------------------------------------------------------------------------
// __primary_____________250____________0.010



module ClockDistribution (
                           input wire root_rst_n,
                           output wire sync_rst_n,
                           input wire root_clock,         // from shell
                           output wire compute_clock,     // to the compute cores
                           output wire control_clock,     // to the control units
                           input wire compute_clock_en, // from the control units
                           output wire locked);           // to the controller
// assign O = I;
`ifdef VERILATOR
// simulation purpose only
assign control_clock = root_clock;
reg output_clk;
always @(root_clock) begin
    if (root_clock == 1'b1 && compute_clock_en == 1'b1) begin
        output_clk = 1'b1;
        end else if (root_clock == 1'b0) begin
        output_clk = 1'b0;
    end
end
assign compute_clock = output_clk;
assign locked        = 1'b1;
`else


clk_dist wiz
(
.clk_out1(control_clock),
.clk_out2(compute_clock),
.clk_out2_ce(compute_clock_en),
.locked(locked),
.clk_in1(root_clock)
);

`endif

  reg rst_sync1, rst_sync2, rst_sync3;
  wire reset_n_trigger;

  assign reset_n_trigger = root_rst_n & locked;

  always @ (posedge control_clock or negedge reset_n_trigger) begin
    if (!reset_n_trigger) begin
      rst_sync1 <= 1'b0;
      rst_sync2 <= 1'b0;
      rst_sync3 <= 1'b0;
    end else begin
      rst_sync1 <= 1'b1;
      rst_sync2 <= rst_sync1;
      rst_sync3 <= rst_sync2;
    end
   end

   assign sync_rst_n = rst_sync3;

endmodule







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
always @(negedge root_clock) begin
  output_clk = 1'b0;
end
always @(posedge root_clock) begin
  if (compute_clock_en)
    output_clk = 1'b1;
  else
    output_clk = 1'b0;
end
// always @(posedge root_clock) begin
//     if (root_clock == 1'b1 && compute_clock_en == 1'b1) begin
//         output_clk = 1'b1;
//         end else if (root_clock == 1'b0) begin
//         output_clk = 1'b0;
//     end
// end
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






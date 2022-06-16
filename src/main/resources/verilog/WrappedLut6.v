module WrappedLut6
#(
  parameter INIT = 64'h0 // defaults to out = 0
)(
  input wire clock,
  input wire we,
  input wire data,
  input wire a0,
  input wire a1,
  input wire a2,
  input wire a3,
  input wire a4,
  input wire a5,
  output wire out
);

`ifdef VERILATOR

  reg [63:0] equ = INIT;
  wire [5:0] index;
  assign index = {a5, a4, a3, a2, a1, a0};

  always @(posedge clock) begin
    if (we) begin
      equ[index] = data;
    end
  end

  assign out = equ[index];

`else

  (* DONT_TOUCH = "yes" *)
  RAM64X1S #(
    .INIT(INIT)
  )
  lutram_inst (
    .O(out),
    .A0(a0),
    .A1(a1),
    .A2(a2),
    .A3(a3),
    .A4(a4),
    .A5(a5),
    .D(data),
    .WCLK(clock),
    .WE(we)
  );
`endif

endmodule
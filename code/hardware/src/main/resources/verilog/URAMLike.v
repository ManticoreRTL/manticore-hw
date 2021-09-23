module URAMLike #(
  parameter DATA_WIDTH = 64,
  parameter ADDRESS_WIDTH = 12
)(
   input         clock,

   // read port
   input  [ADDRESS_WIDTH - 1:0] raddr,
   output [DATA_WIDTH - 1:0] dout,
   // write port
   input         wen,
   input  [ADDRESS_WIDTH - 1:0] waddr,
   input  [DATA_WIDTH - 1:0] din
 );

   (* ram_style = "ultra" *)
   reg [DATA_WIDTH - 1:0] memory [0: (1 << ADDRESS_WIDTH) - 1];
   reg [DATA_WIDTH - 1:0] dout_reg;
   reg [ADDRESS_WIDTH - 1:0] addr_reg;
   always @(posedge clock) begin
    if (wen) begin
      memory[waddr] <= din;
    end
    // addr_reg <= raddr;
    // write-first
    dout_reg <= (waddr == raddr && wen) ? din : memory[raddr];
   end
   assign dout = dout_reg;
  //  assign dout = memory[addr_reg];

endmodule
module GenericVerilogMemoryFromFile #(
  parameter DATA_WIDTH = 16,
  parameter ADDRESS_WIDTH = 11,
  parameter filename = "memory.dat",
  parameter memstyle = "block"
)(
  input         clock,
  input         wea,
  input  [ADDRESS_WIDTH - 1:0] addra,
  input  [DATA_WIDTH - 1:0] dina,
  output [DATA_WIDTH - 1:0] douta,
  input         web,
  input  [ADDRESS_WIDTH - 1:0] addrb,
  input  [DATA_WIDTH - 1:0] dinb,
  output [DATA_WIDTH - 1:0] doutb
);

  (* ram_style = memstyle *)
  reg [DATA_WIDTH - 1:0] memory [0: (1 << ADDRESS_WIDTH) - 1]; 
  reg [ADDRESS_WIDTH - 1:0] addra_pipe;
  reg [ADDRESS_WIDTH - 1:0] addrb_pipe;
  reg [DATA_WIDTH - 1:0] douta_reg;
  reg [DATA_WIDTH - 1:0] doutb_reg;

  always @(posedge clock) begin
    if (wea) begin
      memory[addra] <= dina;
    end
    if (web) begin
      memory[addrb] <= dinb;
    end
    // addra_pipe <= addra;
    // addrb_pipe <= addrb;
    douta_reg <= memory[addra];
    doutb_reg <= memory[addrb];
  end
    // assign douta = memory[addra_pipe];
    // assign doutb = memory[addrb_pipe];
    assign douta = douta_reg;
    assign doutb = doutb_reg;

  initial begin
    $readmemb(filename, memory);
  end

endmodule
module BRAMLike #(
    parameter DATA_WIDTH = 16,
    parameter ADDRESS_WIDTH = 11,
    parameter filename = ""
) (
    input clock,

    // read port
    input  [ADDRESS_WIDTH - 1:0] raddr,
    output [   DATA_WIDTH - 1:0] dout,
    // write port
    input                        wen,
    input  [ADDRESS_WIDTH - 1:0] waddr,
    input  [   DATA_WIDTH - 1:0] din
);

`ifdef VERILATOR
  (* ram_style = "block" *)
  reg [DATA_WIDTH - 1:0] memory[0:(1 << ADDRESS_WIDTH) - 1];
  reg [DATA_WIDTH - 1:0] dout_reg;
  // reg [ADDRESS_WIDTH - 1:0] addr_reg;

  always @(posedge clock) begin
    if (wen) begin
      memory[waddr] <= din;
    end
    //  write-first behavior
    //  dout_reg <= (waddr == raddr && wen) ? din : memory[raddr];
    dout_reg <= memory[raddr];
    // addr_reg <= raddr;

  end
  assign dout = dout_reg;
  //assign dout = memory[addr_reg];

  initial begin
    $readmemb(filename, memory);
  end

`else
  xpm_memory_sdpram #(
      // Common module parameters
      .MEMORY_SIZE((1 << ADDRESS_WIDTH) * DATA_WIDTH),  //positive integer
      .MEMORY_PRIMITIVE("block"),  //string; "auto", "distributed", "block" or "ultra";
      .CLOCKING_MODE("common_clock"),  //string; "common_clock", "independent_clock"
      .MEMORY_INIT_FILE("none"),  //string; "none" or "<filename>.mem"
      .MEMORY_INIT_PARAM(""),  //string;
      .USE_MEM_INIT(1),  //integer; 0,1
      .WAKEUP_TIME("disable_sleep"),  //string; "disable_sleep" or "use_sleep_pin"
      .MESSAGE_CONTROL(0),  //integer; 0,1
      .ECC_MODE ("no_ecc"), //string; "no_ecc", "encode_only", "decode_only" or "both_encode_and_decode"
      .AUTO_SLEEP_TIME(0),  //Do not Change
      // Port A module parameters
      .WRITE_DATA_WIDTH_A(DATA_WIDTH),  //positive integer
      .BYTE_WRITE_WIDTH_A(DATA_WIDTH),  //integer; 8, 9, or WRITE_DATA_WIDTH_A value
      .ADDR_WIDTH_A(ADDRESS_WIDTH),  //positive integer
      // Port B module parameters
      .READ_DATA_WIDTH_B(DATA_WIDTH),  //positive integer
      .ADDR_WIDTH_B(ADDRESS_WIDTH),  //positive integer
      .READ_RESET_VALUE_B("0"),  //string
      .READ_LATENCY_B(1),  //non-negative integer
      .WRITE_MODE_B("read_first")  //string; "write_first", "read_first", "no_change"
  ) bram_inst (
      // Common module ports
      .sleep(1'b0),
      // Port A module ports
      .clka(clock),
      .ena(1'b1),
      .wea(we),
      .addra(waddr),
      .dina(din),
      .injectsbiterra(1'b0),
      .injectdbiterra(1'b0),
      // Port B module ports
      .clkb(clock),
      .rstb(1'b0),
      .enb(1'b1),
      .regceb(1'b1),
      .addrb(raddr),
      .doutb(dout),
      .sbiterrb(1'b0),
      .dbiterrb(1'b0)
  );
`endif
endmodule

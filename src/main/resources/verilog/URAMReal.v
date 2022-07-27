module URAMReal #(
    parameter DATA_WIDTH = 16, 
    parameter ADDRESS_WIDTH = 14,
    parameter READ_LATENCY = 2
) (
    input clock, 
    // read port 
    input [ADDRESS_WIDTH - 1:0] raddr, 
    output [DATA_WIDTH - 1:0] dout, 
    // write port
    input wen,
    input [ADDRESS_WIDTH - 1:0] waddr, 
    input [DATA_WIDTH - 1:0] din
);

`ifdef VERILATOR
    (* ram_style = "ultra" *)
    reg [DATA_WIDTH - 1:0] memory [0: (1 << ADDRESS_WIDTH) - 1];
    reg [DATA_WIDTH - 1:0] dout_reg;
    reg [DATA_WIDTH - 1:0] pipes [0: READ_LATENCY - 1];
    reg [ADDRESS_WIDTH - 1:0] addr_reg;
    integer i;
    always @(posedge clock) begin
        if (wen) begin
            memory[waddr] <= din;
        end
        pipes[0] <= memory[raddr];
        for (i = 1; i < READ_LATENCY ; i = i + 1) begin
            pipes[i] <= pipes[i - 1];
        end
    end
    assign dout = pipes[READ_LATENCY - 1];
`else 
    // URAM288_BASE: 288K-bit High-Density Base Memory Building Block
    //               UltraScale
    // Xilinx HDL Language Template, version 2021.1
    // https://docs.xilinx.com/r/2021.1-English/ug974-vivado-ultrascale-libraries/URAM288_BASE

    wire DBITERR_A, DBITERR_B, SBITERR_A, SBITERR_B;
    wire [71:0] DOUT_A, DOUT_B;
    wire [71:0] DIN_B;
    wire [22:0] ADDR_A, ADDR_B;
    wire [8:0] BWE_B;

    // Port A is used for read
    assign ADDR_A = {11'b0, raddr[ADDRESS_WIDTH - 1:2]};
    assign dout = 
        raddr[1:0] == 2'b11 ? DOUT_A[63:48] :
        raddr[1:0] == 2'b10 ? DOUT_A[47:32] :
        raddr[1:0] == 2'b01 ? DOUT_A[31:16] :
        DOUT_A[15:0];
    
    // Port B is used for write
    assign ADDR_B = {11'b0, waddr[ADDRESS_WIDTH - 1:2]};
    assign DIN_B = {8'b0, din, din, din, din};
    assign BWE_B = 
        waddr[1:0] == 2'b11 ? 8'b1100_0000 :
        waddr[1:0] == 2'b10 ? 8'b0011_0000 :
        waddr[1:0] == 2'b01 ? 8'b0000_1100 :
        8'b0000_0011;

    URAM288_BASE #(
        .AUTO_SLEEP_LATENCY(8),            // Latency requirement to enter sleep mode
        .AVG_CONS_INACTIVE_CYCLES(10),     // Average consecutive inactive cycles when is SLEEP mode for power
                                           // estimation
        .BWE_MODE_A("PARITY_INTERLEAVED"), // Port A Byte write control
        .BWE_MODE_B("PARITY_INTERLEAVED"), // Port B Byte write control
        .EN_AUTO_SLEEP_MODE("FALSE"),      // Enable to automatically enter sleep mode
        .EN_ECC_RD_A("FALSE"),             // Port A ECC encoder
        .EN_ECC_RD_B("FALSE"),             // Port B ECC encoder
        .EN_ECC_WR_A("FALSE"),             // Port A ECC decoder
        .EN_ECC_WR_B("FALSE"),             // Port B ECC decoder
        .IREG_PRE_A("FALSE"),              // Optional Port A input pipeline registers
        .IREG_PRE_B("FALSE"),              // Optional Port B input pipeline registers
        .IS_CLK_INVERTED(1'b0),            // Optional inverter for CLK
        .IS_EN_A_INVERTED(1'b0),           // Optional inverter for Port A enable
        .IS_EN_B_INVERTED(1'b0),           // Optional inverter for Port B enable
        .IS_RDB_WR_A_INVERTED(1'b0),       // Optional inverter for Port A read/write select
        .IS_RDB_WR_B_INVERTED(1'b0),       // Optional inverter for Port B read/write select
        .IS_RST_A_INVERTED(1'b0),          // Optional inverter for Port A reset
        .IS_RST_B_INVERTED(1'b0),          // Optional inverter for Port B reset
        .OREG_A("TRUE"),                   // Optional Port A output pipeline registers
        .OREG_B("TRUE"),                   // Optional Port B output pipeline registers
        .OREG_ECC_A("FALSE"),              // Port A ECC decoder output
        .OREG_ECC_B("FALSE"),              // Port B output ECC decoder
        .RST_MODE_A("SYNC"),               // Port A reset mode
        .RST_MODE_B("SYNC"),               // Port B reset mode
        .USE_EXT_CE_A("FALSE"),            // Enable Port A external CE inputs for output registers
        .USE_EXT_CE_B("FALSE")             // Enable Port B external CE inputs for output registers
    ) URAM288_BASE_inst (
        .DBITERR_A(DBITERR_A),               // 1-bit output: Port A double-bit error flag status
        .DBITERR_B(DBITERR_B),               // 1-bit output: Port B double-bit error flag status
        .DOUT_A(DOUT_A),                     // 72-bit output: Port A read data output
        .DOUT_B(DOUT_B),                     // 72-bit output: Port B read data output
        .SBITERR_A(SBITERR_A),               // 1-bit output: Port A single-bit error flag status
        .SBITERR_B(SBITERR_B),               // 1-bit output: Port B single-bit error flag status
        .ADDR_A(ADDR_A),                     // 23-bit input: Port A address
        .ADDR_B(ADDR_B),                     // 23-bit input: Port B address
        .BWE_A(9'b0),                        // 9-bit input: Port A Byte-write enable
        .BWE_B(BWE_B),                       // 9-bit input: Port B Byte-write enable
        .CLK(clock),                         // 1-bit input: Clock source
        .DIN_A(72'b0),                       // 72-bit input: Port A write data input
        .DIN_B(DIN_B),                       // 72-bit input: Port B write data input
        .EN_A(1'b1),                         // 1-bit input: Port A enable
        .EN_B(1'b1),                         // 1-bit input: Port B enable
        .INJECT_DBITERR_A(1'b0),             // 1-bit input: Port A double-bit error injection
        .INJECT_DBITERR_B(1'b0),             // 1-bit input: Port B double-bit error injection
        .INJECT_SBITERR_A(1'b0),             // 1-bit input: Port A single-bit error injection
        .INJECT_SBITERR_B(1'b0),             // 1-bit input: Port B single-bit error injection
        .OREG_CE_A(1'b1),                    // 1-bit input: Port A output register clock enable
        .OREG_CE_B(1'b1),                    // 1-bit input: Port B output register clock enable
        .OREG_ECC_CE_A(1'b0),                // 1-bit input: Port A ECC decoder output register clock enable
        .OREG_ECC_CE_B(1'b0),                // 1-bit input: Port B ECC decoder output register clock enable
        .RDB_WR_A(1'b0),                     // 1-bit input: Port A read/write select
        .RDB_WR_B(1'b1),                     // 1-bit input: Port B read/write select
        .RST_A(1'b0),                        // 1-bit input: Port A asynchronous or synchronous reset for output
                                             // registers
        .RST_B(1'b0),                        // 1-bit input: Port B asynchronous or synchronous reset for output
                                             // registers
        .SLEEP(1'b0)                         // 1-bit input: Dynamic power gating control
    );
`endif 
endmodule
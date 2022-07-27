module URAMReal 
#(
    parameter DATA_WIDTH = 64, 
    parameter ADDRESS_WIDTH = 12, 
)
(
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
    // https://docs.xilinx.com/r/2021.1-English/ug974-vivado-ultrascale-libraries/URAM288
    URAM288 #(
        .AUTO_SLEEP_LATENCY(8),            // Latency requirement to enter sleep mode
        .AVG_CONS_INACTIVE_CYCLES(10),     // Average consecutive inactive cycles when is SLEEP mode for power
                                            // estimation
        .BWE_MODE_A("PARITY_INTERLEAVED"), // Port A Byte write control
        .BWE_MODE_B("PARITY_INTERLEAVED"), // Port B Byte write control
        .CASCADE_ORDER_A("NONE"),          // Port A position in cascade chain
        .CASCADE_ORDER_B("NONE"),          // Port B position in cascade chain
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
        .OREG_A("FALSE"),                  // Optional Port A output pipeline registers
        .OREG_B("FALSE"),                  // Optional Port B output pipeline registers
        .OREG_ECC_A("FALSE"),              // Port A ECC decoder output
        .OREG_ECC_B("FALSE"),              // Port B output ECC decoder
        .REG_CAS_A("FALSE"),               // Optional Port A cascade register
        .REG_CAS_B("FALSE"),               // Optional Port B cascade register
        .RST_MODE_A("SYNC"),               // Port A reset mode
        .RST_MODE_B("SYNC"),               // Port B reset mode
        .SELF_ADDR_A(11'h000),             // Port A self-address value
        .SELF_ADDR_B(11'h000),             // Port B self-address value
        .SELF_MASK_A(11'h7ff),             // Port A self-address mask
        .SELF_MASK_B(11'h7ff),             // Port B self-address mask
        .USE_EXT_CE_A("FALSE"),            // Enable Port A external CE inputs for output registers
        .USE_EXT_CE_B("FALSE")             // Enable Port B external CE inputs for output registers
    ) URAM288_inst (
        .CAS_OUT_ADDR_A(U),         // 23-bit output: Port A cascade output address
        .CAS_OUT_ADDR_B(CAS_OUT_ADDR_B),         // 23-bit output: Port B cascade output address
        .CAS_OUT_BWE_A(CAS_OUT_BWE_A),           // 9-bit output: Port A cascade Byte-write enable output
        .CAS_OUT_BWE_B(CAS_OUT_BWE_B),           // 9-bit output: Port B cascade Byte-write enable output
        .CAS_OUT_DBITERR_A(CAS_OUT_DBITERR_A),   // 1-bit output: Port A cascade double-bit error flag output
        .CAS_OUT_DBITERR_B(CAS_OUT_DBITERR_B),   // 1-bit output: Port B cascade double-bit error flag output
        .CAS_OUT_DIN_A(CAS_OUT_DIN_A),           // 72-bit output: Port A cascade output write mode data
        .CAS_OUT_DIN_B(CAS_OUT_DIN_B),           // 72-bit output: Port B cascade output write mode data
        .CAS_OUT_DOUT_A(CAS_OUT_DOUT_A),         // 72-bit output: Port A cascade output read mode data
        .CAS_OUT_DOUT_B(CAS_OUT_DOUT_B),         // 72-bit output: Port B cascade output read mode data
        .CAS_OUT_EN_A(CAS_OUT_EN_A),             // 1-bit output: Port A cascade output enable
        .CAS_OUT_EN_B(CAS_OUT_EN_B),             // 1-bit output: Port B cascade output enable
        .CAS_OUT_RDACCESS_A(CAS_OUT_RDACCESS_A), // 1-bit output: Port A cascade read status output
        .CAS_OUT_RDACCESS_B(CAS_OUT_RDACCESS_B), // 1-bit output: Port B cascade read status output
        .CAS_OUT_RDB_WR_A(CAS_OUT_RDB_WR_A),     // 1-bit output: Port A cascade read/write select output
        .CAS_OUT_RDB_WR_B(CAS_OUT_RDB_WR_B),     // 1-bit output: Port B cascade read/write select output
        .CAS_OUT_SBITERR_A(CAS_OUT_SBITERR_A),   // 1-bit output: Port A cascade single-bit error flag output
        .CAS_OUT_SBITERR_B(CAS_OUT_SBITERR_B),   // 1-bit output: Port B cascade single-bit error flag output
        .DBITERR_A(DBITERR_A),                   // 1-bit output: Port A double-bit error flag status
        .DBITERR_B(DBITERR_B),                   // 1-bit output: Port B double-bit error flag status
        .DOUT_A(DOUT_A),                         // 72-bit output: Port A read data output
        .DOUT_B(DOUT_B),                         // 72-bit output: Port B read data output
        .RDACCESS_A(RDACCESS_A),                 // 1-bit output: Port A read status
        .RDACCESS_B(RDACCESS_B),                 // 1-bit output: Port B read status
        .SBITERR_A(SBITERR_A),                   // 1-bit output: Port A single-bit error flag status
        .SBITERR_B(SBITERR_B),                   // 1-bit output: Port B single-bit error flag status
        .ADDR_A(waddr),                          // 23-bit input: Port A address
        .ADDR_B(raddr),                          // 23-bit input: Port B address
        .BWE_A(wen),                             // 9-bit input: Port A Byte-write enable
        .BWE_B(BWE_B),                           // 9-bit input: Port B Byte-write enable
        .CAS_IN_ADDR_A(CAS_IN_ADDR_A),           // 23-bit input: Port A cascade input address
        .CAS_IN_ADDR_B(CAS_IN_ADDR_B),           // 23-bit input: Port B cascade input address
        .CAS_IN_BWE_A(CAS_IN_BWE_A),             // 9-bit input: Port A cascade Byte-write enable input
        .CAS_IN_BWE_B(CAS_IN_BWE_B),             // 9-bit input: Port B cascade Byte-write enable input
        .CAS_IN_DBITERR_A(CAS_IN_DBITERR_A),     // 1-bit input: Port A cascade double-bit error flag input
        .CAS_IN_DBITERR_B(CAS_IN_DBITERR_B),     // 1-bit input: Port B cascade double-bit error flag input
        .CAS_IN_DIN_A(CAS_IN_DIN_A),             // 72-bit input: Port A cascade input write mode data
        .CAS_IN_DIN_B(CAS_IN_DIN_B),             // 72-bit input: Port B cascade input write mode data
        .CAS_IN_DOUT_A(CAS_IN_DOUT_A),           // 72-bit input: Port A cascade input read mode data
        .CAS_IN_DOUT_B(CAS_IN_DOUT_B),           // 72-bit input: Port B cascade input read mode data
        .CAS_IN_EN_A(CAS_IN_EN_A),               // 1-bit input: Port A cascade enable input
        .CAS_IN_EN_B(CAS_IN_EN_B),               // 1-bit input: Port B cascade enable input
        .CAS_IN_RDACCESS_A(CAS_IN_RDACCESS_A),   // 1-bit input: Port A cascade read status input
        .CAS_IN_RDACCESS_B(CAS_IN_RDACCESS_B),   // 1-bit input: Port B cascade read status input
        .CAS_IN_RDB_WR_A(CAS_IN_RDB_WR_A),       // 1-bit input: Port A cascade read/write select input
        .CAS_IN_RDB_WR_B(CAS_IN_RDB_WR_B),       // 1-bit input: Port B cascade read/write select input
        .CAS_IN_SBITERR_A(CAS_IN_SBITERR_A),     // 1-bit input: Port A cascade single-bit error flag input
        .CAS_IN_SBITERR_B(CAS_IN_SBITERR_B),     // 1-bit input: Port B cascade single-bit error flag input
        .CLK(clock),                             // 1-bit input: Clock source
        .DIN_A(DIN_A),                           // 72-bit input: Port A write data input
        .DIN_B(DIN_B),                           // 72-bit input: Port B write data input
        .EN_A(EN_A),                             // 1-bit input: Port A enable
        .EN_B(EN_B),                             // 1-bit input: Port B enable
        .INJECT_DBITERR_A(INJECT_DBITERR_A),     // 1-bit input: Port A double-bit error injection
        .INJECT_DBITERR_B(INJECT_DBITERR_B),     // 1-bit input: Port B double-bit error injection
        .INJECT_SBITERR_A(INJECT_SBITERR_A),     // 1-bit input: Port A single-bit error injection
        .INJECT_SBITERR_B(INJECT_SBITERR_B),     // 1-bit input: Port B single-bit error injection
        .OREG_CE_A(OREG_CE_A),                   // 1-bit input: Port A output register clock enable
        .OREG_CE_B(OREG_CE_B),                   // 1-bit input: Port B output register clock enable
        .OREG_ECC_CE_A(OREG_ECC_CE_A),           // 1-bit input: Port A ECC decoder output register clock enable
        .OREG_ECC_CE_B(OREG_ECC_CE_B),           // 1-bit input: Port B ECC decoder output register clock enable
        .RDB_WR_A(RDB_WR_A),                     // 1-bit input: Port A read/write select
        .RDB_WR_B(RDB_WR_B),                     // 1-bit input: Port B read/write select
        .RST_A(RST_A),                           // 1-bit input: Port A asynchronous or synchronous reset for
                                                    // output registers

        .RST_B(RST_B),                           // 1-bit input: Port B asynchronous or synchronous reset for
                                                    // output registers

        .SLEEP(SLEEP)                            // 1-bit input: Dynamic power gating control
        );
`endif 
endmodule
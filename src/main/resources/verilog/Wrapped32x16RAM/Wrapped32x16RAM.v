module Wrapped32x16RAM 
#(
    parameter INIT_A = 64'h0,
    parameter INIT_B = 64'h0,
    parameter INIT_C = 64'h0,
    parameter INIT_D = 64'h0,
    parameter INIT_E = 64'h0,
    parameter INIT_F = 64'h0,
    parameter INIT_G = 64'h0,
    parameter INIT_H = 64'h0 // defaults to out = 0
) (
    input wire clock, 
    input wire we, 
    input wire [4:0] addr,
    input wire [15:0] din,
    output wire [15:0] dout
);
`ifdef VERILATOR 
    reg [15:0] mem [31:0];
    // initialization vector used in src/test/scala/manticore/machine/pipeline/CustomFunctionTester.scala
    initial begin 
        mem[0] = 16'hcaca;
        mem[1] = 16'hcaca;
        mem[2] = 16'hcaca;
        mem[3] = 16'hcaca;
        mem[4] = 16'hcaca;
        mem[5] = 16'hcaca;
        mem[6] = 16'hcaca;
        mem[7] = 16'hcaca;
        mem[8] = 16'hcaca;
        mem[9] = 16'hcaca;
        mem[10] = 16'hcaca;
        mem[11] = 16'hcaca;
        mem[12] = 16'hcaca;
        mem[13] = 16'hcaca;
        mem[14] = 16'hcaca;
        mem[15] = 16'hcaca;

        mem[16] = 16'hacac;
        mem[17] = 16'hacac;
        mem[18] = 16'hacac;
        mem[19] = 16'hacac;
        mem[20] = 16'hacac;
        mem[21] = 16'hacac;
        mem[22] = 16'hacac;
        mem[23] = 16'hacac;
        mem[24] = 16'hacac;
        mem[25] = 16'hacac;
        mem[26] = 16'hacac;
        mem[27] = 16'hacac;
        mem[28] = 16'hacac;
        mem[29] = 16'hacac;
        mem[30] = 16'hacac;
        mem[31] = 16'hacac;

        // mem[16] = 16'hcaca;
        // mem[17] = 16'hcaca;
        // mem[18] = 16'hcaca;
        // mem[19] = 16'hcaca;
        // mem[20] = 16'hcaca;
        // mem[21] = 16'hcaca;
        // mem[22] = 16'hcaca;
        // mem[23] = 16'hcaca;
        // mem[24] = 16'hcaca;
        // mem[25] = 16'hcaca;
        // mem[26] = 16'hcaca;
        // mem[27] = 16'hcaca;
        // mem[28] = 16'hcaca;
        // mem[29] = 16'hcaca;
        // mem[30] = 16'hcaca;
        // mem[31] = 16'hcaca;
    end

    always @(posedge  clock) begin 
        if (we) begin 
            mem[addr] = din;
        end
    end

    assign dout = mem[addr];

`else

    // RAM32M16: 32-deep by 16-wide Multi Port LUT RAM (Mapped to eight LUT6s)
    //           UltraScale
    // Xilinx HDL Language Template, version 2021.1

    RAM32M16 #(
        .INIT_A(INIT_A),        // Initial contents of A Port
        .INIT_B(INIT_B),        // Initial contents of B Port
        .INIT_C(INIT_C),        // Initial contents of C Port
        .INIT_D(INIT_D),        // Initial contents of D Port
        .INIT_E(INIT_E),        // Initial contents of E Port
        .INIT_F(INIT_F),        // Initial contents of F Port
        .INIT_G(INIT_G),        // Initial contents of G Port
        .INIT_H(INIT_H),        // Initial contents of H Port
        .IS_WCLK_INVERTED(1'b0) // Specifies active high/low WCLK
    ) RAM32M16_inst (
        .DOA(dout[15:14]), // Read port A 2-bit output
        .DOB(dout[13:12]), // Read port B 2-bit output
        .DOC(dout[11:10]), // Read port C 2-bit output
        .DOD(dout[9:8]),   // Read port D 2-bit output
        .DOE(dout[7:6]),   // Read port E 2-bit output
        .DOF(dout[5:4]),   // Read port F 2-bit output
        .DOG(dout[3:2]),   // Read port G 2-bit output
        .DOH(dout[1:0]),   // Read/write port H 2-bit output
        .ADDRA(addr),      // Read port A 5-bit address input
        .ADDRB(addr),      // Read port B 5-bit address input
        .ADDRC(addr),      // Read port C 5-bit address input
        .ADDRD(addr),      // Read port D 5-bit address input
        .ADDRE(addr),      // Read port E 5-bit address input
        .ADDRF(addr),      // Read port F 5-bit address input
        .ADDRG(addr),      // Read port G 5-bit address input
        .ADDRH(addr),      // Read/write port H 5-bit address input
        .DIA(din[15:14]),  // RAM 2-bit data write input addressed by ADDRH,
                           //   read addressed by ADDRA
        .DIB(din[13:12]),  // RAM 2-bit data write input addressed by ADDRH,
                           //   read addressed by ADDRB
        .DIC(din[11:10]),  // RAM 2-bit data write input addressed by ADDRH,
                           //   read addressed by ADDRC
        .DID(din[9:8]),    // RAM 2-bit data write input addressed by ADDRH,
                           //   read addressed by ADDRD
        .DIE(din[7:6]),    // RAM 2-bit data write input addressed by ADDRH,
                           //   read addressed by ADDRE
        .DIF(din[5:4]),    // RAM 2-bit data write input addressed by ADDRH,
                           //   read addressed by ADDRF
        .DIG(din[3:2]),    // RAM 2-bit data write input addressed by ADDRH,
                           //   read addressed by ADDRG
        .DIH(din[1:0]),    // RAM 2-bit data write input addressed by ADDRH,
                           //   read addressed by ADDRH
        .WCLK(clock),      // Write clock input
        .WE(we)            // Write enable input
    );

    // End of RAM32M16_inst instantiation
    
`endif 

endmodule
/**
* Wrapper for Xilinx BUFCE (glitchless clock gating cell)
*/
module ClockBuffer(
    input wire CE, // clock enable
    input wire I, // input (source) clock
    output wire O // output clock
);

`ifndef SYNTHESIS
    // simulation purpose only
    wire input_clk;
    reg output_clk;
    assign input_clk = I;
    always @(input_clk) begin
        if (input_clk == 1'b1 && CE) begin
            output_clk = 1'b1;
        end 
        else if (input_clk == 1'b0) begin
            output_clk = 1'b0;
        end
    end
    assign O = output_clk;
`else
    // the actual deal
    BUFGCE #(
        .CE_TYPE("SYNC"), // SYNC, ASYNC
        .IS_CE_INVERTED(1’b0), // Programmable inversion on CE
        .IS_I_INVERTED(1’b0) // Programmable inversion on I
    )
    impl (
        .O(O), // 1-bit output: Buffer
        .CE(CE), // 1-bit input: Buffer enable
        .I(I) // 1-bit input: Buffer
    );
`endif

endmodule
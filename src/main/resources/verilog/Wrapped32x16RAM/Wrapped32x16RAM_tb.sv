// `timescale 1 ns

module Main ();

    localparam TEST_SIZE = 1;

    // DUT signals
    // inputs, so we use a logic instead of wire (so we can assign a value in the testbench).
    logic              we   = 1'b0;
    logic [ 5 - 1 : 0] addr = 5'b0;
    logic [16 - 1 : 0] din  = 16'b0;
    // outputs, so we use a wire instead of logic.
    wire  [16 - 1 : 0] dout;

    // Testbench signals
    logic clock = 0;

    logic [16 - 1 : 0] expected = 16'b0;

    // Toggle clock.
    always #20 clock = ~clock;

    // DUT
    Wrapped32x16RAM #(
        .INIT_A(64'h0000_ffff_0000_ffff),
        .INIT_B(64'hffff_0000_ffff_0000),
        .INIT_C(64'h00ff_00ff_00ff_00ff),
        .INIT_D(64'hff00_ff00_ff00_ff00),
        .INIT_E(64'h5555_5555_5555_5555),
        .INIT_F(64'haaaa_aaaa_aaaa_aaaa),
        .INIT_G(64'h1ec2_2a79_4142_37db),
        .INIT_H(64'h4254_06d2_4703_39cb)
    ) dut (
        .clock(clock),
        .we(we),
        .addr(addr),
        .din(din),
        .dout(dout)
    );

    task testRoutine;
        addr <= 0;
        expected <= 16'b1100_1100_0110_1111;

        #80;

        addr <= 1;

        #80;

        addr <= 2;

        #80;

        addr <= 3;

        #80;

        addr <= 4;

        #80;

        addr <= 5;

        #80;

        addr <= 6;

        #80;

        addr <= 7;

        #80;

        we <= 1;
        din <= 16'b1011_0111_0000_1101;
        expected <= 16'b1011_0111_0000_1101;

        #80;

        if (dout != expected) begin
            $display("[WRONG] Expected %h but got %h", expected, dout);
        end

        $finish;
    endtask

    initial begin
        @(posedge clock);
        @(posedge clock);
        @(posedge clock);
        @(posedge clock);
        forever begin 
            @(posedge clock) testRoutine;
        end
    end

endmodule
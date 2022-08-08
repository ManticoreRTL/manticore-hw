// `timescale 1 ns

module Main ();

    localparam W = 16;
    localparam TEST_SIZE = 500;

    // DUT signals
    // inputs, so we use a logic instead of wire (so we can assign a value in the testbench).
    logic [W - 1 : 0] in0 = 0;
    logic [W - 1 : 0] in1 = 0;
    logic [W - 1 : 0] in2 = 0; // only used for MUL
    logic             carryin = 0;
    logic [9 - 1 : 0] opmode = 0;
    logic [4 - 1 : 0] alumode = 0;
    logic [2 - 1 : 0] setinst = 0;
    logic             valid_in = 0;
    // outputs, so we use a wire instead of logic.
    wire   [W - 1 : 0] out;
    wire [2*W - 1 : 0] mul_out;
    wire               carryout;
    wire               valid_out;

    // signed wires used to verify the SLTS instruction
    wire signed [W - 1 : 0] in0s, in1s;
    assign in0s = in0; 
    assign in1s = in1;

    //                 | OPMODE[8:0] | ALUMODE[3:0] | Notes
    //   --------------|-------------|--------------|------------------------------
    //                 |  876543210  |     3210     |
    //                 |  vvvvvvvvv  |     vvvv     |
    //   and(b,c)      |  000110011  |     1100     | ug579 pg 29, 38 // W = 0, X = A:B, Y = 0, Z = C // P = X AND Z
    //   or(b,c)       |  000111011  |     1100     | ug579 pg 29, 38 // W = 0, X = A:B, Y = 1, Z = C // P = X OR Z
    //   xor(b,c)      |  000110011  |     0100     | ug579 pg 29, 38 // W = 0, X = A:B, Y = 0, Z = C // P = X XOR Z
    //   add(b,c)      |  000110011  |     0000     | ug579 pg 30, 32 // W = 0, X = A:B, Y = 0, Z = C // P = Z + W + X + Y + CIN
    //   addc(b,c,cin) |  000110011  |     0000     | ug579 pg 30, 32 // W = 0, X = A:B, Y = 0, Z = C // P = Z + W + X + Y + CIN
    //   sub(b,c)      |  000110011  |     0011     | ug579 pg 30, 32 // W = 0, X = A:B, Y = 0, Z = C // P = Z - (W + X + Y + CIN)
    //   mul(a,b)      |  000000101  |     0000     | ug579 pg 29     // W = 0, X = M  , Y = M, Z = 0 // P = X * Y                 
    //                 |             |              | (ALUMODE does not matter and we set it to ADD)
    //   seq(b,c)      |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.
    //   sltu(b,c)     |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.
    //   slts(b,c)     |  000110011  |     0011     | // Use subtraction. External circuit detects comparison result.

    // The DSP has a 2-cycle latency. We therefore need to store the intermediate values
    // to check the result after a delay when the inputs are fed.
    logic   [W - 1 : 0] expected [0:2];
    logic [2*W - 1 : 0] mul_expected [0:3];

    // Testbench signals
    logic              clock = 0;
    logic [10 - 1 : 0] cnt = 0;

    // Toggle clock.
    always #20 clock = ~clock;

    // DUT
    AluDsp48 dut (
        .clock(clock),
        .in0(in0),
        .in1(in1),
        .in2(in2),
        .carryin(carryin),
        .opmode(opmode),
        .alumode(alumode),
        .setinst(setinst),
        .out(out),
        .mul_out(mul_out),
        .carryout(carryout),
        .valid_in(valid_in),
        .valid_out(valid_out)
    );

    task testAnd;
        opmode = 9'b000110011;
        alumode = 4'b1100;
        setinst = 2'b00;

        expected[1] <= expected[0];
        expected[2] <= expected[1];

        // Calling urandom_range(0, 9) instead of urandom_range(0, 1) simply to have
        // longer delays between operations in the simulation.
        if ($urandom_range(0, 9) == 3) begin
            valid_in <= 1;
            // $urandom_range returns an INCLUSIVE range.
            in0 = $urandom_range(1, 65535);
            in1 = $urandom_range(1, 65535);
            carryin = 0;
        end else begin
            valid_in <= 0;
            in0 = 0;
            in1 = 0;
            carryin = 0;
        end

        expected[0] <= (in0 & in1);

        if (valid_out == 1) begin
            cnt <= cnt + 1;
            if (out != expected[2]) begin
                $display("[AND] [%d] Expected %d but got %d", cnt, expected[2], out);
                // $finish;
            end
        end

        if (cnt == TEST_SIZE - 1) begin
            $finish;
        end
    endtask

    task testOr;
        opmode = 9'b000111011;
        alumode = 4'b1100;
        setinst = 2'b00;

        expected[1] <= expected[0];
        expected[2] <= expected[1];

        // Calling urandom_range(0, 9) instead of urandom_range(0, 1) simply to have
        // longer delays between operations in the simulation.
        if ($urandom_range(0, 9) == 3) begin
            valid_in <= 1;
            // $urandom_range returns an INCLUSIVE range.
            in0 = $urandom_range(1, 65535);
            in1 = $urandom_range(1, 65535);
            carryin = 0;
        end else begin
            valid_in <= 0;
            in0 = 0;
            in1 = 0;
            carryin = 0;
        end

        expected[0] <= (in0 | in1);

        if (valid_out == 1) begin
            cnt <= cnt + 1;
            if (out != expected[2]) begin
                $display("[OR]  [%d] Expected %d but got %d", cnt, expected[2], out);
                // $finish;
            end
        end

        if (cnt == TEST_SIZE - 1) begin
            $finish;
        end
    endtask

    task testXor;
        opmode = 9'b000110011;
        alumode = 4'b0100;
        setinst = 2'b00;

        expected[1] <= expected[0];
        expected[2] <= expected[1];

        // Calling urandom_range(0, 9) instead of urandom_range(0, 1) simply to have
        // longer delays between operations in the simulation.
        if ($urandom_range(0, 9) == 3) begin
            valid_in <= 1;
            // $urandom_range returns an INCLUSIVE range.
            in0 = $urandom_range(1, 65535);
            in1 = $urandom_range(1, 65535);
            carryin = 0;
        end else begin
            valid_in <= 0;
            in0 = 0;
            in1 = 0;
            carryin = 0;
        end

        expected[0] <= (in0 ^ in1);

        if (valid_out == 1) begin
            cnt <= cnt + 1;
            if (out != expected[2]) begin
                $display("[XOR] [%d] Expected %d but got %d", cnt, expected[2], out);
                // $finish;
            end
        end

        if (cnt == TEST_SIZE - 1) begin
            $finish;
        end
    endtask

    task testAdd;
        opmode = 9'b000110011;
        alumode = 4'b0000;
        setinst = 2'b00;

        expected[1] <= expected[0];
        expected[2] <= expected[1];

        // Calling urandom_range(0, 9) instead of urandom_range(0, 1) simply to have
        // longer delays between operations in the simulation.
        if ($urandom_range(0, 9) == 3) begin
            valid_in <= 1;
            // $urandom_range returns an INCLUSIVE range.
            in0 = $urandom_range(1, 65535);
            in1 = $urandom_range(1, 65535);
            carryin = $urandom_range(0, 1);
        end else begin
            valid_in <= 0;
            in0 = 0;
            in1 = 0;
            carryin = 0;
        end

        expected[0] <= (in0 + in1 + carryin);

        if (valid_out == 1) begin
            cnt <= cnt + 1;
            if (out != expected[2]) begin
                $display("[ADD] [%d] Expected %d but got %d", cnt, expected[2], out);
                // $finish;
            end
        end

        if (cnt == TEST_SIZE - 1) begin
            $finish;
        end
    endtask

    task testSub;
        opmode = 9'b000110011;
        alumode = 4'b0011;
        setinst = 2'b00;

        expected[1] <= expected[0];
        expected[2] <= expected[1];

        // Calling urandom_range(0, 9) instead of urandom_range(0, 1) simply to have
        // longer delays between operations in the simulation.
        if ($urandom_range(0, 9) == 3) begin
            valid_in <= 1;
            // $urandom_range returns an INCLUSIVE range.
            in0 = $urandom_range(1, 65535);
            in1 = $urandom_range(1, 65535);
            carryin = 0;
        end else begin
            valid_in <= 0;
            in0 = 0;
            in1 = 0;
            carryin = 0;
        end

        expected[0] <= (in0 - in1);

        if (valid_out == 1) begin
            cnt <= cnt + 1;
            if (out != expected[2]) begin
                $display("[SUB] [%d] Expected %d but got %d", cnt, expected[2], out);
                // $finish;
            end
        end

        if (cnt == TEST_SIZE - 1) begin
            $finish;
        end
    endtask

    task testMul;
        opmode = 9'b000000101;
        alumode = 4'b0000;
        setinst = 2'b00;

        // Multiplication has 3 cycle latency unlike other operations
        mul_expected[1] <= mul_expected[0];
        mul_expected[2] <= mul_expected[1];
        mul_expected[3] <= mul_expected[2];

        // Calling urandom_range(0, 9) instead of urandom_range(0, 1) simply to have
        // longer delays between operations in the simulation.
        if ($urandom_range(0, 9) == 3) begin
            valid_in <= 1;
            // $urandom_range returns an INCLUSIVE range.
            in1 = $urandom_range(1, 65535);
            in2 = $urandom_range(1, 65535);
            carryin = 0;
        end else begin
            valid_in <= 0;
            in1 = 0;
            in2 = 0;
            carryin = 0;
        end

        mul_expected[0] <= (in1 * in2);

        if (valid_out == 1) begin
            cnt <= cnt + 1;
            if (mul_out != mul_expected[3]) begin
                $display("[MUL] [%d] Expected %d but got %d", cnt, mul_expected[2], out);
                // $finish;
            end
        end

        if (cnt == TEST_SIZE - 1) begin
            $finish;
        end
    endtask

    task testSeq;
        opmode = 9'b000110011;
        alumode = 4'b0011;
        setinst = 2'b01;

        expected[1] <= expected[0];
        expected[2] <= expected[1];

        // Calling urandom_range(0, 9) instead of urandom_range(0, 1) simply to have
        // longer delays between operations in the simulation.
        if ($urandom_range(0, 9) == 3) begin
            valid_in <= 1;
            // $urandom_range returns an INCLUSIVE range.
            // set smaller upper bound than other tests so that we have enough number
            // of positive (equal) samples
            in0 = $urandom_range(1, 5);
            in1 = $urandom_range(1, 5);
            carryin = 0;
        end else begin
            valid_in <= 0;
            in0 = 0;
            in1 = 0;
            carryin = 0;
        end

        expected[0] <= (in0 == in1);

        if (valid_out == 1) begin
            cnt <= cnt + 1;
            if (out != expected[2]) begin
                $display("[SEQ] [%d] Expected %d but got %d", cnt, expected[2], out);
                // $finish;
            end
        end

        if (cnt == TEST_SIZE - 1) begin
            $finish;
        end
    endtask

    task testSltu;
        opmode = 9'b000110011;
        alumode = 4'b0011;
        setinst = 2'b10;

        expected[1] <= expected[0];
        expected[2] <= expected[1];

        // Calling urandom_range(0, 9) instead of urandom_range(0, 1) simply to have
        // longer delays between operations in the simulation.
        if ($urandom_range(0, 9) == 3) begin
            valid_in <= 1;
            // $urandom_range returns an INCLUSIVE range.
            in0 = $urandom_range(1, 65535);
            in1 = $urandom_range(1, 65535);
            carryin = 0;
        end else begin
            valid_in <= 0;
            in0 = 0;
            in1 = 0;
            carryin = 0;
        end

        expected[0] <= (in0 < in1);

        if (valid_out == 1) begin
            cnt <= cnt + 1;
            if (out != expected[2]) begin
                $display("[SLU] [%d] Expected %d but got %d", cnt, expected[2], out);
                // $finish;
            end
        end

        if (cnt == TEST_SIZE - 1) begin
            $finish;
        end
    endtask

    task testSlts;
        opmode = 9'b000110011;
        alumode = 4'b0011;
        setinst = 2'b11;

        // As it takes one cycle to assign value to in0s and in1s, we skip expected[0] 
        // and put the expected value directly to expected[1]
        expected[0] <= 16'b0;
        expected[1] <= (in0s < in1s);
        expected[2] <= expected[1];

        // Calling urandom_range(0, 9) instead of urandom_range(0, 1) simply to have
        // longer delays between operations in the simulation.
        if ($urandom_range(0, 9) == 3) begin
            valid_in <= 1;
            // $urandom_range returns an INCLUSIVE range.
            in0 = $urandom_range(1, 65535);
            in1 = $urandom_range(1, 65535);
            // in0s = in0;
            // in1s = in1;
            carryin = 0;
        end else begin
            valid_in <= 0;
            in0 = 0;
            in1 = 0;
            carryin = 0;
        end

        if (valid_out == 1) begin
            cnt <= cnt + 1;
            if (out != expected[2]) begin
                $display("[SLS] [%d] Expected %d but got %d", cnt, expected[2], out);
                // $finish;
            end
        end

        if (cnt == TEST_SIZE - 1) begin
            $finish;
        end
    endtask

    integer i = 0;
    initial begin
        // The DSP has a startup latency. We add a few clocks so we can get rid of it.
        @(posedge clock);
        @(posedge clock);
        @(posedge clock);
        @(posedge clock);
        forever begin 
            // uncomment only one of below
            // @(posedge clock) testAnd; 
            // @(posedge clock) testOr; 
            // @(posedge clock) testXor; 
            // @(posedge clock) testAdd; 
            // @(posedge clock) testSub;
            @(posedge clock) testMul;
            // @(posedge clock) testSeq; 
            // @(posedge clock) testSltu; 
            // @(posedge clock) testSlts;  
        end
    end

endmodule
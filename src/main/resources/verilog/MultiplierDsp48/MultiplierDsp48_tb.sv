// `timescale 1 ns

module Main ();

  localparam W = 16;
  localparam TEST_SIZE = 50;

  // DUT signals
  // inputs, so we use a logic instead of wire (so we can assign a value in the testbench).
  logic [W - 1 : 0] in0 = 0;
  logic [W - 1 : 0] in1 = 0;
  logic             valid_in = 0;
  // outputs, so we use a wire instead of logic.
  wire  [2*W-1 : 0] out;
  wire              valid_out;

  // The DSP has a 2-cycle latency. We therefore need to store the intermediate values
  // to check the result after a delay when the inputs are fed.
  logic [2*W-1 : 0] expected [0:2];

  // Testbench signals
  logic              clock = 0;
  logic [10 - 1 : 0] cnt = 0;

  // Toggle clock.
  always #20 clock = ~clock;

  // DUT
  MultiplierDsp48 dut (
    .clock(clock),
    .in0(in0),
    .in1(in1),
    .out(out),
    .valid_in(valid_in),
    .valid_out(valid_out)
  );

  task testRoutine;
    expected[1] <= expected[0];
    expected[2] <= expected[1];

    // Calling urandom_range(0, 9) instead of urandom_range(0, 1) simply to have
    // longer delays between operations in the simulation.
    if ($urandom_range(0,9) == 3) begin
      valid_in <= 1;
      // $urandom_range returns an INCLUSIVE range.
      in0 = $urandom_range(1, 65535);
      in1 = $urandom_range(1, 65535);
    end else begin
      valid_in <= 0;
      in0 = 0;
      in1 = 0;
    end

    expected[0] <= (in0 * in1);

    if (valid_out == 1) begin
      cnt <= cnt + 1;
      if (out != expected[2]) begin
        $display("[%d] Expected %d but got %d", cnt, expected[2], out);
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
    forever begin @(posedge clock) testRoutine; end
  end

endmodule
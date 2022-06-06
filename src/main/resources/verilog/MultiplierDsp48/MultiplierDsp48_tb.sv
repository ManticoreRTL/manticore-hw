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
  wire  [W - 1 : 0] out;
  wire              valid_out;

  // The DSP has a 2-cycle latency. We therefore need to store the intermediate values
  // to check the result after a delay when the inputs are fed.
  logic [W - 1 : 0] outcome [0:1];

  // Testbench signals
  logic              clock = 0;
  logic [10 - 1 : 0] cnt = 0;

  // Toggle clock.
  always #20 clock = ~clock;

  // DUT
  MultiplierDsp48 dut (
    .clk(clock),
    .in0(in0),
    .in1(in1),
    .out(out),
    .valid_in(valid_in),
    .valid_out(valid_out)
  );

  task testRoutine;
    outcome[1] <= outcome[0];

    if (($urandom() & 1) == 0) begin
      valid_in <= 1;
      in0 = ($urandom() & 16'hffff);
      in1 = ($urandom() & 16'hffff);
      outcome[0] <= (in0 * in1);
    end else begin
      valid_in <= 0;
    end

    if (valid_out == 1) begin
      cnt <= cnt + 1;
      valid_in <= 1;
      if (out != outcome[1]) begin
        $display("[%d] Expected %h but got %h", cnt, outcome[1], out);
        $finish;
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



module Testbench();

    `ifdef TEST_SIZE
    localparam TEST_SIZE = `TEST_SIZE;
    `else
    localparam TEST_SIZE = 4096;
    `endif

    localparam PERIOD = 20;
    localparam DELTA = 4;
    logic failed = 0;

    task FailIt; failed = 1; $stop; endtask

    logic clock = 0 ;
    logic reset = 0;

    typedef logic[13:0] address_t;
    typedef logic[15:0] data_t;

    function address_t rnd_address();
        return $urandom_range(0, (1 << 14) - 1);
    endfunction

    function address_t rnd_data();
        return $urandom_range(0, (1 << 13) - 1);
    endfunction

    address_t raddr;
    address_t waddr;

    data_t din;
    logic        wen;

    data_t dout;

    SimpleDualPortMemory dut(
        .clock(clock),
        .reset(reset),
        .io_raddr(raddr),
        .io_dout(dout),
        .io_wen(wen),
        .io_waddr(waddr),
        .io_din(din)
    );

    logic [31: 0] clock_counter;
    always #(PERIOD / 2) clock = ~clock;
    always @(posedge clock) clock_counter <= clock_counter + 1;

    `ifdef DUMP_VCD
    initial begin
        $display("VCD dump is enabled");
        $dumpfile("waves.vcd");
        $dumpvars(0, Testbench);
    end
    `endif

    task Tick;
        @(posedge clock) #DELTA;
    endtask


    address_t s_raddr;
    address_t s_waddr;
    data_t s_din;
    data_t s_dout;
    logic [15:0] memory_model [0 : (1 << 14) - 1];
    initial begin

        for (int i = 0; i < 4; i ++) Tick;
        Tick;
        wen = 0;

        $display("Testing batch read-after-write operations");
        for (int i = 0; i < (1 << 14); i++) begin
            Tick;
            memory_model[i] = rnd_data();
            waddr = i;
            wen = 1;
            din = memory_model[i];
        end
        Tick;
        din = rnd_data();
        wen = 0;
        for (int i = 0; i < (1 << 14); i++) begin
            Tick;
            raddr = i;
            Tick;
            raddr = rnd_address();
            Tick;
            if (dout != memory_model[i]) begin
                $error("Expected %d at address %d but got %d", memory_model[i], i, dout);
                FailIt;
            end
            Tick;
            raddr = i;
            Tick;
            raddr = rnd_address();
            Tick;
            if (dout != memory_model[i]) begin
                $error("Expected %d at address %d but got %d, seems like reads are descrtuctive", memory_model[i], i, dout);
                FailIt;
            end
        end
        Tick;
        $display("Testing single read-after-write operations (pipelined)");
        for (int i = 0; i < TEST_SIZE; i++) begin
            Tick;
            s_waddr = rnd_address();
            s_din = rnd_data();
            wen = 1;
            waddr = s_waddr;
            raddr = rnd_address();
            din = s_din;
            Tick;
            wen = 0;
            din = rnd_data();
            raddr = s_waddr;
            Tick;
            raddr = rnd_address();
            Tick;
            if (s_din != dout) begin
                $error("Expected %d at address %d but got %d", s_din, s_waddr, dout);
                FailIt;
            end
        end
        $finish;
    end



endmodule
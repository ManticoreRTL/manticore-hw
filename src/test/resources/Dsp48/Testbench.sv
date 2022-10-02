

module Testbench ();

    `ifdef TEST_SIZE
    localparam TEST_SIZE = `TEST_SIZE;
    `else
    localparam TEST_SIZE = 2000;
    `endif
    localparam DELTA = 1;
    localparam PERIOD = 20;


    logic clock = 0;
    logic [31:0] clock_counter = 0;
    logic failed = 0;
    task failIt;
        failed = 1;
        $stop;
    endtask
    // timeunit 1ps;
    typedef enum logic[3 : 0] {
        ADD = 0, SUB, MUL, MULH, MULS, AND, OR, XOR, SLL, SRL, SRA, SEQ, SLTU, SLTS, MUX, ADDC
    } Funct_t;

    typedef struct {
        Funct_t fn;
        logic [15 : 0] op1;
        logic [15 : 0] op2;
        logic [0 : 0] ci;
        logic [0 : 0] sel;
        logic [15 : 0] msk;

        logic [15 : 0] r;
        logic [31 : 0] m;
        logic [0 : 0] co;

    } Stimulus;


    function Stimulus mkStimulus();

        automatic Stimulus t;
        automatic Funct_t opcodes [$] = {ADD, SUB, MUL, MULH, AND, OR, XOR, SLL, SRL, SRA, SEQ, SLTU, SLTS, MUX, ADDC};
        // automatic Funct_t opcodes [$] = {ADD, SLL};
        automatic int ix = $urandom_range(0, opcodes.size() - 1);

        t.fn = opcodes[ix];
        // t.fn = SRA;

        t.op1 = $urandom_range(0, 16'hffff);
        t.op2 = $urandom_range(0, 16'hffff);
        t.msk = 16'hffff;
        t.ci = 0;
        t.sel = 0;

        case (t.fn)
            ADDC:
                t.ci = $urandom_range(0, 1);
            SEQ: begin
                // reduce range to have more likely equiality
                t.op1 = $urandom_range(0, 8);
                t.op2 = $urandom_range(0, 8);
                t.op1[15] = $urandom_range(0, 1);
                t.op2[15] = $urandom_range(0, 1);
            end
            SLTS: begin
                // force create negative numbers with 0.5 probability
                t.op1[15] = $urandom_range(0, 1);
                t.op2[15] = $urandom_range(0, 1);
            end
            SLTU: begin
                t.op1[15] = $urandom_range(0, 1);
                t.op2[15] = $urandom_range(0, 1);
            end
            MUX:
                t.sel = $urandom_range(0, 1);
        endcase

        return t;

    endfunction



    `define SIMPLE_OP_CHECK(op, operator) \
    task check``op``(input Stimulus t); \
        automatic logic [15 : 0] r = t.op1 ``operator`` t.op2; \
        if (t.r != r) begin \
            $error("@%d %s\t\t\t0x%x %s 0x%x = 0x%x but got 0x%x", clock_counter, t.fn.name(), t.op1, `"operator`", t.op2, r, t.r); \
            failIt; \
        end else begin \
            $display("@%d %s\t\t\t0x%x %s 0x%x = 0x%x OK!", clock_counter, t.fn.name(), t.op1, `"operator`", t.op2, r); \
        end \
    endtask

    `define LOGICAL_SHIFT_CHECK(op, operator) \
    task check``op``(input Stimulus t); \
        automatic logic [15 : 0] r = t.op1 ``operator`` t.op2[3:0]; \
        if (t.r != r) begin \
            $error("@%d %s\t\t\t0x%x %s 0x%x = 0x%x but got 0x%x", clock_counter, t.fn.name(), t.op1, `"operator`", t.op2, r, t.r); \
            failIt; \
        end else begin \
            $display("@%d %s\t\t\t0x%x %s 0x%x = 0x%x OK!", clock_counter, t.fn.name(), t.op1, `"operator`", t.op2, r); \
        end \
    endtask
    `SIMPLE_OP_CHECK(MUL, *)
    `SIMPLE_OP_CHECK(ADD, +)
    `SIMPLE_OP_CHECK(SUB, -)
    `SIMPLE_OP_CHECK(AND, &)
    `SIMPLE_OP_CHECK(OR, |)
    `SIMPLE_OP_CHECK(XOR, ^)
    `LOGICAL_SHIFT_CHECK(SRL, >>)
    `LOGICAL_SHIFT_CHECK(SLL, <<)

    task checkSRA(input Stimulus t);
        automatic logic signed [15 : 0] r = $signed(t.op1) >>> $signed(t.op2[3:0]);
        if (t.r != r) begin
            $error("@%d %s\t\t\t0x%x >>> 0x%x = 0x%x but got 0x%x", clock_counter, t.fn.name(), t.op1, t.op2, r, t.r);
            failIt;
        end else begin
            $display("@%d %s\t\t\t0x%x >>> 0x%x = 0x%x OK!", clock_counter, t.fn.name(), t.op1, t.op2, r);
        end
    endtask

    task checkMUX(input Stimulus t);
        automatic logic signed [15 : 0] r = t.sel ? t.op2 : t.op1;
        if (t.r != r) begin
            $error("@%d %s\t\t\t%b ? 0x%x : 0x%x = 0x%x but got 0x%x", clock_counter, t.fn.name(), t.sel, t.op2, t.op1, r, t.r);
            failIt;
        end else begin
            $display("@%d %s\t\t\t%b ? 0x%x : 0x%x = 0x%x OK!", clock_counter, t.fn.name(), t.sel, t.op2, t.op1, r);
        end
    endtask


    // task checkMUL(input Stimulus t);
    //     automatic logic [31 : 0] r = t.op1 * t.op2;
    //     if (t.m != r) begin
    //         $error("@%d %s\t\t\t0x%x * 0x%x = 0x%x but got 0x%x", clock_counter, t.fn.name(), t.op1, t.op2, r, t.m);
    //         failIt;
    //     end else begin
    //         $display("@%d %s\t\t\t0x%x * 0x%x = 0x%x OK!", clock_counter, t.fn.name(), t.op1, t.op2, r);
    //     end
    // endtask

    task checkMULH(input Stimulus t);
        automatic logic [31 : 0] r = t.op1 * t.op2;
        if (t.m != r) begin
            $error("@%d %s\t\t\t0x%x * 0x%x = 0x%x but got 0x%x", clock_counter, t.fn.name(), t.op1, t.op2, r, t.m);
            failIt;
        end else begin
            $display("@%d %s\t\t\t0x%x * 0x%x = 0x%x OK!", clock_counter, t.fn.name(), t.op1, t.op2, r);
        end
    endtask

    task checkADDC(input Stimulus t);
        automatic logic [16 : 0] sum = {1'b0, t.op1} + {1'b0, t.op2} + {16'b0, t.ci};
        if (t.r != sum[15:0] || t.co != sum[16]) begin
            $error("@%d ADDC\t\t\t0x%x + 0x%x + %b = {%b, 0x%x} but got {%b, 0x%x}", clock_counter, t.op1, t.op2, t.ci, sum[16], sum[15:0], t.co, t.r);
            failIt;
        end else begin
            $display("@%d ADDC\t\t\t0x%x + 0x%x + %b = {%b, 0x%x} OK!", clock_counter, t.op1, t.op2, t.ci, sum[16], sum[15:0]);
        end
    endtask

    task checkSEQ(input Stimulus t);

        automatic logic eq = t.op1 == t.op2;
        if (eq != t.r) begin
            $error("@%d SEQ\t\t\t0x%x == 0x%x = %b but got %b", clock_counter, t.op1, t.op2, eq, t.r);
            failIt;
        end else begin
            $display("@%d SEQ\t\t\t0x%x == 0x%x = %b OK!", clock_counter, t.op1, t.op2, eq);
        end

    endtask

    task checkSLTU(input Stimulus t);

        automatic logic eq = t.op1 < t.op2;
        if (eq != t.r) begin
            $error("@%d STLU\t\t\t0x%x < 0x%x = %b but got %b", clock_counter, t.op1, t.op2, eq, t.r);
            failIt;
        end else begin
            $display("@%d STLU\t\t\t0x%x < 0x%x = %b OK!", clock_counter, t.op1, t.op2, eq);
        end

    endtask

    task checkSLTS(input Stimulus t);

        automatic logic eq = $signed(t.op1) < $signed(t.op2);
        if (eq != t.r) begin
            $error("@%d SLTS\t\t\tsigned 0x%x < 0x%x = %b but got %b", clock_counter, t.op1, t.op2, eq, t.r);
            failIt;
        end else begin
            $display("@%d SLTS\t\t\tsigned 0x%x < 0x%x = %b OK!", clock_counter, t.op1, t.op2, eq);
        end

    endtask


    Stimulus test_queue [$] = {};



    always #(PERIOD / 2) clock = ~clock;
    always @(posedge clock) clock_counter <= clock_counter + 1;


    logic reset = 0;

    logic [15 : 0] in_x, in_y;
    logic [0 : 0]  select;
    Funct_t  funct;
    logic  [15 : 0] mask;
    logic [0 : 0]  carry_in;
    logic [0 : 0] vld_in;

    wire  [15 : 0] result;
    wire  [31 : 0] mul_result;
    wire  [0 : 0]  carry_out;
    logic  [0 : 0] vld_out, vld0, vld1;

    /** Used only to know when the first input is received */
    always @(posedge clock) begin
        vld0 <= vld_in;
        vld1 <= vld0;
        vld_out <= vld1;
    end

    Stimulus t_in;
    Stimulus t_out;

    StandardALUComb dut(
        .clock(clock),
        .reset(reset),
        .io_in_x(in_x),
        .io_in_y(in_y),
        .io_in_carry(carry_in),
        .io_in_select(select),
        .io_in_mask(mask),
        .io_out(result),
        .io_mul_out(mul_result),
        .io_funct(funct),
        .io_carry_out(carry_out)
    );

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
    int checked = 0;
    // `define CHECK(OP) check``OP``

    `define CHECK(op) op: begin check``op``(t_out); end
    always @(posedge clock) begin
        if (vld_out) begin
            if (test_queue.size() == 0) begin
                $error("@%d Did not expect any result!", clock_counter);
                $stop;
            end else begin
                t_out = test_queue.pop_front();
                t_out.r = result;
                t_out.m = mul_result;
                t_out.co = carry_out;
                case(t_out.fn)
                    `CHECK(ADD)
                    `CHECK(SUB)
                    `CHECK(MUL)
                    `CHECK(MULH)
                    `CHECK(XOR)
                    `CHECK(OR)
                    `CHECK(AND)
                    `CHECK(SEQ)
                    `CHECK(SLTS)
                    `CHECK(SLTU)
                    `CHECK(ADDC)
                    `CHECK(SRL)
                    `CHECK(SRA)
                    `CHECK(SLL)
                    `CHECK(MUX)
                    default: begin
                        // $error("Unhandled result %p!", t_out);
                        $stop;
                    end

                endcase
                // $display("Checked %d", checked);
                checked = checked + 1;
            end
        end
        if (checked == TEST_SIZE) begin
            $display("=======SUCCESS. Tested %d random stimulus without errors.=======", checked);
            $finish;
        end
    end
    integer i = 0;
    initial begin
        // The DSP has a startup latency. We add a few clocks so we can get rid of it.
        vld_in = 0;
        for (int i = 0; i < 20; i++) begin
           Tick;
        end
        for (int i = 0; i < TEST_SIZE; i++) begin
            Tick;
            vld_in = 1;
            t_in = mkStimulus();
            in_x = t_in.op1;
            in_y = t_in.op2;
            carry_in = t_in.ci;
            select = t_in.sel;
            mask = t_in.msk;
            funct = t_in.fn;
            test_queue.push_back(t_in);
            // if (t_in.fn == MULH || t_in.fn == MUL) begin
            //     Tick;
            //     i --;
            //     // Tick;
            //     vld_in = 0;
            //     // Tick;
            // end

        end
        Tick;
        vld_in = 0;
        while(checked != TEST_SIZE) begin
            Tick;
            if (clock_counter == TEST_SIZE + 1000) begin
                $error("@%d timed out with only %d checked results!", clock_counter, checked);
                $stop;
            end
        end


    end

endmodule
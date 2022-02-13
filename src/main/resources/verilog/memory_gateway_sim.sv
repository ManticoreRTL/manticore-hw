module memory_gateway_sim (
    input wire clock,
    input wire reset,
    input wire ap_start,
    output wire ap_done,
    output wire ap_idle,
    output wire ap_ready,
    input wire [63:0] memory_pointer,
    input wire [63:0] addr,
    input wire [15:0] wdata,
    input wire wen,
    output wire [15:0] ap_return
);

  localparam MEM_SIZE = 1 << 22;
  parameter READ_LATENCY = 77;
  parameter WRITE_LATENCY = 77;
  logic [63:0] addr_reg;
  logic [15:0] rdata;
  logic [15:0] wdata_reg;
  logic [15:0] mem[0 : MEM_SIZE - 1];
  logic [31:0] counter;
  typedef enum logic [2:0] {
    Idle,
    WriteWait,
    ReadWait,
    ReadDone,
    WriteDone
  } state_t;

  state_t pstate;
  state_t nstate;


  always_comb begin
    if (pstate == ReadDone) begin
      rdata = mem[addr_reg];
    end else begin
      rdata = 0;
    end
    case (pstate)
      Idle: begin
        if (ap_start && wen) begin
          nstate = WriteWait;
        end else if (ap_start) begin
          nstate = ReadWait;
        end else begin
          nstate = Idle;
        end
      end
      WriteWait: begin
        if (counter == WRITE_LATENCY - 1) begin
          nstate = WriteDone;
        end else begin
          nstate = WriteWait;
        end
      end
      ReadWait: begin
        if (counter == READ_LATENCY - 1) begin
          nstate = ReadDone;
        end else begin
          nstate = ReadWait;
        end
      end
      ReadDone: begin
        nstate = Idle;
      end
      WriteDone: begin
        nstate = Idle;
      end
      default: begin
        nstate = Idle;
      end
    endcase
  end


  assign ap_done   = (pstate == ReadDone || pstate == WriteDone);
  assign ap_ready  = (pstate == ReadDone || pstate == WriteDone);
  assign ap_idle   = (pstate == Idle);
  assign ap_return = rdata;

  always_ff @(posedge clock) begin
    if (reset) begin
      pstate  <= Idle;
      counter <= 0;
    end else begin
      pstate <= nstate;
      if (pstate == Idle) begin
        counter   <= 0;
        addr_reg  <= addr;
        wdata_reg <= wdata;
      end else begin
        counter <= counter + 1;
      end

      if (pstate == WriteDone) begin
        mem[addr_reg] <= wdata_reg;
      end
    end
  end


  initial begin
    $readmemb("exec.dat", mem);
  end

endmodule

/**
 * A simulation model for the memory banks without going through the axi master
 * interface that is part of the xrt compatible shell. This files should only be
 * used for simulation using Verilator and be accompanied by appropriate DPI 
 * hooks.
 *
 **/
module memory_gateway #(
    parameter CACHE_LINE_WIDTH = 256
) (
    input bit clock,
    input bit [1:0] cmd,
    input bit start,
    input bit [CACHE_LINE_WIDTH - 1 : 0] wline,
    input bit [63:0] raddr,
    input bit [63:0] waddr,
    output bit [CACHE_LINE_WIDTH - 1 : 0] rline,
    output bit done,
    output bit idle,
    output bit ready,
    input bit [63:0] memory_pointer
);

  import "DPI-C" function void write_long_word(
    chandle obj,
    longint addr,
    longint word
  );
  import "DPI-C" function longint read_long_word(
    chandle obj,
    longint addr
  );
  import "DPI-C" function int get_read_latency(
    chandle obj,
    longint addr
  );
  import "DPI-C" function int get_write_latency(
    chandle obj,
    longint addr
  );
  import "DPI-C" function void throw_error(string msg);

  localparam CMD_READ = 0, CMD_WRITE = 1, CMD_WRITE_BACK = 2;

  typedef enum logic [2:0] {
    s_idle  = 0,
    s_dpi   = 1,
    s_delay = 2,
    s_done  = 3
  } state_t;
  state_t pstate = s_idle, nstate;

  logic [31:0] delay_counter = 0;
  logic [1:0] cmd_reg;
  logic [CACHE_LINE_WIDTH - 1 : 0] wline_reg;
  logic [63 : 0] waddr_reg;
  logic [63 : 0] raddr_reg;
  int delay_value;
  task read_line(input longint base_addr);
    int j;
    for (j = 0; j < (CACHE_LINE_WIDTH / 64); j = j + 1) begin
      rline[j*64+:64] <= read_long_word(memory_pointer, base_addr + j * 8);
    end
  endtask

  task write_line(input longint base_addr);
    int j;
    for (j = 0; j < (CACHE_LINE_WIDTH / 64); j = j + 1) begin
      write_long_word(memory_pointer, base_addr + j * 8, wline[j*64+:64]);
    end
  endtask

  initial begin


  end
  always_comb begin
    case (pstate)
      s_idle:  nstate = (start) ? s_dpi : s_idle;
      s_dpi:   nstate = s_delay;
      s_delay: nstate = (delay_counter == 0) ? s_done : s_delay;
      s_done:  nstate = s_idle;
      default: throw_error("Invalid state in memory gate way!");
    endcase
  end

  always_ff @(posedge clock) begin
    if (pstate == s_idle) begin
      cmd_reg   <= cmd;
      wline_reg <= wline;
      raddr_reg <= raddr;
      waddr_reg <= waddr;
    end
    if (pstate == s_dpi) begin
      case (cmd_reg)
        CMD_READ: begin
          delay_counter <= get_read_latency(memory_pointer, raddr_reg);
          read_line(raddr_reg);
        end
        CMD_WRITE: begin
          delay_counter <= get_write_latency(memory_pointer, waddr_reg);
          write_line(waddr_reg);
        end
        CMD_WRITE_BACK: begin
          delay_counter <= get_write_latency(
              memory_pointer, waddr_reg
          ) + get_read_latency(
              memory_pointer, raddr_reg
          );
          write_line(waddr_reg);
          read_line(raddr_reg);
        end
        default: begin
          throw_error("invalid command in memory gate way!");
        end
      endcase
    end

    pstate <= nstate;
  end

  assign done  = (pstate == s_done);
  assign idle  = (pstate == s_idle);
  assign ready = (pstate == s_done);
endmodule

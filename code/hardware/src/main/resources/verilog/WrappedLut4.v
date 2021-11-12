module WrappedLut4
#(parameter INIT = 16'h800 // defaults to x & y & u & v
)(
    input wire x, 
    input wire y,
    input wire u,
    input wire v,
    output wire out
);

`ifdef VERILATOR
    assign out = INIT >> {x, y, u, v};
`else 
    (* DONT_TOUCH = "yes" *)
    RAM64X1S #(
        .INIT({48'h0, INIT})
    ) lut_inst(
        .O(out),
        .A0(v),
        .A1(u),
        .A2(y),
        .A3(x),
        .A4(0),
        .A5(0),
        .D(),
        .WCLK(0),
        .WE(0)
    );
`endif

endmodule 
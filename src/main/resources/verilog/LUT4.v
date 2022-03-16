module LUT4
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
    LUT6 #(
        .INIT(INIT)
    ) lut_inst(
        .O(out),
        .I0(v),
        .I1(u),
        .I2(y),
        .I3(x),
        .I4(0),
        .I5(0)
    );
`endif

endmodule
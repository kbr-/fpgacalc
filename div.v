`default_nettype none

module div #(parameter BITS = 32) (
    input wire [BITS-1:0] n,
    input wire [BITS-1:0] d,
    input wire start,

    output wire [BITS-1:0] q,
    output wire [BITS-1:0] r,
    output wire rdy
);

initial rdy = 0;

endmodule;

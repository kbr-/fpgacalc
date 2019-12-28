`default_nettype none

module div #(parameter BITS = 32) (
    input wire clk,
    input wire [BITS-1:0] n,
    input wire [BITS-1:0] d,
    input wire start,

    output reg [BITS-1:0] q,
    output reg [BITS-1:0] r,
    output reg rdy
);

initial rdy = 0;

reg [clog2(BITS)-1:0] i;

reg [BITS-1:0] abs_n, abs_d;
reg sgn_n, sgn_d;

always @(posedge clk) begin
    if (start) begin
        abs_n <= n[BITS-1] ? -n : n;
        abs_d <= d[BITS-1] ? -d : d;
        sgn_n <= n[BITS-1];
        sgn_d <= d[BITS-1];
        i <= BITS - 1;
        r <= 0;
        rdy <= 0;
    end else if (!rdy) begin : loop
        reg [BITS-1:0] r_tmp;
        reg [BITS-1:0] q_tmp;
        reg qi;

        r_tmp = {r[BITS-2:0], abs_n[i]};
        qi    = r_tmp >= abs_d;
        if (qi)
            r_tmp = r_tmp - abs_d;
        q_tmp    = q;
        q_tmp[i] = qi;

        q <= q_tmp;
        r <= r_tmp;
        if (i == 0) begin
            rdy <= 1;
            if (sgn_n ^ sgn_d)
                q <= -q_tmp;
            if (sgn_n)
                r <= -r_tmp;
        end
        i <= i - 1;
    end
end

function integer clog2;
    input integer value;
    begin
        value = value-1;
        for (clog2 = 0; value > 0; clog2 = clog2 + 1)
            value = value >> 1;
    end
endfunction

endmodule

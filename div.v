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

reg set_rdy;
initial rdy     = 0;
initial set_rdy = 0;

reg [clog2(BITS)-1:0] i;
reg [BITS-1:0] curr_n, curr_d;
reg [BITS-1:0] last_curr_n, last_curr_d;

reg [BITS-1:0] r_tmp;

always @(posedge clk) begin
    last_curr_n <= curr_n;
    last_curr_d <= curr_d;

    if (start) begin
        i <= BITS - 1;
        r <= 0;
        set_rdy <= 0;
    end else if (!rdy) begin
        r_tmp = {r[BITS-2:0], curr_n[i]};

        q[i] <= r_tmp >= curr_d;
        r    <= q[i] ? r_tmp - curr_d : r_tmp;

        i <= i - 1;
        if (i == 0) begin
            set_rdy <= 1;
        end
    end
end

always @* begin
    curr_n = last_curr_n;
    curr_d = last_curr_d;
    rdy    = set_rdy;
    if (start) begin
        curr_n = n;
        curr_d = d;
        rdy = 0;
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

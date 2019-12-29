`default_nettype none

module top (
    input wire mclk,
    input wire [7:0] sw,
    input wire [3:0] btn,

    output wire [7:0] led,
    output wire [3:0] an,
    output wire [6:0] seg,
    output wire dp
);

reg [7:0] sw1, sw2;
reg [3:0] btn1, btn2;
initial btn1 = 0;
initial btn2 = 0;

always @(posedge mclk) begin
    sw1 <= sw;
    sw2 <= sw1;

    btn1 <= btn;
    btn2 <= btn1;
end

wire [31:0] top;
wire empty;

calc CALC(
    .clk(mclk), .sw(sw2), .btn(btn2),
    .out_error(led[7]), .out_top(top), .out_stack_size(led[6:0]), .out_empty(empty)
);

display DISP(
    .clk(mclk), .d(btn[0] ? top[31:16] : top[15:0]), .is_d(~empty),
    .an(an), .seg(seg)
);

assign dp = 1;

endmodule

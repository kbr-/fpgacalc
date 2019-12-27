`default_nettype none

module display (
    input wire clk,

    input wire [15:0] d,
    input wire is_d,

    output reg [3:0] an,
    output reg [6:0] seg
);

// Which digit is currently being displayed, in {0, .., 3}
reg [1:0] digit_idx;
initial digit_idx = 3;

// Which step of displaying the current digit are we in
reg [2:0] display_state;
localparam CHARGE    = 3'b001;
localparam SHOW      = 3'b010;
localparam DISCHARGE = 3'b100;
initial display_state = DISCHARGE;

// Counts cycles to change `display_state`
// and/or `which_digit` (if `display_state` goes from DISCHARGE to CHARGE)
reg [13:0] display_ctr;
initial display_ctr = 0;

integer i;
always @(posedge clk) begin
    display_ctr <= display_ctr - 1;

    case (display_state)
    CHARGE: begin
        if (display_ctr == 0) begin
            display_state <= SHOW;
            display_ctr <= 14336;

            for (i = 0; i < 4; i = i + 1) begin
                an[i] <= 1;
            end

            an[digit_idx] <= 0;
        end
    end
    SHOW: begin
        if (display_ctr == 0) begin
            display_state <= DISCHARGE;
            display_ctr <= 1024;

            for (i = 0; i < 4; i = i + 1) begin
                an[i] <= 1;
            end
        end
    end
    DISCHARGE: begin
        if (display_ctr == 0) begin
            display_state <= CHARGE;
            display_ctr <= 1024;

            digit_idx <= digit_idx + 1;
            if (digit_idx == 3)
                digit_idx <= 0;

            for (i = 0; i < 4; i = i + 1) begin
                an[i] <= 1;
            end
        end
    end
    endcase
end

reg [3:0] chosen_digit;
always @* begin
    chosen_digit = d[3:0];
    case (digit_idx)
    1: chosen_digit = d[7:4];
    2: chosen_digit = d[11:8];
    3: chosen_digit = d[15:12];
    endcase
end

always @* begin
    seg = ~7'b1000000; // '-'
    if (is_d) case (chosen_digit)
     0: seg = ~7'b0111111;
     1: seg = ~7'b0000110;
     2: seg = ~7'b1011011;
     3: seg = ~7'b1001111;
     4: seg = ~7'b1100110;
     5: seg = ~7'b1101101;
     6: seg = ~7'b1111101;
     7: seg = ~7'b0100111;
     8: seg = ~7'b1111111;
     9: seg = ~7'b1101111;
    10: seg = ~7'b1110111;
    11: seg = ~7'b1111100;
    12: seg = ~7'b0111001;
    13: seg = ~7'b1011110;
    14: seg = ~7'b1111001;
    15: seg = ~7'b1110001;
    endcase
end

endmodule

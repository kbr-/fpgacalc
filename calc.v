`default_nettype none

module calc (
    input wire clk,
    input wire [7:0] sw,
    input wire [3:0] btn,

    output reg out_error,
    output reg [15:0] out_top,
    output reg [6:0] out_stack_size,
    output reg out_empty
);

reg [15:0] prev_out_top;
initial out_error      = 0;
initial out_stack_size = 0;
initial out_empty      = 1;

reg [6:0] state, next_state;
localparam IDLE         = 7'b0000001;
localparam OP_ADD       = 7'b0000010;
localparam OP_SUB       = 7'b0000100;
localparam OP_MUL       = 7'b0001000;
localparam OP_DIV_START = 7'b0010000;
localparam OP_DIV_WAIT  = 7'b0100000;
localparam OP_SWITCH    = 7'b1000000;
initial state      = IDLE;
initial next_state = IDLE;

reg [31:0] stack [511:0];
reg [9:0] sp, next_sp;
localparam SP_EMPTY = 10'b1111111111;
initial sp      = SP_EMPTY;
initial next_sp = SP_EMPTY;

reg set_top, prev_set_top;
reg [31:0] set_top_val, prev_set_top_val;
reg [31:0] read_top;
initial set_top = 0;

reg [31:0] top, prev_top;

reg error, next_error;
initial error      = 0;
initial next_error = 0;

reg div_start;
reg div_mux, next_div_mux;
reg  [31:0] div_n, div_d;
wire [31:0] div_q, div_r;
wire div_rdy;
initial div_start = 0;

div DIV (
    .clk(clk),
    .n(div_n), .d(div_d), .start(div_start),
    .q(div_q), .r(div_r), .rdy(div_rdy)
);

reg [3:0] prev_btn;
initial prev_btn = 0;

always @(posedge clk) begin
    sp <= next_sp;
    error <= next_error;
    state <= next_state;
    div_mux <= next_div_mux;

    prev_out_top <= out_top;
    prev_top <= top;
    prev_set_top <= set_top;
    prev_set_top_val <= set_top_val;
    prev_btn <= btn;

    read_top <= stack[next_sp[8:0]];
    if (set_top)
        stack[next_sp[8:0]] <= set_top_val;

    if (next_state == IDLE) begin
        out_stack_size <= next_sp + 1;
        out_empty <= next_sp[9];
        out_error <= next_error;
    end
end

always @* begin
    next_error = error;
    next_sp = sp;
    next_div_mux = div_mux;
    next_state = IDLE;

    top = prev_set_top ? prev_set_top_val : read_top;
    set_top = 0;
    set_top_val = 32'hxxxxxxxx;

    div_n = 32'hxxxxxxxx;
    div_d = 32'hxxxxxxxx;
    div_start = 0;

    out_top = prev_out_top;
    if (state == IDLE)
        out_top = btn[0] ? top[31:16] : top[15:0];

    if (btn[3] && btn[0]) begin
        next_error = 0;
        next_sp = SP_EMPTY;
    end else case(state)
    IDLE: begin
        if (btn[1] && !prev_btn) begin
            if (sp == 511)
                next_error = 1;
            else begin
                next_sp = sp + 1;
                set_top = 1;
                set_top_val = {24'h000000, sw};
                next_error = 0;
            end
        end else if (btn[2] && !prev_btn) begin
            if (sp == SP_EMPTY)
                next_error = 1;
            else begin
                set_top = 1;
                set_top_val = {top[23:0], sw};
                next_error = 0;
            end
        end else if (btn[3] && !prev_btn) case (sw[2:0])
        3'b000: begin
            if (sp == SP_EMPTY || sp == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_state = OP_ADD;
            end
        end
        3'b001: begin
            if (sp == SP_EMPTY || sp == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_state = OP_SUB;
            end
        end
        3'b010: begin
            if (sp == SP_EMPTY || sp == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_state = OP_MUL;
            end
        end
        3'b011: begin
            if (sp == SP_EMPTY || sp == 0 || top == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_div_mux = 0;
                next_state = OP_DIV_START;
            end
        end
        3'b100: begin
            if (sp == SP_EMPTY || sp == 0 || top == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_div_mux = 1;
                next_state = OP_DIV_START;
            end
        end
        3'b101: begin
            if (sp == SP_EMPTY)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_error = 0;
            end
        end
        3'b110: begin
            if (sp == SP_EMPTY || sp == 511)
                next_error = 1;
            else begin
                next_sp = sp + 1;
                set_top = 1;
                set_top_val = top;
                next_error = 0;
            end
        end
        3'b111: begin
            if (sp == SP_EMPTY || sp == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                set_top = 1;
                set_top_val = top;
                next_state = OP_SWITCH;
            end
        end
        endcase
    end
    OP_ADD: begin
        set_top = 1;
        set_top_val = top + prev_top;
        next_error = 0;
    end
    OP_SUB: begin
        set_top = 1;
        set_top_val = top - prev_top;
        next_error = 0;
    end
    OP_MUL: begin
        set_top = 1;
        set_top_val = top * prev_top;
        next_error = 0;
    end
    OP_DIV_START: begin
        div_n = top;
        div_d = prev_top;
        div_start = 1;
        next_state = OP_DIV_WAIT;
    end
    OP_DIV_WAIT: begin
        if (div_rdy) begin
            set_top = 1;
            set_top_val = div_mux ? div_r : div_q;
            next_error = 0;
        end else
            next_state = OP_DIV_WAIT;
    end
    OP_SWITCH: begin
        next_sp = sp + 1;
        set_top = 1;
        set_top_val = read_top;
        next_error = 0;
    end
    endcase
end

endmodule

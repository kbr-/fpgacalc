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

initial out_error      = 0;
initial out_stack_size = 0;
initial out_empty      = 1;

reg [7:0] state, next_state;
localparam IDLE         = 8'b00000001;
localparam OP_ADD       = 8'b00000010;
localparam OP_SUB       = 8'b00000100;
localparam OP_MUL       = 8'b00001000;
localparam OP_DIV_START = 8'b00010000;
localparam OP_DIV_WAIT  = 8'b00100000;
localparam OP_SWITCH_1  = 8'b01000000;
localparam OP_SWITCH_2  = 8'b10000000;
initial state = IDLE;

reg [31:0] stack [511:0];
reg [9:0] sp, next_sp;
initial sp = -1;

reg [31:0] top, next_top, last_top;

reg set_top;
reg [31:0] set_top_val;
initial set_top = 0;

reg error, next_error;
initial error = 0;

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

always @(posedge clk) begin
    sp <= next_sp;
    error <= next_error;
    state <= next_state;
    div_mux <= next_div_mux;

    last_top <= top;

    next_top = set_top ? set_top_val : stack[next_sp[8:0]];
    top <= next_top;

    if (set_top)
        stack[next_sp[8:0]] <= set_top_val;

    if (next_state == IDLE) begin
        if (btn[0])
            out_top <= next_top[31:16];
        else
            out_top <= next_top[15:0];
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

    set_top = 0;
    set_top_val = 32'hxxxxxxxx;
    div_n = 32'hxxxxxxxx;
    div_d = 32'hxxxxxxxx;
    div_start = 0;

    if (btn[3] && btn[0]) begin
        next_error = 0;
        next_sp = -1;
    end else case(state)
    IDLE: begin
        if (btn[1]) begin
            if (sp == 511)
                next_error = 1;
            else begin
                next_sp = sp + 1;
                set_top = 1;
                set_top_val = {24'h000000, sw};
                next_error = 0;
            end
        end else if (btn[2]) begin
            if (sp == -1)
                next_error = 1;
            else begin
                set_top = 1;
                set_top_val = {top[23:0], sw};
                next_error = 0;
            end
        end else if (btn[3]) case (sw[2:0])
        3'b000: begin
            if (sp == -1 || sp == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_state = OP_ADD;
            end
        end
        3'b001: begin
            if (sp == -1 || sp == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_state = OP_SUB;
            end
        end
        3'b010: begin
            if (sp == -1 || sp == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_state = OP_MUL;
            end
        end
        3'b011: begin
            if (sp == -1 || sp == 0 || top == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_div_mux = 0;
                next_state = OP_DIV_START;
            end
        end
        3'b100: begin
            if (sp == -1 || sp == 0 || top == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_div_mux = 1;
                next_state = OP_DIV_START;
            end
        end
        3'b101: begin
            if (sp == -1)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_error = 0;
            end
        end
        3'b110: begin
            if (sp == -1 || sp == 511)
                next_error = 1;
            else begin
                next_sp = sp + 1;
                set_top = 1;
                set_top_val = top;
                next_error = 0;
            end
        end
        3'b110: begin
            if (sp == -1 || sp == 0)
                next_error = 1;
            else begin
                next_sp = sp - 1;
                next_state = OP_SWITCH_1;
            end
        end
        endcase
    end
    OP_ADD: begin
        set_top = 1;
        set_top_val = top + last_top;
        next_error = 0;
    end
    OP_SUB: begin
        set_top = 1;
        set_top_val = top - last_top;
        next_error = 0;
    end
    OP_MUL: begin
        set_top = 1;
        set_top_val = top * last_top;
        next_error = 0;
    end
    OP_DIV_START: begin
        div_n = top;
        div_d = last_top;
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
    OP_SWITCH_1: begin
        set_top = 1;
        set_top_val = last_top;
        next_state = OP_SWITCH_2;
    end
    OP_SWITCH_2: begin
        next_sp = sp + 1;
        set_top = 1;
        set_top_val = last_top;
        next_error = 0;
    end
    endcase
end

endmodule

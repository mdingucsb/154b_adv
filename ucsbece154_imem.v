// ucsbece154_imem.v
// All Rights Reserved
// Copyright (c) 2024 UCSB ECE
// Distribution Prohibited

`define MIN(A,B) (((A)<(B))?(A):(B))

module ucsbece154_imem #(
    parameter TEXT_SIZE = 64,
    parameter BLOCK_WORDS = 4,          // words per burst (must match cache)
    parameter T0_DELAY = 40             // first word delay (cycles)
) (
    input wire clk,
    input wire reset,

    input wire ReadRequest,
    input wire [31:0] ReadAddress,

    output reg [31:0] DataIn,
    output reg DataReady,
    output wire [1:0] block_index // need to change when changing parameter
);

  parameter LOG_BLOCK_WORDS = $clog2(BLOCK_WORDS);

  parameter idle = 2'b00,
            fetch = 2'b01,
            send_first = 2'b10,
            send_rest = 2'b11;
           
  reg [1:0] state_reg, state_next;
  reg fetch_start, send_start;
  reg [5:0] fetch_count;
  reg [31:0] send_count;
  reg [31:0] a_i_old;
  wire [31:0] base_address;

  reg [31:0] a_i;

  wire [31:0] rd_o;
  reg [LOG_BLOCK_WORDS:0] num_words;

// Implement SDRAM interface here

  assign base_address = {ReadAddress[31:LOG_BLOCK_WORDS+2], {LOG_BLOCK_WORDS+2{1'b0}}};

  always @(posedge clk) begin
      if (state_reg == fetch && state_next == send_first)
       num_words <= BLOCK_WORDS - block_index;
  end

  always @(posedge clk) begin
    if (state_reg == idle || state_next == fetch)
      a_i <= ReadAddress;
    else if (state_reg == send_first && state_next == send_rest) // after first word
        if (block_index == (BLOCK_WORDS - 1)) // jump to first word of block if needed
          a_i <= a_i - 4 * (BLOCK_WORDS - 1);
        else
          a_i <= a_i + 4;
    else if (state_reg == send_rest) begin
      if (block_index == (BLOCK_WORDS - 1)) // jump to first word of block if needed
        a_i <= a_i - 4 * (BLOCK_WORDS - 1);
      else
        a_i <= a_i + 4;
    end
    else
      a_i <= ReadAddress;
  end
  
  assign block_index = a_i[3:2];
   
  always @(*) begin
    if (state_reg == send_first || state_reg == send_rest) begin
      DataIn = rd_o;
    end
    else begin
      DataIn = 32'bx;
    end
  end
  // next state, fetch and send start logic
  always @(*) begin
    state_next = state_reg;
    fetch_start = 1'b0;
    send_start = 1'b0;
    DataReady = 1'b0;
    case (state_reg)
      idle: begin
        if (ReadRequest) begin
          state_next = fetch;
          fetch_start = 1'b1;
        end
      end
      fetch: begin
        if (fetch_count == T0_DELAY - 1) begin
          state_next = send_first;
          send_start = 1'b1;
        end
      end
      send_first: begin
        DataReady = 1'b1;
        state_next = send_rest;
      end
      send_rest: begin
        DataReady = 1'b1;
        if (send_count == num_words - 1) state_next = idle;
      end
      default: state_next = idle;
    endcase
  end

  // state reg
  always @(posedge clk) begin
    if (reset) begin
      state_reg <= idle;
    end else begin
      state_reg <= state_next;
    end
  end

  // fetch wait counter
  always @(posedge clk) begin
    if (reset || fetch_start) begin
      fetch_count <= 0;
    end else begin
      if (fetch_count == T0_DELAY - 1) fetch_count <= 0;
      else fetch_count <= fetch_count + 1;
    end
  end

  // send wait counter
  always @(posedge clk) begin
    if (reset || send_start) begin
      send_count <= 0;
    end else begin
      if (send_count == BLOCK_WORDS - 1) send_count <= 0;
      else send_count <= send_count + 1;
    end
  end

// instantiate/initialize BRAM
reg [31:0] TEXT [0:TEXT_SIZE-1];

// initialize memory with test program. Change this with your file for running custom code
initial $readmemh("text.dat", TEXT);

// calculate address bounds for memory
localparam TEXT_START = 32'h00010000;
localparam TEXT_END   = `MIN( TEXT_START + (TEXT_SIZE*4), 32'h10000000);

// calculate address width
localparam TEXT_ADDRESS_WIDTH = $clog2(TEXT_SIZE);

// create flags to specify whether in-range
wire text_enable = (TEXT_START <= a_i) && (a_i < TEXT_END);

// create addresses
wire [TEXT_ADDRESS_WIDTH-1:0] text_address = a_i[2 +: TEXT_ADDRESS_WIDTH]-(TEXT_START[2 +: TEXT_ADDRESS_WIDTH]);

// get read-data
wire [31:0] text_data = TEXT[ text_address ];

// set rd_o iff a_i is in range
assign rd_o =
    text_enable ? text_data :
    {32{1'bz}}; // not driven by this memory

`ifdef SIM
always @ * begin
    if (a_i[1:0]!=2'b0)
        $warning("Attempted to access invalid address 0x%h. Address coerced to 0x%h.", a_i, (a_i&(~32'b11)));
end
`endif

endmodule

`undef MIN

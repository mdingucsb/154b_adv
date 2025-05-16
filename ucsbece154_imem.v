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
    output reg DataReady
);

  localparam LOG_BLOCK_WORDS = $clog2(BLOCK_WORDS);

  parameter idle = 2'b00,
            fetch = 2'b01,
            send_first = 2'b10;
            send_rest = 2'b11;
            
  reg [1:0] state_reg, state_next;
  reg fetch_start, send_start;
  reg [5:0] fetch_count;
  reg [1:0] send_count;
  reg [31:0] first_address;

  wire [1:0] block_index;
  reg [1:0] block_count;

  reg [31:0] a_i;

  wire [31:0] rd_o;

// Implement SDRAM interface here

  assign block_index = ReadAddress[LOG_BLOCK_WORDS+1:2];

  always @(posedge clk) begin
    if (state_reg == idle)
      a_i <= ReadAddress;
    else if (state_reg == send)
      a_i <= a_i + 4;
    else
      a_i <= a_i;
  end
  
  always @(*) begin
    if (state_reg == send) begin
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
          state_next = send;
          send_start = 1'b1;
        end
      end
      send_first: begin
        DataReady = 1'b1;
      end
      send: begin
        DataReady = 1'b1;
        if (send_count == BLOCK_WORDS - 1) state_next = idle;
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

  always @(posedge clk) begin
    if (reset) begin
      
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

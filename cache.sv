module ucsbece154b_icache #(
  parameter NUM_SETS    = 8,
  parameter NUM_WAYS    = 4,
  parameter BLOCK_WORDS = 4,
  parameter WORD_SIZE   = 32
) (
  input logic clk,
  input logic reset,

  // core fetch interface
  input logic readEnable, // ~StallF
  input logic [31:0] readAddress, // PCNewF
  output logic [WORD_SIZE-1:0] instruction,
  output logic ready, // means valid instruction output, next readAddress can be input
  output logic busy, // if false, insert NO-OP instruction to fetch (addi, x0, x0, 0)

  // SDRAM-controller interface
  input logic [31:0] memDataIn, // word data input from SDRAM
  input logic memDataReady, // raised when block data tranmission starts, lowered by SDRAM after all words supplied
  output logic [31:0] memReadAddress, // readAddress from cache, output after cache miss
  output logic memReadRequest, // asserted when cache miss, lowered when data ready received (assert for roughly T0)
  input logic FlushD,
  output logic FlushD_alt,
  output logic read_to_delay,
  input logic [1:0] memBlockIndex // need to change when changing parameters
);

  parameter LOG_NUM_SETS = $clog2(NUM_SETS);
  parameter LOG_NUM_WAYS = $clog2(NUM_WAYS);
  parameter LOG_BLOCK_WORDS = $clog2(BLOCK_WORDS);

  integer i, j, k, l, m;

  typedef enum logic [1:0] {read, delay, words, write} state_t;
  state_t stateNext, stateReg;

  typedef logic [WORD_SIZE-1:0] word_t; // a word is 32 bits
  typedef word_t block_t [0:BLOCK_WORDS-1]; // a line is 4 words (128 bits total)

  typedef struct { // a way is {v, u, tag, block}
    logic v;
    logic [29-LOG_NUM_SETS-LOG_BLOCK_WORDS:0] tag;
    block_t data; 
  } way_t;


  typedef way_t set_t [0:NUM_WAYS-1]; // 4 ways in a set
  set_t SRAM [0:NUM_SETS-1]; // 8 sets in the memory

  logic [29-LOG_BLOCK_WORDS:0] bufferTag;
  logic [31:0] bufferData [0:3];

  logic [29-LOG_NUM_SETS-LOG_BLOCK_WORDS:0] tagIndex;
  logic [LOG_NUM_SETS-1:0] setIndex;
  logic [LOG_NUM_SETS-1:0] setIndex_w;
  logic [LOG_NUM_SETS-1:0] setIndex_i;
  logic [LOG_BLOCK_WORDS-1:0] blockIndex;
  logic cacheHit;
  logic [31:0] DataIn;
  logic [29-LOG_NUM_SETS-LOG_BLOCK_WORDS:0] memTagIndex;
  
  logic [LOG_NUM_WAYS-1:0] hitWay;

  logic [LOG_BLOCK_WORDS-1:0] words_wait_counter;

  logic [15:0] lfsr;
  logic [1:0] randBits;

  logic FlushD_tmp;
  logic [LOG_BLOCK_WORDS:0] num_words;

  assign tagIndex = readAddress[31:LOG_NUM_SETS+LOG_BLOCK_WORDS+2]; // [31:7]
  assign setIndex = readAddress[LOG_NUM_SETS+LOG_BLOCK_WORDS+1:LOG_BLOCK_WORDS+2]; // [6:4]
  assign blockIndex = readAddress[LOG_BLOCK_WORDS+1:2]; // [3:2]
  assign memTagIndex = memReadAddress[31:LOG_NUM_SETS+LOG_BLOCK_WORDS+2];

  always_ff @(posedge clk) begin
    if (stateReg == delay && stateNext == words)
       num_words <= BLOCK_WORDS - readAddress[3:2];
  end
  always_comb begin
    // defaults
    busy = 1'b0;
    memReadAddress = readAddress;
    memReadRequest = 1'b0;
    ready = 1'b0;
    instruction = 32'h00000013;
    cacheHit = 1'b0;
    hitWay = 2'b00;

    // output logic
    case (stateReg)
      read: begin
        for (l = 0; l < NUM_WAYS; l++) begin
          if (tagIndex == SRAM[setIndex][l].tag && SRAM[setIndex][l].v) begin // if match and valid
            cacheHit = 1'b1;
            hitWay = l;
          end
        end
        if (cacheHit) begin
          instruction = SRAM[setIndex][hitWay].data[blockIndex];
          ready = 1'b1;
        end else begin
          instruction = 32'h00000013;
          memReadRequest = 1'b1;
        end
      end
      delay: begin
        memReadRequest = 1'b1;
        if (stateNext == write)
	  instruction = 32'h00000013;
        if (stateNext == words) begin
          ready = 1'b1;
          for (l = 0; l < NUM_WAYS; l++) begin
            if (tagIndex == SRAM[setIndex][l].tag && SRAM[setIndex][l].v) begin // if match and valid
              cacheHit = 1'b1;
              hitWay = l;
            end
          end
          if (cacheHit)
            instruction = SRAM[setIndex][hitWay].data[blockIndex];
	  else
	    instruction = memDataIn;
        end
      end
      words: begin
        if (stateNext == words) begin
          ready = 1'b1;
          for (l = 0; l < NUM_WAYS; l++) begin
            if (tagIndex == SRAM[setIndex][l].tag && SRAM[setIndex][l].v) begin // if match and valid
              cacheHit = 1'b1;
              hitWay = l;
            end
          end
          if (cacheHit)
            instruction = SRAM[setIndex][hitWay].data[blockIndex];
	  else
	    instruction = memDataIn;
        end
      end
      write: begin
        busy = 1'b1;
      end
    endcase;
  end

  // SRAM reset, read, and write
  always_ff @(posedge clk) begin
    if (reset) begin
      for (i = 0; i < NUM_SETS; i++) begin
        for (j = 0; j < NUM_WAYS; j++) begin
          SRAM[i][j].v = 1'bx;
          SRAM[i][j].tag = 25'bx;
          for (k = 0; k < BLOCK_WORDS; k++) begin
            SRAM[i][j].data[k] = 32'bx;
          end
        end
      end
    end else begin
      DataIn <= memDataIn;
      if (stateNext == words) begin
        bufferTag <= memTagIndex;
        bufferData [memBlockIndex] <= memDataIn;
      end
      if (stateNext == write) begin
        SRAM[setIndex_w][randBits].v <= 1;
        SRAM[setIndex_w][randBits].tag <= tagIndex;
        for (m = 0; m < BLOCK_WORDS; m++) begin
          SRAM[setIndex_w][randBits].data[m] <= bufferData[m];
        end
      end
    end
  end

  // counter for words_wait
  always_ff @(posedge clk) begin
    if (reset || (stateReg == delay && stateNext == words)) begin // set to 0 upon reset or transition to write
      words_wait_counter <= 0;
    end else begin
      if (words_wait_counter == BLOCK_WORDS - 1)
        words_wait_counter <= 0;
      else
        words_wait_counter <= words_wait_counter + 1;
    end
  end

  // next state logic
  always_comb begin
    stateNext = stateReg;
    case (stateReg)
      read: begin
        if (readEnable) begin
          if (cacheHit)
            stateNext = read;
          else
            stateNext = delay;
        end else
          stateNext = delay;
      end
      delay: begin
        if (memDataReady)
          stateNext = words;
        if (cacheHit)
          stateNext = read;
      end
      words: begin
        if (words_wait_counter == num_words - 1)
          stateNext = write;
      end
      write: begin
        stateNext = read;
/*
        if (cacheHit)
          stateNext = read;
        else
          stateNext = delay;
*/
      end
    endcase;
  end

  // state register
  always_ff @(posedge clk) begin
    if (reset) begin
      stateReg <= read;
      setIndex_w <= 0;
      setIndex_i <= 0;
    end else begin
      stateReg <= stateNext;
      setIndex_w <= setIndex_i;
      setIndex_i <= setIndex;
    end
  end


  // FlushD_alt logic (latching FlushD during memory fetch delay)
  
  logic r_to_d;
  logic w_to_r;

  always @(posedge clk) begin
    r_to_d <= (stateReg == read && stateNext == delay);
    w_to_r <= (stateReg == write && stateNext == read);
  end


  always_ff @(posedge clk) begin
    if (r_to_d) // latch value one cycle after stateReg == read AND stateNext == delay
      FlushD_tmp <= FlushD;
    else
      FlushD_tmp <= FlushD_tmp;
    if (stateReg == write && stateNext == read) // update value 1 cycle after w_to_r transition
      FlushD_alt <= FlushD_tmp;
    else
      FlushD_alt <= 1'b0;
  end

  assign read_to_delay = (stateReg == read && stateNext == delay);

  // random bits generator

always_ff @(posedge clk) begin
  if (reset) begin
    lfsr <= 16'b1010_0000_0000_0000;  // Any non-zero seed
  end else if (stateReg == read && stateNext == delay) begin
    lfsr <= {lfsr[14:0], lfsr[15] ^ lfsr[13] ^ lfsr[12] ^ lfsr[10]};
  end
end


  assign randBits = lfsr[LOG_BLOCK_WORDS-1:0];

  

endmodule

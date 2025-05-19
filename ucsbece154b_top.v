// ucsbece154b_top.v
// ECE 154B, RISC-V pipelined processor 
// All Rights Reserved
// Copyright (c) 2024 UCSB ECE
// Distribution Prohibited

module ucsbece154b_top #(
  parameter NUM_SETS    = 8,
  parameter NUM_WAYS    = 4,
  parameter BLOCK_WORDS = 4,
  parameter WORD_SIZE   = 32
) (
    input clk, reset
);

parameter LOG_BLOCK_WORDS = $clog2(BLOCK_WORDS);

wire [31:0] pc, instr, readdata;
wire [31:0] writedata, dataadr;
wire  memwrite, Readenable, busy;
wire [31:0] SDRAM_ReadAddress;
wire [31:0] SDRAM_DataIn;
reg [31:0] instrF;
wire SDRAM_ReadRequest;
wire SDRAM_DataReady;
wire ReadyF;
wire [31:0] PCnewF;
wire FlushD;
wire read_to_delay;
wire [LOG_BLOCK_WORDS-1:0] BlockIndex;

assign instrF = busy ? 32'h00000013 : instr;

ucsbece154b_icache icache (
    .clk(clk),
    .reset(reset),
    .readEnable(Readenable),          
    .readAddress(pc),
    .instruction(instr),
    .ready(ReadyF),
    .busy(busy),                   
    .memReadAddress(SDRAM_ReadAddress),
    .memReadRequest(SDRAM_ReadRequest),
    .memDataIn(SDRAM_DataIn),
    .memDataReady(SDRAM_DataReady),
    .FlushD (FlushD),
    .FlushD_alt (FlushD_alt),
    .read_to_delay (read_to_delay),
    .memBlockIndex (BlockIndex)
);


// processor and memories are instantiated here
ucsbece154b_riscv_pipe riscv (
    .clk(clk), .reset(reset),
    .PCF_o(pc),
    .InstrF_i(instrF),
    .MemWriteM_o(memwrite),
    .ALUResultM_o(dataadr), 
    .WriteDataM_o(writedata),
    .ReadDataM_i(readdata),
    .ReadyF(ReadyF), //added Ready instruction to stall fetch stage in case of cache miss
    .ReadEnable(Readenable),
    .Busy (busy),
    .PCnewF (PCnewF),
    .FlushD (FlushD),
    .FlushD_alt (FlushD_alt),
    .read_to_delay (read_to_delay)
);
ucsbece154_imem imem (
    .clk(clk),
    .reset(reset),

    .ReadRequest(SDRAM_ReadRequest),
    .ReadAddress(SDRAM_ReadAddress),
    .DataIn(SDRAM_DataIn),
    .DataReady(SDRAM_DataReady),
    .block_index(BlockIndex)
);
ucsbece154_dmem dmem (
    .clk(clk), .we_i(memwrite),
    .a_i(dataadr), .wd_i(writedata),
    .rd_o(readdata)
);

endmodule

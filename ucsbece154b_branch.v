// ucsbece154_branch.v
// All Rights Reserved
// Copyright (c) 2024 UCSB ECE
// Distribution Prohibited


module ucsbece154b_branch 
#(
    parameter NUM_BTB_ENTRIES = 32,  // should be power of 2 and > 1
    parameter NUM_GHR_BITS    = 5,
    parameter NUM_PHT_ENTRIES = 128  // should be power of 2 and > 1
 ) (
    input               clk, 
    input               reset_i,
    input        [31:0] pc_i,
    input        [31:0] BTBwriteaddress_i,
    input        [31:0] BTBwritedata_i,
    input               BTBwriteB_i,   
    input               BTBwe_i, 
    output       [31:0] BTBtarget_o,  
    output              BranchTaken_o,
    input         [6:0] op_i, 
    input               PHTincrement_i, 
    input               GHRreset_i,
    input               PHTwe_i,
//    input    [NUM_GHR_BITS-1:0]  PHTwriteindex_i,
    input    [$clog2(NUM_PHT_ENTRIES)-1:0]  PHTwriteindex_i,
 //   output   [NUM_GHR_BITS-1:0]  PHTreadindex_o
    output   [$clog2(NUM_PHT_ENTRIES)-1:0]  PHTreadindex_o

);

`include "ucsbece154b_defines.vh"

// Predecoder
reg branch_instr; 

always @ * begin
   case(op_i)
      instr_branch_op:  branch_instr = 1'b1;
      default:   	branch_instr = 1'b0; 
   endcase
end


// BTB  
reg [29:0] 			   BTBtag    [0:NUM_BTB_ENTRIES-1]; 
reg [29-$clog2(NUM_BTB_ENTRIES):0] BTBtarget [0:NUM_BTB_ENTRIES-1]; 
reg 				   BTBJflag  [0:NUM_BTB_ENTRIES-1]; 
reg 				   BTBBflag  [0:NUM_BTB_ENTRIES-1]; 

// Read
wire [$clog2(NUM_BTB_ENTRIES)-1:0] btbreadind = pc_i[$clog2(NUM_BTB_ENTRIES)+1:2];  
reg jumphit, branchhit;

// reg [0:0] temp = 1;

assign BTBtarget_o = {BTBtarget[btbreadind],2'b0};

always @ * begin
 if (BTBtag[btbreadind] == pc_i[31:$clog2(NUM_BTB_ENTRIES)+2]) begin
   jumphit   = BTBJflag[btbreadind];
   branchhit = BTBBflag[btbreadind];
 end else begin
   jumphit   = 1'b0;
   branchhit = 1'b0;
 end
end

// Write 
wire [$clog2(NUM_BTB_ENTRIES)-1:0] btbwriteind = BTBwriteaddress_i[$clog2(NUM_BTB_ENTRIES)+1:2];  
integer c;

always @ (posedge clk) begin
    if (reset_i) begin
        for(c=0; c<NUM_BTB_ENTRIES; c=c+1) begin
             BTBtag[c]    <= {30-$clog2(NUM_BTB_ENTRIES){1'b0}};
             BTBtarget[c] <= 30'b0;
             BTBJflag[c]  <= 1'b0;
             BTBBflag[c]  <= 1'b0;
        end 
    end else if (BTBwe_i) begin 
        BTBtag[btbwriteind]    <= BTBwriteaddress_i[31:$clog2(NUM_BTB_ENTRIES)+2];
        BTBtarget[btbwriteind] <= BTBwritedata_i[31:2];
        BTBJflag[btbwriteind]  <= ~BTBwriteB_i;
        BTBBflag[btbwriteind]  <= BTBwriteB_i;
    end  
end


// Branch predictor
reg   [NUM_GHR_BITS-1:0]  GHR; 
//reg [1:0]  PHT [0:2**NUM_GHR_BITS-1];
reg [1:0]  PHT [0:NUM_PHT_ENTRIES-1];  


// Read
wire predict_taken;

assign PHTreadindex_o = {GHR,{($clog2(NUM_PHT_ENTRIES)-NUM_GHR_BITS){1'b0}}} ^ pc_i[$clog2(NUM_PHT_ENTRIES)+1:2]; // XORING BITS OF GHR AND PC 


// assign PHTreadindex_o = GHR ^ pc_i[NUM_GHR_BITS)+1:2]; // XORING BITS OF GHR AND PC   
assign predict_taken = PHT[PHTreadindex_o][1];


// Write PHT

// first read the counter value at write index 
// Note that this leads to implemnetaiton with two read ports and also increase the critical path delay. A more optimal would be to propagate counter value read at fetch stage via pipeline

wire [1:0] counteroldvalue = PHT[PHTwriteindex_i];
reg [1:0] counternewvalue;

always @ * begin
     case (PHTincrement_i) 
       1'b1:case (counteroldvalue)        // increment
               2'b00: counternewvalue <= 2'b01;
 	       2'b01: counternewvalue <= 2'b10;
 	       2'b10: counternewvalue <= 2'b11;
 	       2'b11: counternewvalue <= 2'b11;
             default: counternewvalue <= 2'bxx;
            endcase
      1'b0: case (counteroldvalue)       // decrement
               2'b00: counternewvalue <= 2'b00;
     	       2'b01: counternewvalue <= 2'b00;
 	       2'b10: counternewvalue <= 2'b01;
 	       2'b11: counternewvalue <= 2'b10;
             default: counternewvalue <= 2'bxx;
            endcase
   default: counternewvalue <= 2'bxx;
   endcase
end 

always @ (posedge clk) begin
    if (reset_i) begin
//        for(c=0; c<2**NUM_GHR_BITS; c=c+1) PHT[c] <= 2'b00;
        for(c=0; c<NUM_PHT_ENTRIES; c=c+1) PHT[c] <= 2'b00;
    end else if (PHTwe_i) begin 
        PHT[PHTwriteindex_i]  <=  counternewvalue;
    end 
end

// Write GHR

always @ (posedge clk) begin
    if (reset_i | GHRreset_i) GHR <= {NUM_GHR_BITS{1'b0}};
    else if (branch_instr & (NUM_GHR_BITS>1)) GHR <= {predict_taken, GHR[NUM_GHR_BITS-1:1]};
end


assign BranchTaken_o = (branchhit & predict_taken)| jumphit;

endmodule

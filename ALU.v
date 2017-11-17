
// Buttons: { AAns, BAns, Clr, AnsB, AnsA, ldB, ldA, ^, ~, |, &, x, -, +}
// BusIn:   16 bit input 
// Error:   { (+)overflow, (-)underflow, (x)truncation }
// Flags:   { negative, A>B, A=B, B>A }
// Clock:   Clock that causes the registers to set
module ALU(buttons, busIn, busOut, error, flags, clk);
	parameter n = 16;
	
	input [n-1:0] busIn;
	input [13:0] buttons; // AAns, BAns, Clr, AnsB, AnsA, ldB, ldA, ^, ~, |, &, x, -, +
	input clk;
	
	output [n-1:0] busOut;
	output [3:0] flags; // negative, A>B, A=B, A<B
	wire [2:0] flagBuffer;
	
	
	output [2:0] error; // +ovfl, -udlf, xtrunc
	wire [2:0] errBuffer;
	
	wire [3:0] enc, opIn, opOut;
	
						 // opMux   A    B
	wire [18:0] control; // [ 8:0][3:0][3:0]
						 // [18:8][7:4][3:0]
	
	
	
	wire [n-1:0] Ain, Aout;
	wire [n-1:0] Bin, Bout;
	
	wire [n-1:0] addW, subW, mltW, andW, orW, xorW, notW;
	wire [n-1:0] ansIn;
	
	
	// {1'b0,buttons,1'b0} is the buttons input because:
	//   the encoder takes 16 bits in and buttons is only 14 bits
	//   0x0 is No Operation (Ø), which we want to happen when:
	//     no buttons pressed,
	//	   more than one button press at a time,
	//   which all map to a 1 being passed to the encoder
	Encoder16x4 encode ({1'b0,buttons,1'b0}, enc);
	
	
	Mux2 #(4) opMux( 4'b0000, enc, opOut[0]|opOut[1]|opOut[2]|opOut[3] , opIn);
	
	DFF #(4) opReg(clk, opIn, opOut);
	
	
	
	
	
	// CONTROL LOGIC	
	//	4bit			11bit   4bit	4bit
	//	opCode	name	Ans		A       B
	//	0		Ø		0		0		0		No Operation		(Ø)
	//	1		+		2		0		0		Addition			(Ans = A + B)
	//	2		-		3		0		0		Subtraction			(Ans = A - B)
	//	3		x		4		0		0		Multiplication		(Ans = A x B)
	//	4		&		5		0		0		Bitwise A AND B		(Ans = A & B)
	//	5		|		6		0		0		Bitwise A OR B		(Ans = A | B)
	//	6		~		8		0		0		Bitwise NOT A		(Ans = ~A)
	//	7		^		7		0		0		Bitwise A XOR B		(Ans = A ^ B)
	//	8		ldA		0		1		0		Load input into A	(A = input)
	//	9		ldB		0		0		1		Load input into B	(B = input)
	//	10		Ans-A	0		2		0		Move Ans to A 		(A = Ans)
	//	11		Ans-B	0		0		2		Move Ans to B 		(B = Ans)
	//	12		Clr		1		3		3		Clear all registers (A,B,Ans = 0)
	//	13		A-Ans	9		0		0		Move A to Ans 		(Ans = A)
	//	14		B-Ans	10		0		0		Move B to Ans 		(Ans = B)
	//	15		-		-		-		-		
	// 
	// Table to Opcode : (1 << table[operation])
	
	// Module Opcode (OpCtrlAns)
	assign control[18:8] =  (((11'd1) <<  0) & ({11{opOut ==   0}}|
												{11{opOut ==   8}}|
												{11{opOut ==   9}}|
												{11{opOut ==  10}}|
												{11{opOut ==  11}})) |
							(((11'd1) <<  1) & {11{opOut == 12}}) |
							(((11'd1) <<  2) & {11{opOut ==  1}}) | 
							(((11'd1) <<  3) & {11{opOut ==  2}}) |
							(((11'd1) <<  4) & {11{opOut ==  3}}) |
							(((11'd1) <<  5) & {11{opOut ==  4}}) |
							(((11'd1) <<  6) & {11{opOut ==  5}}) |
							(((11'd1) <<  7) & {11{opOut ==  7}}) |
							(((11'd1) <<  8) & {11{opOut ==  6}}) |
							(((11'd1) <<  9) & {11{opOut == 13}}) |
							(((11'd1) << 10) & {11{opOut == 14}});
	
	// A Opcode (OpCtrlA)
	assign control[3:0] = (4'b1000 & {4{opOut == 12}}) | 
						  (4'b0100 & {4{opOut == 10}}) | 
						  (4'b0010 & {4{opOut ==  8}}) |
						  (4'b0001 & {4{(opOut != 12) && (opOut != 10) && (opOut != 8)}}); 
	
	// B Opcode (OpCtrlB)
	assign control[7:4] = (4'b1000 & {4{opOut == 12}}) | 
						  (4'b0100 & {4{opOut == 11}}) | 
						  (4'b0010 & {4{opOut ==  9}}) |
						  (4'b0001 & {4{(opOut != 12) && (opOut != 11) && (opOut != 9)}}); 
	
	
	// control 1000 - clear (0)
	// control 0100 - ans   (busOut)
	// control 0010 - input (busIn)
	// control 0001 - no op (Aout)
	Mux4 amux( {n{1'b0}}, busOut, busIn, Aout, control[7:4],Ain );
	DFF Areg(clk, Ain, Aout);
	
	// control 1000 - clear (0)
	// control 0100 - ans   (busOut)
	// control 0010 - input (busIn)
	// control 0001 - no op (Bout)
	Mux4 bmux( {n{1'b0}}, busOut, busIn, Bout, control[3:0] ,Bin );
	DFF Breg(clk, Bin, Bout);
	
	// ALU MODULES (+,-,x,&,|,^,~)
	Adder      addM(Aout, Bout, addW, errBuffer[2]);
	Subtractor subM(Aout, Bout, subW, errBuffer[1]);
	Multiplier mltM(Aout, Bout, mltW, errBuffer[0]);
	And 	   andM(Aout, Bout, andW);
	Or 	   	   orM (Aout, Bout, orW);
	Xor 	   xorM(Aout, Bout, xorW);
	Not		   notM(Aout, notW);
	
	
	// Only output error flags when the operation that causes the error is selected
	assign error[2] = errBuffer[2] & {11{(opOut == 1)}};
	assign error[1] = errBuffer[1] & {11{(opOut == 2)}};
	assign error[0] = errBuffer[0] & {11{(opOut == 3)}};
	
	
	//AnsMux    Input      // Control Code
	Mux16 ansMux({n{1'b0}}, // 15- no op
				 {n{1'b0}}, // 14- no op
				 {n{1'b0}}, // 13- no op
				 {n{1'b0}}, // 12- no op
				 {n{1'b0}}, // 11- no op
				 Bout,      // 10- B
				 Aout,      // 9 - A
				 notW,      // 8 - bitwise NOT A
				 xorW,      // 7 - bitwise XOR
				 orW,       // 6 - bitwise OR
				 andW,      // 5 - bitwise AND
				 mltW,      // 4 - multiply
				 subW,      // 3 - sub
				 addW, 	   // 2 - add
				 {n{1'b0}}, // 1 - clear
				 busOut,    // 0 - no op
				
				 {5'b0,control[18:8]}, 
				 ansIn) ;
	DFF ans(clk, ansIn, busOut);
	
	
	// Magnitude Flags
	// Flags: {negative, A>B, A=B, A<B}
	MagComp comparator(Aout, Bout, flags[2:0]);
	assign flags[3] = busOut[n-1];
	

endmodule



// Datapath Modules
module Encoder16x4( in, out);
	parameter n = 16;
	input [n-1:0] in;
	output [3:0] out;
	
	assign out = ({4{(16'h0001 == in)}} & 0)  | // Default state if more than one / none in input is hot
				 ({4{(16'h0002 == in)}} & 1)  | 
				 ({4{(16'h0004 == in)}} & 2)  |
				 ({4{(16'h0008 == in)}} & 3)  |
				 ({4{(16'h0010 == in)}} & 4)  |
				 ({4{(16'h0020 == in)}} & 5)  |
				 ({4{(16'h0040 == in)}} & 6)  |
				 ({4{(16'h0080 == in)}} & 7)  |
				 ({4{(16'h0100 == in)}} & 8)  |
				 ({4{(16'h0200 == in)}} & 9)  |
				 ({4{(16'h0400 == in)}} & 10) |
				 ({4{(16'h0800 == in)}} & 11) |
				 ({4{(16'h1000 == in)}} & 12) |
				 ({4{(16'h2000 == in)}} & 13) |
				 ({4{(16'h4000 == in)}} & 14) |
				 ({4{(16'h8000 == in)}} & 15);
endmodule

module Mux16(aF, aE, aD, aC, aB, aA, a9, a8, a7, a6, a5, a4, a3, a2, a1, a0, s, b) ;
  parameter n = 16 ;
  input [n-1:0] aF, aE, aD, aC, aB, aA, a9, a8, a7, a6, a5, a4, a3, a2, a1, a0;  // inputs
  input [15:0]   s ; // one-hot select
  output[n-1:0] b ;
  assign b = ({n{s[15]}} & aF) |
             ({n{s[14]}} & aE) | 
             ({n{s[13]}} & aD) |
             ({n{s[12]}} & aC) | 
             ({n{s[11]}} & aB) | 
             ({n{s[10]}} & aA) | 
             ({n{s[9]}} & a9) |
             ({n{s[8]}} & a8) | 
             ({n{s[7]}} & a7) |
             ({n{s[6]}} & a6) | 
             ({n{s[5]}} & a5) |
             ({n{s[4]}} & a4) | 
             ({n{s[3]}} & a3) |
             ({n{s[2]}} & a2) | 
             ({n{s[1]}} & a1) |
             ({n{s[0]}} & a0) ;
endmodule // Mux16

module Mux4(a3, a2, a1, a0, s, b) ;
  parameter n = 16 ;
  input [n-1:0] a3, a2, a1, a0 ;  // inputs
  input [3:0]   s ; // one-hot select
  output[n-1:0] b ;
  assign b = ({n{s[3]}} & a3) | 
             ({n{s[2]}} & a2) | 
             ({n{s[1]}} & a1) |
             ({n{s[0]}} & a0) ;
endmodule // Mux4 


module Mux2(a1, a0, s, b) ;
  parameter n = 16 ;
  input [n-1:0] a1, a0 ;  // inputs
  input s ; // 0 selects a0, 1 selects a1
  output[n-1:0] b ;
  assign b = ({n{ s}} & a1) | 
             ({n{~s}} & a0);
endmodule // Mux2


module DFF(clk,in,out);
  parameter n = 16 ;//width
  input clk;
  input [n-1:0] in;
  output [n-1:0] out;
  reg [n-1:0] out;
  
  always @(posedge clk)
	out = in;
endmodule
// End of datapath modules




// ALU MODULES (+,-,x,&,|,^,~)

// adder +
module Adder(a, b, out, err) ; 
	input [15:0] a;
	input [15:0] b;

	output [15:0] out;
	output err;
	
	assign out = a + b;
	assign err = out > 32768; //OVERFLOW
endmodule

//subtractor -
module Subtractor(a, b, out, err) ;
	input [15:0] a;
	input [15:0] b;

	output [15:0] out;
	output err;
	
	assign out = a - b;
	assign err = out < -32768; //UNDERFLOW
endmodule

//multiplier x
module Multiplier(a, b, product, error);
	parameter n = 16;
	input [n-1:0] a, b;
	output error;
	output [n-1:0] product;
	wire [n-1:0] sum1, sum2, sum3, sum4, sum5, sum6, sum7, sum8, sum9, sumA,  sumB, sumC, sumD, sumE, sumF, sumG;
	wire err[n-1:0];
	Adder #(n) add0(   a                  & {n{b[ 0]}}, 16'b0, sum1, err[0]);
	Adder #(n) add1( {{a[n-2:0], { 1'b0}} & {n{b[ 1]}}}, sum1, sum2, err[1]);
	Adder #(n) add2( {{a[n-3:0], { 2'b0}} & {n{b[ 2]}}}, sum2, sum3, err[2]);
	Adder #(n) add3( {{a[n-4:0], { 3'b0}} & {n{b[ 3]}}}, sum3, sum4, err[3]);
	Adder #(n) add4( {{a[n-5:0], { 4'b0}} & {n{b[ 4]}}}, sum4, sum5, err[4]);
	Adder #(n) add5( {{a[n-6:0], { 5'b0}} & {n{b[ 5]}}}, sum5, sum6, err[5]);
	Adder #(n) add6( {{a[n-7:0], { 6'b0}} & {n{b[ 6]}}}, sum6, sum7, err[6]);
	Adder #(n) add7( {{a[n-8:0], { 7'b0}} & {n{b[ 7]}}}, sum7, sum8, err[7]);
	Adder #(n) add8( {{a[n-9:0], { 8'b0}} & {n{b[ 8]}}}, sum8, sum9, err[8]);
	Adder #(n) add9( {{a[n-10:0],{ 9'b0}} & {n{b[ 9]}}}, sum9, sumA, err[9]);
	Adder #(n) addA( {{a[n-11:0],{10'b0}} & {n{b[10]}}}, sumA, sumB, err[10]);
	Adder #(n) addB( {{a[n-12:0],{11'b0}} & {n{b[11]}}}, sumB, sumC, err[11]);
	Adder #(n) addC( {{a[n-13:0],{12'b0}} & {n{b[12]}}}, sumC, sumD, err[12]);
	Adder #(n) addD( {{a[n-14:0],{13'b0}} & {n{b[13]}}}, sumD, sumE, err[13]);
	Adder #(n) addE( {{a[n-15:0],{14'b0}} & {n{b[14]}}}, sumE, sumF, err[14]);
	Adder #(n) addF( {{a[n-16:0],{15'b0}} & {n{b[15]}}}, sumF, sumG, err[15]);
	assign error = err[0] |err[1] |err[2] |err[3] |err[4] |err[5] |err[6] |err[7] |err[8] |err[9] |err[10] |err[11] |err[12] |err[13] |err[14] |err[15];
	assign product = sumG[n-1:0];
endmodule

// and &
module And(A, B, out);
	parameter n = 16;
	input [n-1:0] A, B;
	output [n-1:0] out;
	
	assign out = (A&B);
endmodule

// or |
module Or(A, B, out);
	parameter n = 16;
	input [n-1:0] A, B;
	output [n-1:0] out;
	
	assign out = (A|B);
endmodule

/* xor ^
Applies the bitwise XOR operation on two sixteen bit inputs. Inputs are 16bit a
and 16bit b. Outputs are 16bit c and 1bit error. The value of c[i] is a[i] XOR b[i],
that is, c[i]=1 if a[i]=1 or b[i]=1, but c[i]=0 if both a[i] and b[i] equal 0 or 1.
Error is not used in this case. 
*/
module Xor(a,b,c) ;
	parameter n=16 ;

	input  [n-1:0] a ;
	input  [n-1:0] b ;
	output  [n-1:0] c ;
	
	assign c = a ^ b ;
	
endmodule

module Not(a, out);
	parameter n=16 ;
	input [n-1:0] a;
	output [n-1:0] out;
	assign out = ~a;
endmodule


/* Magnitude Comparator -,>,=,<
This module determines which of two 16bit inputs have the greater numerical value. The
inputs are 16bit a and 16bit b. The output is 3bit c. The value of c is encoded as such:
	100: a>b 
	010: a=b 
	001: a<b 
*/
module MagComp(a,b,c) ;
	parameter n=16 ;
	
	input [n-1:0] a ;
	input [n-1:0] b ;
	output [2:0] c ;
	
	wire [n-1:0] eq = a ~^ b ;
	wire [n-1:0] gt = a & ~b ;
	wire [n:0] gtb ={((eq[n-1:0]&gtb[n-1:0])|gt[n-1:0]),1'b0} ;
	
	assign c[2] = gtb[n] & ~equals ;
	assign c[1] = equals ;
	assign c[0] = ~gtb[n] & ~equals ;
	
	
	wire equals = eq[0]&eq[1]&eq[2]&eq[3]&eq[4]&eq[5]&eq[6]&eq[7]&eq[8]&eq[9]&eq[10]&eq[11]&eq[12]&eq[13]&eq[14]&eq[15] ;
	

endmodule



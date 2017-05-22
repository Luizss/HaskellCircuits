#include "systemc.h"
#include "__fork2__.h"
#include "const5.h"
#include "g.h"
#include "mul.h"


SC_MODULE(mainFunc) {
sc_fifo_in<int> x;

sc_fifo_out<int> out;

sc_fifo<int> __fork2__1_out2__g1_y;
sc_fifo<int> const51_out__mul2_in2;
sc_fifo<int> __fork2__1_out1__mul2_in1;
sc_fifo<int> main1_x____fork2__1_in;
sc_fifo<int> mul1_out__main1_out;

sc_fifo<int> __fifo__2;
__fork2__ __fork2__1;
sc_fifo<int> __fifo__1;
const5 const51;
g g1;
mul mul2;
mul mul1;


SC_CTOR(mainFunc) : __fork2__1("__fork2__1"), const51("const51"), g1("g1"), mul2("mul2"), mul1("mul1") {

__fork2__1.out2(__fork2__1_out2__g1_y);
g1.y(__fork2__1_out2__g1_y);

mul1.in2(__fifo__2);
g1.out(__fifo__2);
const51.out(const51_out__mul2_in2);
mul2.in2(const51_out__mul2_in2);

__fork2__1.out1(__fork2__1_out1__mul2_in1);
mul2.in1(__fork2__1_out1__mul2_in1);

__fork2__1.in(x);
mul1.in1(__fifo__1);
mul2.out(__fifo__1);
mul1.out(out);


}
};


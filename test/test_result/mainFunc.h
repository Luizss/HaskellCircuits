#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "__fork2__.h"
#include "const5.h"
#include "g.h"
#include "mul.h"
#include "add.h"


SC_MODULE(mainFunc) {
sc_fifo_in<int> x;
sc_fifo_in<int> y;

sc_fifo_out<int> out;

sc_fifo<int> main1_y__add1_in2;
sc_fifo<int> __fork2__1_out2__g1_y;
sc_fifo<int> const51_out__mul2_in2;
sc_fifo<int> __fork2__1_out1__mul2_in1;
sc_fifo<int> main1_x____fork2__1_in;
sc_fifo<int> add1_out__main1_out;

sc_fifo<int> __fifo__3;
__fork2__ __fork2__1;
sc_fifo<int> __fifo__2;
sc_fifo<int> __fifo__1;
const5 const51;
g g1;
mul mul2;
mul mul1;
add add1;


SC_CTOR(mainFunc) : __fork2__1("__fork2__1"), const51("const51"), g1("g1"), mul2("mul2"), mul1("mul1"), add1("add1") {

add1.in2(y);
__fork2__1.out2(__fork2__1_out2__g1_y);
g1.y(__fork2__1_out2__g1_y);

mul1.in2(__fifo__3);
g1.out(__fifo__3);
const51.out(const51_out__mul2_in2);
mul2.in2(const51_out__mul2_in2);

__fork2__1.out1(__fork2__1_out1__mul2_in1);
mul2.in1(__fork2__1_out1__mul2_in1);

__fork2__1.in(x);
mul1.in1(__fifo__2);
mul2.out(__fifo__2);
add1.in1(__fifo__1);
mul1.out(__fifo__1);
add1.out(out);


}
};

#endif

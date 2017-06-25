#ifndef F_H_
#define F_H_

#include "systemc.h"
#include "__fork2__.h"
#include "mul1_.h"


SC_MODULE(f) {
sc_fifo_in<sc_lv<31> > x;

sc_fifo_out<sc_lv<31> > out;

sc_fifo<sc_lv<31> > __fork2__1_out2__mul1_in2;
sc_fifo<sc_lv<31> > __fork2__1_out1__mul1_in1;

__fork2__ __fork2__1;
mul1_ mul1;



SC_CTOR(f) : __fork2__1("__fork2__1"), mul1("mul1") {

__fork2__1.out2(__fork2__1_out2__mul1_in2);
mul1.in2(__fork2__1_out2__mul1_in2);

__fork2__1.out1(__fork2__1_out1__mul1_in1);
mul1.in1(__fork2__1_out1__mul1_in1);

__fork2__1.in(x);
mul1.out(out);


}
};

#endif

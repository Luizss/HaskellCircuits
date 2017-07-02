#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "add1_.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > __i0;
sc_fifo_in<sc_lv<32> > __i1;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > main1___i1__add1_in2;
sc_fifo<sc_lv<32> > main1___i0__add1_in1;
sc_fifo<sc_lv<32> > add1_out__main1_out;

add1_ add1;



SC_CTOR(mainFunc) : add1("add1") {

add1.in2(__i1);
add1.in1(__i0);
add1.out(out);


}
};

#endif

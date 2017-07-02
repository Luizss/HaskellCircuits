#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "equ3_.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > __i0;
sc_fifo_in<sc_lv<32> > __i1;

sc_fifo_out<sc_lv<1> > out;

sc_fifo<sc_lv<32> > main1___i1__equ1_in2;
sc_fifo<sc_lv<32> > main1___i0__equ1_in1;
sc_fifo<sc_lv<1> > equ1_out__main1_out;

equ3_ equ1;



SC_CTOR(mainFunc) : equ1("equ1") {

equ1.in2(__i1);
equ1.in1(__i0);
equ1.out(out);


}
};

#endif

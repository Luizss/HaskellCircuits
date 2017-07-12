#ifndef WINDOW_H_
#define WINDOW_H_

#include "systemc.h"
#include "__fork2__.h"
#include "take.h"
#include "reverse.h"


SC_MODULE(window) {
sc_fifo_in<sc_lv<31> > n;
sc_fifo_in<sc_lv<32> > s;
sc_fifo_in<sc_lv<32> > a;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > __fork2__1_out2__reverse1_a;
sc_fifo<sc_lv<32> > __fork2__1_out1__take1_a;

__fork2__ __fork2__1;
sc_fifo<sc_lv<32> > __fifo__1;
take take1;
reverse reverse1;



SC_CTOR(window) : __fork2__1("__fork2__1"), take1("take1"), reverse1("reverse1") {

__fork2__1.out2(__fork2__1_out2__reverse1_a);
reverse1.a(__fork2__1_out2__reverse1_a);

__fork2__1.out1(__fork2__1_out1__take1_a);
take1.a(__fork2__1_out1__take1_a);

__fork2__1.in(a);
take1.s(s);
take1.n(n);
reverse1.s(__fifo__1);
take1.out(__fifo__1);
reverse1.out(out);


}
};

#endif

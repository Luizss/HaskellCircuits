#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "h__0g.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > s;
sc_fifo_in<sc_lv<32> > a;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > main1_a__h__0g1_a;
sc_fifo<sc_lv<32> > main1_s__h__0g1_s;
sc_fifo<sc_lv<32> > h__0g1_out__main1_out;

h__0g h__0g1;



SC_CTOR(mainFunc) : h__0g1("h__0g1") {

h__0g1.a(a);
h__0g1.s(s);
h__0g1.out(out);


}
};

#endif

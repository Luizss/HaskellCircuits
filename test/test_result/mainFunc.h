#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "not_.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > x;

sc_fifo_out<sc_lv<8> > out;

sc_fifo<sc_lv<32> > main1_x__not_1_in1;
sc_fifo<sc_lv<8> > not_1_out__main1_out;

not_ not_1;


SC_CTOR(mainFunc) : not_1("not_1") {

not_1.in1(x);
not_1.out(out);


}
};

#endif

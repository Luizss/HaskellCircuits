#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "const_dec_01_.h"
#include "sum.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > s;

sc_fifo_out<sc_lv<31> > out;

sc_fifo<sc_lv<31> > const_dec_01_out__sum1_a;
sc_fifo<sc_lv<32> > main1_s__sum1_s;
sc_fifo<sc_lv<31> > sum1_out__main1_out;

const_dec_01_ const_dec_01;
sum sum1;



SC_CTOR(mainFunc) : const_dec_01("const_dec_01"), sum1("sum1") {

const_dec_01.out(const_dec_01_out__sum1_a);
sum1.a(const_dec_01_out__sum1_a);

sum1.s(s);
sum1.out(out);


}
};

#endif

#ifndef SUM_H_
#define SUM_H_

#include "systemc.h"
#include "const_dec_03_.h"
#include "sum_.h"


SC_MODULE(sum) {
sc_fifo_in<sc_lv<32> > s;

sc_fifo_out<sc_lv<31> > out;

sc_fifo<sc_lv<31> > const_dec_01_out__sum_1_a;

const_dec_03_ const_dec_01;
sum_ sum_1;



SC_CTOR(sum) : const_dec_01("const_dec_01"), sum_1("sum_1") {

const_dec_01.out(const_dec_01_out__sum_1_a);
sum_1.a(const_dec_01_out__sum_1_a);

sum_1.s(s);
sum_1.out(out);


}
};

#endif

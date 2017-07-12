#ifndef SUM_H_
#define SUM_H_

#include "systemc.h"
#include "const_dec_02_.h"
#include "sum__left.h"


SC_MODULE(sum) {
sc_fifo_in<sc_lv<33> > __i0;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_dec_01_out__sum__left1___acc;

const_dec_02_ const_dec_01;
sum__left sum__left1;



SC_CTOR(sum) : const_dec_01("const_dec_01"), sum__left1("sum__left1") {

const_dec_01.out(const_dec_01_out__sum__left1___acc);
sum__left1.__acc(const_dec_01_out__sum__left1___acc);

sum__left1.__i0(__i0);
sum__left1.out(out);


}
};

#endif

#ifndef FAC_H_
#define FAC_H_

#include "systemc.h"
#include "const_dec_11_.h"
#include "fac__left.h"


SC_MODULE(fac) {
sc_fifo_in<sc_lv<32> > __i0;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_dec_11_out__fac__left1___acc;

const_dec_11_ const_dec_11;
fac__left fac__left1;



SC_CTOR(fac) : const_dec_11("const_dec_11"), fac__left1("fac__left1") {

const_dec_11.out(const_dec_11_out__fac__left1___acc);
fac__left1.__acc(const_dec_11_out__fac__left1___acc);

fac__left1.__i0(__i0);
fac__left1.out(out);


}
};

#endif

#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "const_hex_1.h"
#include "fac.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > n;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_hex_11_out__fac1_a;
sc_fifo<sc_lv<32> > main1_n__fac1_n;
sc_fifo<sc_lv<32> > fac1_out__main1_out;

const_hex_1 const_hex_11;
fac fac1;



SC_CTOR(mainFunc) : const_hex_11("const_hex_11"), fac1("fac1") {

const_hex_11.out(const_hex_11_out__fac1_a);
fac1.a(const_hex_11_out__fac1_a);

fac1.n(n);
fac1.out(out);


}
};

#endif

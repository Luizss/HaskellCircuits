#ifndef G_H_
#define G_H_

#include "systemc.h"
#include "const_dec_21_.h"
#include "mul1_.h"


SC_MODULE(g) {
sc_fifo_in<sc_lv<32> > __i0;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_dec_21_out__mul1_in1;

const_dec_21_ const_dec_21;
mul1_ mul1;



SC_CTOR(g) : const_dec_21("const_dec_21"), mul1("mul1") {

mul1.in2(__i0);
const_dec_21.out(const_dec_21_out__mul1_in1);
mul1.in1(const_dec_21_out__mul1_in1);

mul1.out(out);


}
};

#endif

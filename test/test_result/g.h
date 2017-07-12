#ifndef G_H_
#define G_H_

#include "systemc.h"
#include "mul1_.h"


SC_MODULE(g) {
sc_fifo_in<sc_lv<32> > __i0;
sc_fifo_in<sc_lv<32> > __i1;

sc_fifo_out<sc_lv<32> > out;


mul1_ mul1;



SC_CTOR(g) : mul1("mul1") {

mul1.in2(__i1);
mul1.in1(__i0);
mul1.out(out);


}
};

#endif

#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "fac.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > __i0;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > main1___i0__fac1___i0;
sc_fifo<sc_lv<32> > fac1_out__main1_out;

fac fac1;



SC_CTOR(mainFunc) : fac1("fac1") {

fac1.__i0(__i0);
fac1.out(out);


}
};

#endif

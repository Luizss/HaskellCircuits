#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "const_dec_31_.h"
#include "take.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<33> > __i0;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<32> > const_dec_31_out__take1___i1;
sc_fifo<sc_lv<33> > main1___i0__take1___i0;
sc_fifo<sc_lv<33> > take1_out__main1_out;

const_dec_31_ const_dec_31;
take take1;



SC_CTOR(mainFunc) : const_dec_31("const_dec_31"), take1("take1") {

const_dec_31.out(const_dec_31_out__take1___i1);
take1.__i1(const_dec_31_out__take1___i1);

take1.__i0(__i0);
take1.out(out);


}
};

#endif

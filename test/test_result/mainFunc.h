#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "zipWith.h"
#include "sum.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<33> > __i0;
sc_fifo_in<sc_lv<33> > __i1;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<33> > main1___i1__zipWith1___i1;
sc_fifo<sc_lv<33> > main1___i0__zipWith1___i0;
sc_fifo<sc_lv<32> > sum1_out__main1_out;

sc_fifo<sc_lv<33> > __fifo__1;
zipWith zipWith1;
sum sum1;



SC_CTOR(mainFunc) : zipWith1("zipWith1"), sum1("sum1") {

zipWith1.__i1(__i1);
zipWith1.__i0(__i0);
sum1.__i0(__fifo__1);
zipWith1.out(__fifo__1);
sum1.out(out);


}
};

#endif

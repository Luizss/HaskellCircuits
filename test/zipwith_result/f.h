#ifndef F_H_
#define F_H_

#include "systemc.h"
#include "mul1_.h"


SC_MODULE(f) {
sc_fifo_in<sc_lv<31> > x;
sc_fifo_in<sc_lv<31> > y;

sc_fifo_out<sc_lv<31> > out;


mul1_ mul1;



SC_CTOR(f) : mul1("mul1") {

mul1.in2(y);
mul1.in1(x);
mul1.out(out);


}
};

#endif

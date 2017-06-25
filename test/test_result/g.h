#ifndef G_H_
#define G_H_

#include "systemc.h"
#include "add1_.h"


SC_MODULE(g) {
sc_fifo_in<sc_lv<32> > x;
sc_fifo_in<sc_lv<32> > y;
sc_fifo_in<sc_lv<32> > z;

sc_fifo_out<sc_lv<32> > out;


sc_fifo<sc_lv<32> > __fifo__1;
add1_ add2;
add1_ add1;



SC_CTOR(g) : add2("add2"), add1("add1") {

add1.in2(z);
add2.in2(y);
add2.in1(x);
add1.in1(__fifo__1);
add2.out(__fifo__1);
add1.out(out);


}
};

#endif

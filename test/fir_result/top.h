#include "systemc.h"
#include "testbench.h"
#include "mainFunc.h"

SC_MODULE(top) {

mainFunc m;
testbench tb;
sc_fifo<sc_lv<32> > bs;
sc_fifo<sc_lv<32> > xs;
sc_fifo<sc_lv<32> > a;
sc_fifo<sc_lv<32> > c;
sc_fifo<sc_lv<32> > out;


SC_CTOR(top) : m("m"), tb("tb") {
m.bs(bs); tb.bs(bs);
m.xs(xs); tb.xs(xs);
m.a(a); tb.a(a);
m.c(c); tb.c(c);
m.out(out); tb.out(out);


}
};
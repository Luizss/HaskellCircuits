#include "systemc.h"
#include "testbench.h"
#include "mainFunc.h"

SC_MODULE(top) {

mainFunc m;
testbench tb;
sc_fifo<sc_lv<32> > s;
sc_fifo<sc_lv<32> > l;
sc_fifo<sc_lv<32> > out;


SC_CTOR(top) : m("m"), tb("tb") {
m.s(s); tb.s(s);
m.l(l); tb.l(l);
m.out(out); tb.out(out);


}
};
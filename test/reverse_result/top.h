#include "systemc.h"
#include "testbench.h"
#include "mainFunc.h"

SC_MODULE(top) {

mainFunc m;
testbench tb;
sc_fifo<sc_lv<32> > s;
sc_fifo<sc_lv<32> > a;
sc_fifo<sc_lv<32> > b;
sc_fifo<sc_lv<32> > out;


SC_CTOR(top) : m("m"), tb("tb") {
m.s(s); tb.s(s);
m.a(a); tb.a(a);
m.b(b); tb.b(b);
m.out(out); tb.out(out);


}
};
#include "systemc.h"
#include "testbench.h"
#include "mainFunc.h"

SC_MODULE(top) {

mainFunc m;
testbench tb;
sc_fifo<sc_lv<32> > n;
sc_fifo<sc_lv<32> > k;
sc_fifo<sc_lv<32> > s;
sc_fifo<sc_lv<32> > a;
sc_fifo<sc_lv<32> > out;


SC_CTOR(top) : m("m"), tb("tb") {
m.n(n); tb.n(n);
m.k(k); tb.k(k);
m.s(s); tb.s(s);
m.a(a); tb.a(a);
m.out(out); tb.out(out);


}
};
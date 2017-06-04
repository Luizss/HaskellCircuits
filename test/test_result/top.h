#include "systemc.h"
#include "testbench.h"
#include "mainFunc.h"

SC_MODULE(top) {

mainFunc m;
testbench tb;
sc_fifo<sc_lv<32> > x;
sc_fifo<sc_lv<8> > out;


SC_CTOR(top) : m("m"), tb("tb") {
m.x(x); tb.x(x);
m.out(out); tb.out(out);


}
};
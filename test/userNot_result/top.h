#include "systemc.h"
#include "testbench.h"
#include "mainFunc.h"

SC_MODULE(top) {

mainFunc m;
testbench tb;
sc_fifo<sc_lv<1> > __i0;
sc_fifo<sc_lv<1> > out;


SC_CTOR(top) : m("m"), tb("tb") {
m.__i0(__i0); tb.__i0(__i0);
m.out(out); tb.out(out);


}
};
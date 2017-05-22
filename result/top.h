#include "systemc.h"
#include "testbench.h"
#include "mainFunc.h"

SC_MODULE(top) {

mainFunc m;
testbench tb;
sc_fifo<int> x;
sc_fifo<int> out;


SC_CTOR(top) : m("m"), tb("tb") {
m.x(x); tb.x(x);
m.out(out); tb.out(out);


}
};
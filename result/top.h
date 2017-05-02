#include "systemc.h"
#include "testbench.h"
#include "mainFunc.h"

SC_MODULE(top) {

mainFunc m;
testbench tb;

sc_fifo<int> in_0;
sc_fifo<int> out_0;

SC_CTOR(top) : m("m"), tb("tb") {

m.in_0(in_0); tb.in_0(in_0);
m.out_0(out_0); tb.out_0(out_0);

}
};
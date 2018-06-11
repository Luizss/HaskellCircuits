#ifndef AND_1__H_
#define AND_1__H_
#include "systemc.h"
SC_MODULE(and_1_) {
sc_fifo_in<sc_lv<1> > in1;
sc_fifo_in<sc_lv<1> > in2;
sc_fifo_out<sc_lv<1> > out;


void proc();
SC_CTOR(and_1_) {
SC_THREAD(proc);
}
};

void and_1_::proc() {
while(true) {
out.write(in1.read()&in2.read());
}
}
#endif
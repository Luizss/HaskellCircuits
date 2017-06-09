#ifndef AND__H_
#define AND__H_
#include "systemc.h"
SC_MODULE(and_) {
sc_fifo_in<sc_lv<32> > in1;
sc_fifo_in<sc_lv<32> > in2;
sc_fifo_out<sc_lv<32> > out;


void proc();
SC_CTOR(and_) {
SC_THREAD(proc);
}
};

void and_::proc() {
while(true) {
out.write(in1.read()&in2.read());
}
}
#endif
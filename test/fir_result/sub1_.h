#ifndef SUB1__H_
#define SUB1__H_
#include "systemc.h"
SC_MODULE(sub1_) {
sc_fifo_in<sc_lv<31> > in1;
sc_fifo_in<sc_lv<31> > in2;
sc_fifo_out<sc_lv<31> > out;


void proc();
SC_CTOR(sub1_) {
SC_THREAD(proc);
}
};

void sub1_::proc() {
while(true) {
out.write((sc_uint<31>)in1.read()-(sc_uint<31>)in2.read());
}
}
#endif
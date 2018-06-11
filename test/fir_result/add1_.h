#ifndef ADD1__H_
#define ADD1__H_
#include "systemc.h"
SC_MODULE(add1_) {
sc_fifo_in<sc_lv<32> > in1;
sc_fifo_in<sc_lv<32> > in2;
sc_fifo_out<sc_lv<32> > out;


void proc();
SC_CTOR(add1_) {
SC_THREAD(proc);
}
};

void add1_::proc() {
while(true) {
out.write((sc_uint<32>)in1.read()+(sc_uint<32>)in2.read());
}
}
#endif
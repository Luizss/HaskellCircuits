#ifndef NOT__H_
#define NOT__H_
#include "systemc.h"
SC_MODULE(not_) {
sc_fifo_in<sc_lv<32> > in1;
sc_fifo_out<sc_lv<32> > out;


void proc();
SC_CTOR(not_) {
SC_THREAD(proc);
}
};

void not_::proc() {
while(true) {
out.write(~in1.read());
}
}
#endif
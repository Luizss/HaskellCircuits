#ifndef NOT_1__H_
#define NOT_1__H_
#include "systemc.h"
SC_MODULE(not_1_) {
sc_fifo_in<sc_lv<1> > in1;
sc_fifo_out<sc_lv<1> > out;


void proc();
SC_CTOR(not_1_) {
SC_THREAD(proc);
}
};

void not_1_::proc() {
while(true) {
out.write(~in1.read());
}
}
#endif
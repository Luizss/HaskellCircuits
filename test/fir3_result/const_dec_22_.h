#ifndef CONST_DEC_22__H_
#define CONST_DEC_22__H_
#include "systemc.h"
SC_MODULE(const_dec_22_) {
sc_fifo_out<sc_lv<32> > out;
void proc();
SC_CTOR(const_dec_22_) {
SC_THREAD(proc);
}
};

void const_dec_22_::proc() {
while(true) {
out.write(2);
}
}
#endif
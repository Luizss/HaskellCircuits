#ifndef CONST_DEC_32__H_
#define CONST_DEC_32__H_
#include "systemc.h"
SC_MODULE(const_dec_32_) {
sc_fifo_out<sc_lv<32> > out;
void proc();
SC_CTOR(const_dec_32_) {
SC_THREAD(proc);
}
};

void const_dec_32_::proc() {
while(true) {
out.write(3);
}
}
#endif
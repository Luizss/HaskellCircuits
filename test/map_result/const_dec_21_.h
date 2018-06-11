#ifndef CONST_DEC_21__H_
#define CONST_DEC_21__H_
#include "systemc.h"
SC_MODULE(const_dec_21_) {
sc_fifo_out<sc_lv<32> > out;
void proc();
SC_CTOR(const_dec_21_) {
SC_THREAD(proc);
}
};

void const_dec_21_::proc() {
while(true) {
out.write(2);
}
}
#endif
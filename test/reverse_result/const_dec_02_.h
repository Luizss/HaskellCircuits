#ifndef CONST_DEC_02__H_
#define CONST_DEC_02__H_
#include "systemc.h"
SC_MODULE(const_dec_02_) {
sc_fifo_out<sc_lv<1> > out;
void proc();
SC_CTOR(const_dec_02_) {
SC_THREAD(proc);
}
};

void const_dec_02_::proc() {
while(true) {
out.write(0);
}
}
#endif
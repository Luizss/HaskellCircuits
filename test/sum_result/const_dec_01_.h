#ifndef CONST_DEC_01__H_
#define CONST_DEC_01__H_
#include "systemc.h"
SC_MODULE(const_dec_01_) {
sc_fifo_out<sc_lv<31> > out;
void proc();
SC_CTOR(const_dec_01_) {
SC_THREAD(proc);
}
};

void const_dec_01_::proc() {
while(true) {
out.write(0);
}
}
#endif
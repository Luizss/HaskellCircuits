#ifndef CONST_DEC_03__H_
#define CONST_DEC_03__H_
#include "systemc.h"
SC_MODULE(const_dec_03_) {
sc_fifo_out<sc_lv<31> > out;
void proc();
SC_CTOR(const_dec_03_) {
SC_THREAD(proc);
}
};

void const_dec_03_::proc() {
while(true) {
out.write(0);
}
}
#endif
#ifndef CONST_DEC_0_H_
#define CONST_DEC_0_H_
#include "systemc.h"
SC_MODULE(const_dec_0) {
sc_fifo_out<sc_lv<32> > out;
void proc();
SC_CTOR(const_dec_0) {
SC_THREAD(proc);
}
};

void const_dec_0::proc() {
while(true) {
out.write(0);
}
}
#endif
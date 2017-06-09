#ifndef CONST_DEC_1_H_
#define CONST_DEC_1_H_
#include "systemc.h"
SC_MODULE(const_dec_1) {
sc_fifo_out<sc_lv<1> > out;
void proc();
SC_CTOR(const_dec_1) {
SC_THREAD(proc);
}
};

void const_dec_1::proc() {
while(true) {
out.write(1);
}
}
#endif
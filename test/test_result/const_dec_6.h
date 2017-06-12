#ifndef CONST_DEC_6_H_
#define CONST_DEC_6_H_
#include "systemc.h"
SC_MODULE(const_dec_6) {
sc_fifo_out<sc_lv<32> > out;
void proc();
SC_CTOR(const_dec_6) {
SC_THREAD(proc);
}
};

void const_dec_6::proc() {
while(true) {
out.write(6);
}
}
#endif
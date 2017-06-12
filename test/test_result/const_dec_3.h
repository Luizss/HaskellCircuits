#ifndef CONST_DEC_3_H_
#define CONST_DEC_3_H_
#include "systemc.h"
SC_MODULE(const_dec_3) {
sc_fifo_out<sc_lv<32> > out;
void proc();
SC_CTOR(const_dec_3) {
SC_THREAD(proc);
}
};

void const_dec_3::proc() {
while(true) {
out.write(3);
}
}
#endif
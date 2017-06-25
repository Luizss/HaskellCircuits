#ifndef CONST_DEC_13__H_
#define CONST_DEC_13__H_
#include "systemc.h"
SC_MODULE(const_dec_13_) {
sc_fifo_out<sc_lv<31> > out;
void proc();
SC_CTOR(const_dec_13_) {
SC_THREAD(proc);
}
};

void const_dec_13_::proc() {
while(true) {
out.write(1);
}
}
#endif
#ifndef CONST_BIN_11__H_
#define CONST_BIN_11__H_
#include "systemc.h"
SC_MODULE(const_bin_11_) {
sc_fifo_out<sc_lv<1> > out;
void proc();
SC_CTOR(const_bin_11_) {
SC_THREAD(proc);
}
};

void const_bin_11_::proc() {
while(true) {
out.write("1");
}
}
#endif
#ifndef CONST_BIN_01__H_
#define CONST_BIN_01__H_
#include "systemc.h"
SC_MODULE(const_bin_01_) {
sc_fifo_out<sc_lv<1> > out;
void proc();
SC_CTOR(const_bin_01_) {
SC_THREAD(proc);
}
};

void const_bin_01_::proc() {
while(true) {
out.write("0");
}
}
#endif
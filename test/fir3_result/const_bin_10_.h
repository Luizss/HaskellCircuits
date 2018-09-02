#ifndef CONST_BIN_10__H_
#define CONST_BIN_10__H_
#include "systemc.h"
SC_MODULE(const_bin_10_) {
sc_fifo_out<sc_lv<1> > out;
void proc();
SC_CTOR(const_bin_10_) {
SC_THREAD(proc);
}
};

void const_bin_10_::proc() {
while(true) {
out.write("1");
}
}
#endif
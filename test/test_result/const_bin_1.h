#ifndef CONST_BIN_1_H_
#define CONST_BIN_1_H_
#include "systemc.h"
SC_MODULE(const_bin_1) {
sc_fifo_out<sc_lv<32> > out;
void proc();
SC_CTOR(const_bin_1) {
SC_THREAD(proc);
}
};

void const_bin_1::proc() {
while(true) {
out.write("1");
}
}
#endif
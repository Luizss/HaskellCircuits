#ifndef CONST_HEX_1_H_
#define CONST_HEX_1_H_
#include "systemc.h"
SC_MODULE(const_hex_1) {
sc_fifo_out<sc_lv<32> > out;
void proc();
SC_CTOR(const_hex_1) {
SC_THREAD(proc);
}
};

void const_hex_1::proc() {
while(true) {
out.write("0x1");
}
}
#endif
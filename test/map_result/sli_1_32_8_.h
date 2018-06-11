#ifndef SLI_1_32_8__H_
#define SLI_1_32_8__H_

#include "systemc.h"
SC_MODULE(sli_1_32_8_) {
sc_fifo_in<sc_lv<33> > in1;

sc_fifo_out<sc_lv<32> > out;

void proc();
SC_CTOR(sli_1_32_8_) {
SC_THREAD(proc);
}
};
void sli_1_32_8_::proc() {
while(true) {
out.write(in1.read().range(32, 1));
}
}
#endif

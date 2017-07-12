#ifndef SLI_1_32_21__H_
#define SLI_1_32_21__H_

#include "systemc.h"
SC_MODULE(sli_1_32_21_) {
sc_fifo_in<sc_lv<33> > in1;

sc_fifo_out<sc_lv<32> > out;

void proc();
SC_CTOR(sli_1_32_21_) {
SC_THREAD(proc);
}
};
void sli_1_32_21_::proc() {
while(true) {
out.write(in1.read().range(32, 1));
}
}
#endif

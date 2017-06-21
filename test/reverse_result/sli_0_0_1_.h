#ifndef SLI_0_0_1__H_
#define SLI_0_0_1__H_

#include "systemc.h"
SC_MODULE(sli_0_0_1_) {
sc_fifo_in<sc_lv<32> > in1;

sc_fifo_out<sc_lv<1> > out;

void proc();
SC_CTOR(sli_0_0_1_) {
SC_THREAD(proc);
}
};
void sli_0_0_1_::proc() {
while(true) {
out.write(in1.read().range(0, 0));
}
}
#endif

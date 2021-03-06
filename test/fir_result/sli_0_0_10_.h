#ifndef SLI_0_0_10__H_
#define SLI_0_0_10__H_

#include "systemc.h"
SC_MODULE(sli_0_0_10_) {
sc_fifo_in<sc_lv<33> > in1;

sc_fifo_out<sc_lv<1> > out;

void proc();
SC_CTOR(sli_0_0_10_) {
SC_THREAD(proc);
}
};
void sli_0_0_10_::proc() {
while(true) {
out.write(in1.read().range(0, 0));
}
}
#endif

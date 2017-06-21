#ifndef SLI_1_31_2__H_
#define SLI_1_31_2__H_

#include "systemc.h"
SC_MODULE(sli_1_31_2_) {
sc_fifo_in<sc_lv<32> > in1;

sc_fifo_out<sc_lv<31> > out;

void proc();
SC_CTOR(sli_1_31_2_) {
SC_THREAD(proc);
}
};
void sli_1_31_2_::proc() {
while(true) {
out.write(in1.read().range(31, 1));
}
}
#endif

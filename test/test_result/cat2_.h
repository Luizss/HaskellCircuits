#ifndef CAT2__H_
#define CAT2__H_

#include "systemc.h"
SC_MODULE(cat2_) {
sc_fifo_in<sc_lv<32> > in1;
sc_fifo_in<sc_lv<33> > in2;

sc_fifo_out<sc_lv<65> > out;

void proc();
SC_CTOR(cat2_) {
SC_THREAD(proc);
}
};
void cat2_::proc() {
while(true) {
out.write((in1.read(),in2.read()));
}
}
#endif

#ifndef CAT1__H_
#define CAT1__H_

#include "systemc.h"
SC_MODULE(cat1_) {
sc_fifo_in<sc_lv<31> > in1;
sc_fifo_in<sc_lv<1> > in2;

sc_fifo_out<sc_lv<32> > out;

void proc();
SC_CTOR(cat1_) {
SC_THREAD(proc);
}
};
void cat1_::proc() {
while(true) {
out.write((in1.read(),in2.read()));
}
}
#endif

#ifndef CAT3__H_
#define CAT3__H_

#include "systemc.h"
SC_MODULE(cat3_) {
sc_fifo_in<sc_lv<65> > in1;
sc_fifo_in<sc_lv<1> > in2;

sc_fifo_out<sc_lv<33> > out;

void proc();
SC_CTOR(cat3_) {
SC_THREAD(proc);
}
};
void cat3_::proc() {
while(true) {
out.write((in1.read(),in2.read()));
}
}
#endif

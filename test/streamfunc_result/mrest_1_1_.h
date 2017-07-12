#ifndef MREST_1_1__H_
#define MREST_1_1__H_

#include "systemc.h"
SC_MODULE(mrest_1_1_) {
sc_fifo_in<sc_lv<32> > in1;

sc_fifo_out<sc_lv<32> > out;

sc_lv<32> v;
void proc();
SC_CTOR(mrest_1_1_) {
SC_THREAD(proc);
}
};
void mrest_1_1_::proc() {
while(true) {
in1.read();

while(in1.nb_read(v)) {
out.write(v);
}
}
}
#endif

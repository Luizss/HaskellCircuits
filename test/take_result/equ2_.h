#ifndef EQU2__H_
#define EQU2__H_
#include "systemc.h"
SC_MODULE(equ2_) {
sc_fifo_in<sc_lv<32> > in1;
sc_fifo_in<sc_lv<32> > in2;
sc_fifo_out<sc_lv<1> > out;


void proc();
SC_CTOR(equ2_) {
SC_THREAD(proc);
}
};

void equ2_::proc() {
while(true) {
out.write(in1.read()==in2.read());
}
}
#endif
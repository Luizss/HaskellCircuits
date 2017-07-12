#ifndef EQU1__H_
#define EQU1__H_
#include "systemc.h"
SC_MODULE(equ1_) {
sc_fifo_in<sc_lv<32> > in1;
sc_fifo_in<sc_lv<32> > in2;
sc_fifo_out<sc_lv<1> > out;


void proc();
SC_CTOR(equ1_) {
SC_THREAD(proc);
}
};

void equ1_::proc() {
while(true) {
out.write(in1.read()==in2.read());
}
}
#endif
#ifndef EQU_H_
#define EQU_H_
#include "systemc.h"
SC_MODULE(equ) {
sc_fifo_in<sc_lv<32> > in1;
sc_fifo_in<sc_lv<32> > in2;
sc_fifo_out<sc_lv<1> > out;


void proc();
SC_CTOR(equ) {
SC_THREAD(proc);
}
};

void equ::proc() {
while(true) {
out.write(in1.read()==in2.read());
}
}
#endif
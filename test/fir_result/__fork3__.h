#ifndef __FORK3___H_
#define __FORK3___H_
#include "systemc.h"
SC_MODULE(__fork3__) {
sc_lv<32> in__aux;
sc_fifo_in<sc_lv<32> > in;
sc_fifo_out<sc_lv<32> > out1;
sc_fifo_out<sc_lv<32> > out2;
sc_fifo_out<sc_lv<32> > out3;
void proc();
SC_CTOR(__fork3__) {
SC_THREAD(proc);
}
};

void __fork3__::proc() {
while(true) {
in__aux = in.read();
out1.write(in__aux);
out2.write(in__aux);
out3.write(in__aux);
}
}
#endif
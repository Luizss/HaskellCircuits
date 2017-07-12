#ifndef __FORK2___H_
#define __FORK2___H_
#include "systemc.h"
SC_MODULE(__fork2__) {
sc_lv<31> in__aux;
sc_fifo_in<sc_lv<31> > in;
sc_fifo_out<sc_lv<31> > out1;
sc_fifo_out<sc_lv<31> > out2;
void proc();
SC_CTOR(__fork2__) {
SC_THREAD(proc);
}
};

void __fork2__::proc() {
while(true) {
in__aux = in.read();
out1.write(in__aux);
out2.write(in__aux);
}
}
#endif
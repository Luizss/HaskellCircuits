#ifndef ADD_H_
#define ADD_H_
#include "systemc.h"
SC_MODULE(add) {
sc_fifo_in<int> in1;
sc_fifo_in<int> in2;
sc_fifo_out<int> out;


void proc();
SC_CTOR(add) {
SC_THREAD(proc);
}
};

void add::proc() {
while(true) {
out.write(in1.read()+in2.read());
}
}
#endif
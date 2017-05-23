#ifndef MUL_H_
#define MUL_H_
#include "systemc.h"
SC_MODULE(mul) {
sc_fifo_in<int> in1;
sc_fifo_in<int> in2;
sc_fifo_out<int> out;


void proc();
SC_CTOR(mul) {
SC_THREAD(proc);
}
};

void mul::proc() {
while(true) {
out.write(in1.read()*in2.read());
}
}
#endif
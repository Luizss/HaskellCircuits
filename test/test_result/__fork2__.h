#include "systemc.h"
SC_MODULE(__fork2__) {
int in_aux;
sc_fifo_in<int> in;
sc_fifo_out<int> out1;
sc_fifo_out<int> out2;
void proc();
SC_CTOR(__fork2__) {
SC_THREAD(proc);
}
};

void __fork2__::proc() {
while(true) {
in_aux = in.read();
out1.write(in_aux);
out2.write(in_aux);
}
}
#include "systemc.h"
SC_MODULE(special_mul) {

sc_fifo_in<int> in_0;
sc_fifo_in<int> in_1;
sc_fifo_out<int> out_0;

void proc();
SC_CTOR(special_mul) {
SC_THREAD(proc);
}
};

void special_mul::proc() {
while(true) {
out_0.write(in_0.read()*in_1.read());
}
}
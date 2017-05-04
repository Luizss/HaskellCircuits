#include "systemc.h"
SC_MODULE(constant_5) {
sc_fifo_out<int> out_0;
void proc();
SC_CTOR(constant_5) {
SC_THREAD(proc);
}
};

void constant_5::proc() {
while(true) {
out_0.write(5);
}
}
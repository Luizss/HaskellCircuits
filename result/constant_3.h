#include "systemc.h"
SC_MODULE(constant_3) {
sc_fifo_out<int> out_0;
void proc();
SC_CTOR(constant_3) {
SC_THREAD(proc);
}
};

void constant_3::proc() {
while(true) {
out_0.write(3);
}
}
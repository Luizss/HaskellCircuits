#include "systemc.h"
SC_MODULE(const3) {
sc_fifo_out<int> out;
void proc();
SC_CTOR(const3) {
SC_THREAD(proc);
}
};

void const3::proc() {
while(true) {
out.write(3);
}
}
#include "systemc.h"
SC_MODULE(const5) {
sc_fifo_out<int> out;
void proc();
SC_CTOR(const5) {
SC_THREAD(proc);
}
};

void const5::proc() {
while(true) {
out.write(5);
}
}
#ifndef CONST8_H_
#define CONST8_H_
#include "systemc.h"
SC_MODULE(const8) {
sc_fifo_out<int> out;
void proc();
SC_CTOR(const8) {
SC_THREAD(proc);
}
};

void const8::proc() {
while(true) {
out.write(8);
}
}
#endif
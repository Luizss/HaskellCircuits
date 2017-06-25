#ifndef G_H_
#define G_H_

#include "systemc.h"


SC_MODULE(g) {
sc_fifo_in<sc_lv<32> > s;

sc_fifo_out<sc_lv<32> > out;



sc_lv<32> s__aux;

void proc();
SC_CTOR(g) {


SC_THREAD(proc);
}
};
void g::proc() {
while(true) {
s__aux = s.read();
out.write(s__aux);

}
}

#endif

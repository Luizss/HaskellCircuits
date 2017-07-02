#include "systemc.h"

SC_MODULE(testbench) {
sc_fifo_in<sc_lv<32> > out;
sc_fifo_out<sc_lv<32> > __i0;
sc_fifo_out<sc_lv<32> > __i1;


void proc();

SC_CTOR(testbench) {
SC_THREAD(proc);
}
};

void testbench::proc() {__i0.write(1);
__i1.write(1);
cout << out.read() << endl;


}
#include "systemc.h"

SC_MODULE(testbench) {
sc_fifo_in<sc_lv<8> > out;
sc_fifo_out<sc_lv<32> > x;


void proc();

SC_CTOR(testbench) {
SC_THREAD(proc);
}
};

void testbench::proc() {x.write(1);
cout << out.read() << endl;
x.write(2);
cout << out.read() << endl;
x.write(3);
cout << out.read() << endl;
x.write(4);
cout << out.read() << endl;


}
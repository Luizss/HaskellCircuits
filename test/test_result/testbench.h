#include "systemc.h"

SC_MODULE(testbench) {
sc_fifo_in<sc_lv<32> > out;
sc_fifo_out<sc_lv<32> > x;
sc_fifo_out<sc_lv<32> > y;


void proc();

SC_CTOR(testbench) {
SC_THREAD(proc);
}
};

void testbench::proc() {x.write(1);
y.write(1);
cout << out.read() << endl;
x.write(2);
y.write(2);
cout << out.read() << endl;
x.write(3);
y.write(4);
cout << out.read() << endl;
x.write(4);
y.write(5);
cout << out.read() << endl;


}
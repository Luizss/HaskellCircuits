#include "systemc.h"

SC_MODULE(testbench) {
sc_fifo_in<int> out;
sc_fifo_out<int> x;
sc_fifo_out<int> y;


void proc();

SC_CTOR(testbench) {
SC_THREAD(proc);
}
};

void testbench::proc() {x.write(1);
y.write(2);
cout << out.read() << endl;
x.write(2);
y.write(1);
cout << out.read() << endl;


}
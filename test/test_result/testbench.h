#include "systemc.h"

SC_MODULE(testbench) {
sc_fifo_in<sc_lv<32> > out;
sc_fifo_out<sc_lv<32> > n;


void proc();

SC_CTOR(testbench) {
SC_THREAD(proc);
}
};

void testbench::proc() {n.write(1);
cout << out.read() << endl;
n.write(2);
cout << out.read() << endl;
n.write(3);
cout << out.read() << endl;
n.write(4);
cout << out.read() << endl;
n.write(5);
cout << out.read() << endl;
n.write(6);
cout << out.read() << endl;
n.write(7);
cout << out.read() << endl;
n.write(8);
cout << out.read() << endl;
n.write(9);
cout << out.read() << endl;
n.write(10);
cout << out.read() << endl;


}
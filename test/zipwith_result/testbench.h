#include "systemc.h"

SC_MODULE(testbench) {
sc_fifo_in<sc_lv<32> > out;
sc_fifo_out<sc_lv<32> > s1;
sc_fifo_out<sc_lv<32> > s2;
sc_fifo_out<sc_lv<32> > a;


void proc();

SC_CTOR(testbench) {
SC_THREAD(proc);
}
};

void testbench::proc() {s1.write(1);
s2.write(1);
a.write(1);
cout << out.read() << endl;
s1.write(2);
s2.write(2);
a.write(2);
cout << out.read() << endl;
s1.write(2);
s2.write(2);
a.write(3);
cout << out.read() << endl;
s1.write(3);
s2.write(3);
a.write(4);
cout << out.read() << endl;
s1.write(4);
s2.write(4);
a.write(5);
cout << out.read() << endl;


}
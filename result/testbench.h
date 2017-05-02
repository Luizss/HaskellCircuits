#include "systemc.h"

SC_MODULE(testbench) {

sc_fifo_in<int> out_0;
sc_fifo_out<int> in_0;

void proc();

SC_CTOR(testbench) {
SC_THREAD(proc);
}
};

void testbench::proc() {
in_0.write(1);
cout << out_0.read() << endl;
in_0.write(2);
cout << out_0.read() << endl;
in_0.write(3);
cout << out_0.read() << endl;
in_0.write(4);
cout << out_0.read() << endl;
in_0.write(5);
cout << out_0.read() << endl;
in_0.write(6);
cout << out_0.read() << endl;
in_0.write(7);
cout << out_0.read() << endl;
in_0.write(8);
cout << out_0.read() << endl;
in_0.write(9);
cout << out_0.read() << endl;
in_0.write(10);
cout << out_0.read() << endl;


}
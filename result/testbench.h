#include "systemc.h"

SC_MODULE(testbench) {
sc_fifo_in<int> out;
sc_fifo_out<int> x;


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

x.write(5);

cout << out.read() << endl;

x.write(6);

cout << out.read() << endl;

x.write(7);

cout << out.read() << endl;

x.write(8);

cout << out.read() << endl;

x.write(9);

cout << out.read() << endl;

x.write(10);

cout << out.read() << endl;



}
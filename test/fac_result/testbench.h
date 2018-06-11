#include "systemc.h"

SC_MODULE(testbench) {
sc_fifo_in<sc_lv<32> > out;
sc_fifo_out<sc_lv<32> > __i0;


void proc();

SC_CTOR(testbench) {
SC_THREAD(proc);
}
};

void testbench::proc() {__i0.write(0b00000000000000000000000000000001);
__i0.write(0b00000000000000000000000000000010);
__i0.write(0b00000000000000000000000000000011);
__i0.write(0b00000000000000000000000000000100);
__i0.write(0b00000000000000000000000000000101);

while(true){
cout << out.read() << endl;
}


}
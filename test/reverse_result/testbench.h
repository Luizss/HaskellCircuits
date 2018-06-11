#include "systemc.h"

SC_MODULE(testbench) {
sc_fifo_in<sc_lv<33> > out;
sc_fifo_out<sc_lv<33> > __i0;


void proc();

SC_CTOR(testbench) {
SC_THREAD(proc);
}
};

void testbench::proc() {__i0.write(0b000000000000000000000000000000001);
__i0.write(0b000000000000000000000000000000011);
__i0.write(0b000000000000000000000000000000101);
__i0.write(0b000000000000000000000000000000111);
__i0.write(0b000000000000000000000000000001001);
__i0.write(0b000000000000000000000000000001011);

__i0.write(0b000000000000000000000000000000000);
while(true){
cout << out.read() << endl;
}


}
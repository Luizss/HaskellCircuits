#include "systemc.h"

SC_MODULE(testbench) {
sc_fifo_in<sc_lv<31> > out;
sc_fifo_out<sc_lv<32> > s;


void proc();

SC_CTOR(testbench) {
SC_THREAD(proc);
}
};

void testbench::proc() {

  
  s.write(0b00000000000000000000000000000011);
  s.write(0b00000000000000000000000000000101);
  s.write(0b00000000000000000000000000011101);
  s.write(0b00000000000000000000000000000000);
  while (true) {
  cout << "ANS: " << out.read() << endl;
  }


}

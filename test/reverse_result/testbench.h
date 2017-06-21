#include "systemc.h"

SC_MODULE(testbench) {
sc_fifo_in<sc_lv<32> > out;
sc_fifo_out<sc_lv<32> > s;
sc_fifo_out<sc_lv<32> > a;
sc_fifo_out<sc_lv<32> > b;


void proc();

SC_CTOR(testbench) {
SC_THREAD(proc);
}
};

void testbench::proc() {
  
  a.write(0b00000000000000000000000000000011);
  a.write(0b00000000000000000000000000000000);
  b.write(0b00000000000000000000001110000000);
  s.write(0b00000000000000000000000000000011);
  s.write(0b00000000000000000000000000000101);
  s.write(0b00000000000000000000000000011101);
  s.write(0b00000000000000000000000000000000);
  while (true) {
  cout << "ANS: " << out.read() << endl;
  }


}

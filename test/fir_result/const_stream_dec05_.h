#ifndef CONST_STREAM_DEC05__H_
#define CONST_STREAM_DEC05__H_
#include "systemc.h"
SC_MODULE(const_stream_dec05_) {
sc_fifo_out<sc_lv<33> > out;
void proc();
SC_CTOR(const_stream_dec05_) {
SC_THREAD(proc);
}
};

void const_stream_dec05_::proc() {
while(true) {
out.write(0);

}
}
#endif
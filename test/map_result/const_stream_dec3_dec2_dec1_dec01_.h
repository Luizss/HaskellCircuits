#ifndef CONST_STREAM_DEC3_DEC2_DEC1_DEC01__H_
#define CONST_STREAM_DEC3_DEC2_DEC1_DEC01__H_
#include "systemc.h"
SC_MODULE(const_stream_dec3_dec2_dec1_dec01_) {
sc_fifo_out<sc_lv<32> > out;
void proc();
SC_CTOR(const_stream_dec3_dec2_dec1_dec01_) {
SC_THREAD(proc);
}
};

void const_stream_dec3_dec2_dec1_dec01_::proc() {
while(true) {
out.write(3);

out.write(2);

out.write(1);

out.write(0);

}
}
#endif
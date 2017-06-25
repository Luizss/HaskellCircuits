#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "const_stream_dec3_dec2_dec1_dec01_.h"
#include "map.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > s;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_stream_dec3_dec2_dec1_dec01_out__map1_a;
sc_fifo<sc_lv<32> > main1_s__map1_s;
sc_fifo<sc_lv<32> > map1_out__main1_out;

const_stream_dec3_dec2_dec1_dec01_ const_stream_dec3_dec2_dec1_dec01;
map map1;



SC_CTOR(mainFunc) : const_stream_dec3_dec2_dec1_dec01("const_stream_dec3_dec2_dec1_dec01"), map1("map1") {

const_stream_dec3_dec2_dec1_dec01.out(const_stream_dec3_dec2_dec1_dec01_out__map1_a);
map1.a(const_stream_dec3_dec2_dec1_dec01_out__map1_a);

map1.s(s);
map1.out(out);


}
};

#endif

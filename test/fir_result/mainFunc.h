#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "map.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > bs;
sc_fifo_in<sc_lv<32> > xs;
sc_fifo_in<sc_lv<32> > a;
sc_fifo_in<sc_lv<32> > c;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > main1_c__map1_c;
sc_fifo<sc_lv<32> > main1_a__map1_a;
sc_fifo<sc_lv<32> > main1_xs__map1_xs;
sc_fifo<sc_lv<32> > main1_bs__map1_bs;
sc_fifo<sc_lv<32> > map1_out__main1_out;

map map1;



SC_CTOR(mainFunc) : map1("map1") {

map1.c(c);
map1.a(a);
map1.xs(xs);
map1.bs(bs);
map1.out(out);


}
};

#endif

#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "map.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<33> > __i0;
sc_fifo_in<sc_lv<33> > __i1;
sc_fifo_in<sc_lv<33> > __i2;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<33> > main1___i2__map1___i2;
sc_fifo<sc_lv<33> > main1___i1__map1___i1;
sc_fifo<sc_lv<33> > main1___i0__map1___i0;
sc_fifo<sc_lv<33> > map1_out__main1_out;

map map1;



SC_CTOR(mainFunc) : map1("map1") {

map1.__i2(__i2);
map1.__i1(__i1);
map1.__i0(__i0);
map1.out(out);


}
};

#endif

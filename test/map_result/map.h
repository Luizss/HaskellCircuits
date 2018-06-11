#ifndef MAP_H_
#define MAP_H_

#include "systemc.h"
#include "const_stream_dec01_.h"
#include "map__left.h"


SC_MODULE(map) {
sc_fifo_in<sc_lv<33> > __i0;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<33> > const_stream_dec01_out__map__left1___acc;

const_stream_dec01_ const_stream_dec01;
map__left map__left1;



SC_CTOR(map) : const_stream_dec01("const_stream_dec01"), map__left1("map__left1") {

const_stream_dec01.out(const_stream_dec01_out__map__left1___acc);
map__left1.__acc(const_stream_dec01_out__map__left1___acc);

map__left1.__i0(__i0);
map__left1.out(out);


}
};

#endif

#ifndef TAKE_H_
#define TAKE_H_

#include "systemc.h"
#include "const_stream_dec02_.h"
#include "take__left.h"


SC_MODULE(take) {
sc_fifo_in<sc_lv<33> > __i0;
sc_fifo_in<sc_lv<32> > __i1;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<33> > const_stream_dec01_out__take__left1___acc;

const_stream_dec02_ const_stream_dec01;
take__left take__left1;



SC_CTOR(take) : const_stream_dec01("const_stream_dec01"), take__left1("take__left1") {

const_stream_dec01.out(const_stream_dec01_out__take__left1___acc);
take__left1.__acc(const_stream_dec01_out__take__left1___acc);

take__left1.__i1(__i1);
take__left1.__i0(__i0);
take__left1.out(out);


}
};

#endif

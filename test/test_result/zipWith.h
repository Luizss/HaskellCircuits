#ifndef ZIPWITH_H_
#define ZIPWITH_H_

#include "systemc.h"
#include "const_stream_dec05_.h"
#include "zipWith__left.h"


SC_MODULE(zipWith) {
sc_fifo_in<sc_lv<33> > __i0;
sc_fifo_in<sc_lv<33> > __i1;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<33> > const_stream_dec01_out__zipWith__left1___acc;

const_stream_dec05_ const_stream_dec01;
zipWith__left zipWith__left1;



SC_CTOR(zipWith) : const_stream_dec01("const_stream_dec01"), zipWith__left1("zipWith__left1") {

const_stream_dec01.out(const_stream_dec01_out__zipWith__left1___acc);
zipWith__left1.__acc(const_stream_dec01_out__zipWith__left1___acc);

zipWith__left1.__i1(__i1);
zipWith__left1.__i0(__i0);
zipWith__left1.out(out);


}
};

#endif

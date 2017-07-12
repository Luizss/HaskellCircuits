#ifndef REVERSE_H_
#define REVERSE_H_

#include "systemc.h"
#include "const_stream_dec04_.h"
#include "reverse_.h"


SC_MODULE(reverse) {
sc_fifo_in<sc_lv<33> > __i0;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<33> > const_stream_dec01_out__reverse_1___i1;

const_stream_dec04_ const_stream_dec01;
reverse_ reverse_1;



SC_CTOR(reverse) : const_stream_dec01("const_stream_dec01"), reverse_1("reverse_1") {

const_stream_dec01.out(const_stream_dec01_out__reverse_1___i1);
reverse_1.__i1(const_stream_dec01_out__reverse_1___i1);

reverse_1.__i0(__i0);
reverse_1.out(out);


}
};

#endif

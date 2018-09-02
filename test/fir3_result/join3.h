#ifndef JOIN3_H_
#define JOIN3_H_

#include "systemc.h"
#include "const_stream_dec01_.h"
#include "join3__left.h"


SC_MODULE(join3) {
sc_fifo_in<sc_lv<33> > __i0;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<33> > const_stream_dec01_out__join3__left1___acc;

const_stream_dec01_ const_stream_dec01;
join3__left join3__left1;



SC_CTOR(join3) : const_stream_dec01("const_stream_dec01"), join3__left1("join3__left1") {

const_stream_dec01.out(const_stream_dec01_out__join3__left1___acc);
join3__left1.__acc(const_stream_dec01_out__join3__left1___acc);

join3__left1.__i0(__i0);
join3__left1.out(out);


}
};

#endif

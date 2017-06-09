#ifndef G_H_
#define G_H_

#include "systemc.h"
#include "not_.h"


SC_MODULE(g) {
sc_fifo_in<sc_lv<32> > x;

sc_fifo_out<sc_lv<32> > out;


not_ not_1;



SC_CTOR(g) : not_1("not_1") {

not_1.in1(x);
not_1.out(out);


}
};

#endif

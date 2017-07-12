#ifndef G_H_
#define G_H_

#include "systemc.h"
#include "sub1_.h"


SC_MODULE(g) {
sc_fifo_in<sc_lv<32> > __i0;
sc_fifo_in<sc_lv<32> > __i1;

sc_fifo_out<sc_lv<32> > out;


sub1_ sub1;



SC_CTOR(g) : sub1("sub1") {

sub1.in2(__i1);
sub1.in1(__i0);
sub1.out(out);


}
};

#endif

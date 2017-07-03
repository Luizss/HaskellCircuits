#ifndef CONS_INT_H_
#define CONS_INT_H_

#include "systemc.h"
#include "const_bin_11_.h"
#include "cat2_.h"
#include "cat3_.h"


SC_MODULE(Cons_Int) {
sc_fifo_in<sc_lv<32> > _x0;
sc_fifo_in<sc_lv<33> > _x1;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<1> > const_bin_11_out__cat1_in2;

sc_fifo<sc_lv<65> > __fifo__1;
const_bin_11_ const_bin_11;
cat2_ cat2;
cat3_ cat1;



SC_CTOR(Cons_Int) : const_bin_11("const_bin_11"), cat2("cat2"), cat1("cat1") {

const_bin_11.out(const_bin_11_out__cat1_in2);
cat1.in2(const_bin_11_out__cat1_in2);

cat2.in2(_x1);
cat2.in1(_x0);
cat1.in1(__fifo__1);
cat2.out(__fifo__1);
cat1.out(out);


}
};

#endif

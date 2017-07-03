#ifndef NIL_INT_H_
#define NIL_INT_H_

#include "systemc.h"
#include "const_bin_01_.h"
#include "const_dec_02_.h"
#include "cat1_.h"


SC_MODULE(Nil_Int) {

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<1> > const_bin_01_out__cat1_in2;
sc_fifo<sc_lv<32> > const_dec_01_out__cat1_in1;

const_bin_01_ const_bin_01;
const_dec_02_ const_dec_01;
cat1_ cat1;



SC_CTOR(Nil_Int) : const_bin_01("const_bin_01"), const_dec_01("const_dec_01"), cat1("cat1") {

const_bin_01.out(const_bin_01_out__cat1_in2);
cat1.in2(const_bin_01_out__cat1_in2);

const_dec_01.out(const_dec_01_out__cat1_in1);
cat1.in1(const_dec_01_out__cat1_in1);

cat1.out(out);


}
};

#endif

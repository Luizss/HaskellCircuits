#ifndef __IS__USERFALSE_H_
#define __IS__USERFALSE_H_

#include "systemc.h"
#include "const_bin_11_.h"
#include "sli_0_0_1_.h"
#include "equ1_.h"


SC_MODULE(__is__UserFalse) {
sc_fifo_in<sc_lv<1> > _x0;

sc_fifo_out<sc_lv<1> > out;

sc_fifo<sc_lv<1> > const_bin_11_out__equ1_in1;

sc_fifo<sc_lv<1> > __fifo__1;
const_bin_11_ const_bin_11;
sli_0_0_1_ sli_0_0_1;
equ1_ equ1;



SC_CTOR(__is__UserFalse) : const_bin_11("const_bin_11"), sli_0_0_1("sli_0_0_1"), equ1("equ1") {

sli_0_0_1.in1(_x0);
equ1.in2(__fifo__1);
sli_0_0_1.out(__fifo__1);
const_bin_11.out(const_bin_11_out__equ1_in1);
equ1.in1(const_bin_11_out__equ1_in1);

equ1.out(out);


}
};

#endif

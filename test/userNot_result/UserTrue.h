#ifndef USERTRUE_H_
#define USERTRUE_H_

#include "systemc.h"
#include "const_bin_01_.h"


SC_MODULE(UserTrue) {

sc_fifo_out<sc_lv<1> > out;


const_bin_01_ const_bin_01;



SC_CTOR(UserTrue) : const_bin_01("const_bin_01") {

const_bin_01.out(out);


}
};

#endif

#ifndef USERFALSE_H_
#define USERFALSE_H_

#include "systemc.h"
#include "const_bin_11_.h"


SC_MODULE(UserFalse) {

sc_fifo_out<sc_lv<1> > out;


const_bin_11_ const_bin_11;



SC_CTOR(UserFalse) : const_bin_11("const_bin_11") {

const_bin_11.out(out);


}
};

#endif

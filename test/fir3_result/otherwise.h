#ifndef OTHERWISE_H_
#define OTHERWISE_H_

#include "systemc.h"
#include "const_bin_10_.h"


SC_MODULE(otherwise) {

sc_fifo_out<sc_lv<1> > out;


const_bin_10_ const_bin_11;



SC_CTOR(otherwise) : const_bin_11("const_bin_11") {

const_bin_11.out(out);


}
};

#endif

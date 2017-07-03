#ifndef TRUE_H_
#define TRUE_H_

#include "systemc.h"
#include "const_bin_11_.h"


SC_MODULE(True) {

sc_fifo_out<sc_lv<1> > out;


const_bin_11_ const_bin_11;



SC_CTOR(True) : const_bin_11("const_bin_11") {

const_bin_11.out(out);


}
};

#endif

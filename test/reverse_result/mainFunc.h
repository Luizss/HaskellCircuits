#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "reverse.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<33> > __i0;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<33> > main1___i0__reverse1___i0;
sc_fifo<sc_lv<33> > reverse1_out__main1_out;

reverse reverse1;



SC_CTOR(mainFunc) : reverse1("reverse1") {

reverse1.__i0(__i0);
reverse1.out(out);


}
};

#endif

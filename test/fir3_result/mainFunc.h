#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "join3.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<33> > __i0;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<33> > main1___i0__join31___i0;
sc_fifo<sc_lv<33> > join31_out__main1_out;

join3 join31;



SC_CTOR(mainFunc) : join31("join31") {

join31.__i0(__i0);
join31.out(out);


}
};

#endif

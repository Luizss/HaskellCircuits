#ifndef WINDOW_H_
#define WINDOW_H_

#include "systemc.h"
#include "take.h"
#include "reverse.h"


SC_MODULE(window) {
sc_fifo_in<sc_lv<32> > __i0;
sc_fifo_in<sc_lv<33> > __i1;

sc_fifo_out<sc_lv<33> > out;


sc_fifo<sc_lv<33> > __fifo__1;
take take1;
reverse reverse1;



SC_CTOR(window) : take1("take1"), reverse1("reverse1") {

take1.__i1(__i0);
take1.__i0(__i1);
reverse1.__i0(__fifo__1);
take1.out(__fifo__1);
reverse1.out(out);


}
};

#endif

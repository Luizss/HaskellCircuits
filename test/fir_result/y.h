#ifndef Y_H_
#define Y_H_

#include "systemc.h"
#include "window.h"
#include "reverse.h"
#include "zipWith.h"
#include "sum.h"


SC_MODULE(y) {
sc_fifo_in<sc_lv<33> > __i0;
sc_fifo_in<sc_lv<33> > __i1;
sc_fifo_in<sc_lv<32> > __i2;

sc_fifo_out<sc_lv<32> > out;


sc_fifo<sc_lv<33> > __fifo__3;
sc_fifo<sc_lv<33> > __fifo__2;
sc_fifo<sc_lv<33> > __fifo__1;
window window1;
reverse reverse1;
zipWith zipWith1;
sum sum1;



SC_CTOR(y) : window1("window1"), reverse1("reverse1"), zipWith1("zipWith1"), sum1("sum1") {

window1.__i1(__i1);
window1.__i0(__i2);
zipWith1.__i1(__fifo__3);
window1.out(__fifo__3);
reverse1.__i0(__i0);
zipWith1.__i0(__fifo__2);
reverse1.out(__fifo__2);
sum1.__i0(__fifo__1);
zipWith1.out(__fifo__1);
sum1.out(out);


}
};

#endif

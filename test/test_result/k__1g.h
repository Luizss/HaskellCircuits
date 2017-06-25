#ifndef K__1G_H_
#define K__1G_H_

#include "systemc.h"
#include "g.h"


SC_MODULE(k__1g) {
sc_fifo_in<sc_lv<32> > x;
sc_fifo_in<sc_lv<32> > y;
sc_fifo_in<sc_lv<32> > z;

sc_fifo_out<sc_lv<32> > out;


g g1;



SC_CTOR(k__1g) : g1("g1") {

g1.z(z);
g1.y(y);
g1.x(x);
g1.out(out);


}
};

#endif

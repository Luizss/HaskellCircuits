#ifndef Y_H_
#define Y_H_

#include "systemc.h"
#include "__fork3__.h"
#include "window.h"
#include "reverse.h"
#include "zipWith.h"
#include "sum.h"


SC_MODULE(y) {
sc_fifo_in<sc_lv<32> > bs;
sc_fifo_in<sc_lv<32> > xs;
sc_fifo_in<sc_lv<32> > a;
sc_fifo_in<sc_lv<31> > n;

sc_fifo_out<sc_lv<31> > out;

sc_fifo<sc_lv<32> > __fork3__1_out3__zipWith1_a;
sc_fifo<sc_lv<32> > __fork3__1_out2__window1_a;
sc_fifo<sc_lv<32> > __fork3__1_out1__reverse1_a;

sc_fifo<sc_lv<32> > __fifo__3;
__fork3__ __fork3__1;
sc_fifo<sc_lv<32> > __fifo__2;
sc_fifo<sc_lv<32> > __fifo__1;
window window1;
reverse reverse1;
zipWith zipWith1;
sum sum1;



SC_CTOR(y) : __fork3__1("__fork3__1"), window1("window1"), reverse1("reverse1"), zipWith1("zipWith1"), sum1("sum1") {

__fork3__1.out3(__fork3__1_out3__zipWith1_a);
zipWith1.a(__fork3__1_out3__zipWith1_a);

__fork3__1.out2(__fork3__1_out2__window1_a);
window1.a(__fork3__1_out2__window1_a);

window1.s(xs);
window1.n(n);
zipWith1.s2(__fifo__3);
window1.out(__fifo__3);
__fork3__1.out1(__fork3__1_out1__reverse1_a);
reverse1.a(__fork3__1_out1__reverse1_a);

__fork3__1.in(a);
reverse1.s(bs);
zipWith1.s1(__fifo__2);
reverse1.out(__fifo__2);
sum1.s(__fifo__1);
zipWith1.out(__fifo__1);
sum1.out(out);


}
};

#endif

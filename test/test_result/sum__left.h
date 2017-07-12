#ifndef SUM__LEFT_H_
#define SUM__LEFT_H_

#include "systemc.h"
#include "sli_1_32_14_.h"
#include "add1_.h"
#include "sli_0_0_13_.h"
#include "sli_0_0_12_.h"
#include "not_1_.h"


SC_MODULE(sum__left) {
sc_fifo_in<sc_lv<33> > __i0;
sc_fifo_in<sc_lv<32> > __acc;

sc_fifo_out<sc_lv<32> > out;


sc_fifo<sc_lv<32> > __fifo__2;
sc_fifo<sc_lv<1> > __fifo__1;
sli_1_32_14_ sli_1_32_1;
add1_ add1;
sli_0_0_13_ sli_0_0_2;
sli_0_0_12_ sli_0_0_1;
not_1_ not_1;

sc_lv<32> __acc__aux;
sc_lv<33> __i0__now__1;
sc_fifo<sc_lv<33> > __fifo__sum__left__cond__1__in__now__1____i0;
sc_fifo<sc_lv<33> > __fifo__sum__left__cond__2__in__now__1____i0;
sc_lv<1> __fifo__sum__left__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__sum__left__cond__1__out;
sc_lv<1> __fifo__sum__left__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__sum__left__cond__2__out;
sc_lv<2> cond;
sc_lv<33> __i0__destroy;
sc_fifo<sc_lv<32> > __fifo__sum__left__rec__expr__2__2__in____acc;
sc_fifo<sc_lv<33> > __fifo__sum__left__rec__expr__2__2__in__now__1____i0;
sc_lv<32> __fifo__sum__left__rec__expr__2__2__out__aux;
sc_fifo<sc_lv<32> > __fifo__sum__left__rec__expr__2__2__out;

void proc();
SC_CTOR(sum__left) : sli_1_32_1("sli_1_32_1"), add1("add1"), sli_0_0_2("sli_0_0_2"), sli_0_0_1("sli_0_0_1"), not_1("not_1") {

add1.in2(__fifo__sum__left__rec__expr__2__2__in____acc);
sli_1_32_1.in1(__fifo__sum__left__rec__expr__2__2__in__now__1____i0);
add1.in1(__fifo__2);
sli_1_32_1.out(__fifo__2);
add1.out(__fifo__sum__left__rec__expr__2__2__out);
sli_0_0_2.in1(__fifo__sum__left__cond__2__in__now__1____i0);
sli_0_0_2.out(__fifo__sum__left__cond__2__out);
sli_0_0_1.in1(__fifo__sum__left__cond__1__in__now__1____i0);
not_1.in1(__fifo__1);
sli_0_0_1.out(__fifo__1);
not_1.out(__fifo__sum__left__cond__1__out);

SC_THREAD(proc);
}
};
void sum__left::proc() {
while(true) {
__acc__aux = __acc.read();
while (true) {
__i0__now__1 = __i0.read();
__fifo__sum__left__cond__1__in__now__1____i0.write(__i0__now__1);
__fifo__sum__left__cond__2__in__now__1____i0.write(__i0__now__1);
__fifo__sum__left__cond__1__out__aux = __fifo__sum__left__cond__1__out.read();
__fifo__sum__left__cond__2__out__aux = __fifo__sum__left__cond__2__out.read();
cond = (__fifo__sum__left__cond__2__out__aux, __fifo__sum__left__cond__1__out__aux);
if (cond[0]==1) {
out.write(__acc__aux);
if (__i0__now__1 != 0) {
__i0__destroy = __i0.read();
if (__i0__destroy != 0) {
while(__i0.nb_read(__i0__destroy)) { if (__i0__destroy == 0) break; }
}
}

break;


} else {
__fifo__sum__left__rec__expr__2__2__in____acc.write(__acc__aux);
__fifo__sum__left__rec__expr__2__2__in__now__1____i0.write(__i0__now__1);
__fifo__sum__left__rec__expr__2__2__out__aux = __fifo__sum__left__rec__expr__2__2__out.read();
__acc__aux = __fifo__sum__left__rec__expr__2__2__out__aux;

}

}

}
}

#endif

#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "__fork2__.h"
#include "const_dec_12_.h"
#include "otherwise.h"
#include "sli_1_32_7_.h"
#include "equ2_.h"
#include "sli_0_0_6_.h"
#include "and_1_.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<33> > __i0;
sc_fifo_in<sc_lv<33> > __i1;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<32> > const_dec_11_out__equ1_in2;
sc_fifo<sc_lv<33> > __fork2__1_out2__sli_1_32_1_in1;
sc_fifo<sc_lv<33> > __fork2__1_out1__sli_0_0_1_in1;

sc_fifo<sc_lv<32> > __fifo__3;
sc_fifo<sc_lv<1> > __fifo__2;
__fork2__ __fork2__1;
sc_fifo<sc_lv<1> > __fifo__1;
const_dec_12_ const_dec_11;
otherwise otherwise1;
sli_1_32_7_ sli_1_32_1;
equ2_ equ1;
sli_0_0_6_ sli_0_0_1;
and_1_ and_1;

sc_fifo<sc_lv<33> > __fifo__main__cond__1__in__now__1____i0;
sc_lv<1> __fifo__main__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__1__out;
sc_lv<1> __fifo__main__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__2__out;
sc_lv<2> cond;
sc_fifo<sc_lv<33> > __fifo__main__expr__1__out;
sc_lv<33> __fifo__main__expr__1__out__copy__val;
sc_lv<33> __i0__destroy;
sc_lv<33> __i1__destroy;
sc_lv<33> __i1__copy__val;

void proc();
SC_CTOR(mainFunc) : __fork2__1("__fork2__1"), const_dec_11("const_dec_11"), otherwise1("otherwise1"), sli_1_32_1("sli_1_32_1"), equ1("equ1"), sli_0_0_1("sli_0_0_1"), and_1("and_1") {

otherwise1.out(__fifo__main__cond__2__out);
const_dec_11.out(const_dec_11_out__equ1_in2);
equ1.in2(const_dec_11_out__equ1_in2);

__fork2__1.out2(__fork2__1_out2__sli_1_32_1_in1);
sli_1_32_1.in1(__fork2__1_out2__sli_1_32_1_in1);

equ1.in1(__fifo__3);
sli_1_32_1.out(__fifo__3);
and_1.in2(__fifo__2);
equ1.out(__fifo__2);
__fork2__1.out1(__fork2__1_out1__sli_0_0_1_in1);
sli_0_0_1.in1(__fork2__1_out1__sli_0_0_1_in1);

__fork2__1.in(__fifo__main__cond__1__in__now__1____i0);
and_1.in1(__fifo__1);
sli_0_0_1.out(__fifo__1);
and_1.out(__fifo__main__cond__1__out);

SC_THREAD(proc);
}
};
void mainFunc::proc() {
while(true) {
__fifo__main__cond__1__in__now__1____i0.write(__i0__now__1);
__fifo__main__cond__1__out__aux = __fifo__main__cond__1__out.read();
__fifo__main__cond__2__out__aux = __fifo__main__cond__2__out.read();
cond = (__fifo__main__cond__2__out__aux, __fifo__main__cond__1__out__aux);
if (cond[0]==1) {
__fifo__main__expr__1__out__copy__val = __fifo__main__expr__1__out.read();
out.write(__fifo__main__expr__1__out__copy__val);
if (__fifo__main__expr__1__out__copy__val != 0) {
while (__fifo__main__expr__1__out.nb_read(__fifo__main__expr__1__out__copy__val)) {
out.write(__fifo__main__expr__1__out__copy__val);
if (__fifo__main__expr__1__out__copy__val == 0) break;
}
}

if (__i0__now__1 != 0) {
__i0__destroy = __i0.read();
if (__i0__destroy != 0) {
while(__i0.nb_read(__i0__destroy)) { if (__i0__destroy == 0) break; }
}
}

__i1__destroy = __i1.read();
if (__i1__destroy != 0) {
while(__i1.nb_read(__i1__destroy)) { if (__i1__destroy == 0) break; }
}



} else {
__i1__copy__val = __i1.read();
out.write(__i1__copy__val);
if (__i1__copy__val != 0) {
while (__i1.nb_read(__i1__copy__val)) {
out.write(__i1__copy__val);
if (__i1__copy__val == 0) break;
}
}

if (__i0__now__1 != 0) {
__i0__destroy = __i0.read();
if (__i0__destroy != 0) {
while(__i0.nb_read(__i0__destroy)) { if (__i0__destroy == 0) break; }
}
}

__i1__destroy = __i1.read();
if (__i1__destroy != 0) {
while(__i1.nb_read(__i1__destroy)) { if (__i1__destroy == 0) break; }
}


}

}
}

#endif

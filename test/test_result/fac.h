#ifndef FAC_H_
#define FAC_H_

#include "systemc.h"
#include "const_bin_1.h"
#include "const_dec_1.h"
#include "const_dec_6.h"
#include "const_dec_3.h"
#include "const_dec_0.h"
#include "mul.h"
#include "sub.h"
#include "equ.h"
#include "add.h"


SC_MODULE(fac) {
sc_fifo_in<sc_lv<32> > n;
sc_fifo_in<sc_lv<32> > a;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_bin_11_out__sub1_in2;
sc_fifo<sc_lv<32> > const_dec_61_out__mul1_in1;
sc_fifo<sc_lv<32> > const_dec_31_out__equ2_in2;
sc_fifo<sc_lv<32> > const_dec_02_out__add1_in2;
sc_fifo<sc_lv<32> > const_dec_01_out__equ1_in2;

const_bin_1 const_bin_11;
const_dec_1 const_dec_11;
const_dec_6 const_dec_61;
const_dec_3 const_dec_31;
const_dec_0 const_dec_02;
const_dec_0 const_dec_01;
mul mul2;
sub sub1;
mul mul1;
equ equ2;
add add1;
equ equ1;

sc_lv<32> n__aux;
sc_lv<32> a__aux;
sc_fifo<sc_lv<32> > __fifo__fac__cond__1__in__n;
sc_fifo<sc_lv<32> > __fifo__fac__cond__2__in__n;
sc_lv<1> __fifo__fac__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__fac__cond__1__out;
sc_lv<1> __fifo__fac__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__fac__cond__2__out;
sc_lv<1> __fifo__fac__cond__3__out__aux;
sc_fifo<sc_lv<1> > __fifo__fac__cond__3__out;
sc_lv<3> cond;
sc_fifo<sc_lv<32> > __fifo__fac__expr__1__in__a;
sc_lv<32> __fifo__fac__expr__1__out__aux;
sc_fifo<sc_lv<32> > __fifo__fac__expr__1__out;
sc_fifo<sc_lv<32> > __fifo__fac__expr__2__in__a;
sc_lv<32> __fifo__fac__expr__2__out__aux;
sc_fifo<sc_lv<32> > __fifo__fac__expr__2__out;
sc_fifo<sc_lv<32> > __fifo__fac__rec__expr__3__1__in__n;
sc_fifo<sc_lv<32> > __fifo__fac__rec__expr__3__2__in__n;
sc_fifo<sc_lv<32> > __fifo__fac__rec__expr__3__2__in__a;
sc_lv<32> __fifo__fac__rec__expr__3__1__out__aux;
sc_fifo<sc_lv<32> > __fifo__fac__rec__expr__3__1__out;
sc_lv<32> __fifo__fac__rec__expr__3__2__out__aux;
sc_fifo<sc_lv<32> > __fifo__fac__rec__expr__3__2__out;

void proc();
SC_CTOR(fac) : const_bin_11("const_bin_11"), const_dec_11("const_dec_11"), const_dec_61("const_dec_61"), const_dec_31("const_dec_31"), const_dec_02("const_dec_02"), const_dec_01("const_dec_01"), mul2("mul2"), sub1("sub1"), mul1("mul1"), equ2("equ2"), add1("add1"), equ1("equ1") {

mul2.in2(__fifo__fac__rec__expr__3__2__in__a);
mul2.in1(__fifo__fac__rec__expr__3__2__in__n);
mul2.out(__fifo__fac__rec__expr__3__2__out);
const_bin_11.out(const_bin_11_out__sub1_in2);
sub1.in2(const_bin_11_out__sub1_in2);

sub1.in1(__fifo__fac__rec__expr__3__1__in__n);
sub1.out(__fifo__fac__rec__expr__3__1__out);
const_dec_11.out(__fifo__fac__cond__3__out);
mul1.in2(__fifo__fac__expr__2__in__a);
const_dec_61.out(const_dec_61_out__mul1_in1);
mul1.in1(const_dec_61_out__mul1_in1);

mul1.out(__fifo__fac__expr__2__out);
const_dec_31.out(const_dec_31_out__equ2_in2);
equ2.in2(const_dec_31_out__equ2_in2);

equ2.in1(__fifo__fac__cond__2__in__n);
equ2.out(__fifo__fac__cond__2__out);
const_dec_02.out(const_dec_02_out__add1_in2);
add1.in2(const_dec_02_out__add1_in2);

add1.in1(__fifo__fac__expr__1__in__a);
add1.out(__fifo__fac__expr__1__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

equ1.in1(__fifo__fac__cond__1__in__n);
equ1.out(__fifo__fac__cond__1__out);

SC_THREAD(proc);
}
};
void fac::proc() {
while(true) {
n__aux = n.read();
a__aux = a.read();
while (true) {
__fifo__fac__cond__1__in__n.write(n__aux);
__fifo__fac__cond__2__in__n.write(n__aux);
__fifo__fac__cond__1__out__aux = __fifo__fac__cond__1__out.read();
__fifo__fac__cond__2__out__aux = __fifo__fac__cond__2__out.read();
__fifo__fac__cond__3__out__aux = __fifo__fac__cond__3__out.read();
cond = (__fifo__fac__cond__3__out__aux, __fifo__fac__cond__2__out__aux, __fifo__fac__cond__1__out__aux);
if (cond[0]==1) {
__fifo__fac__expr__1__in__a.write(a__aux);
__fifo__fac__expr__1__out__aux = __fifo__fac__expr__1__out.read();
out.write(__fifo__fac__expr__1__out__aux);
break;


} else if (cond[1]==1) {
__fifo__fac__expr__2__in__a.write(a__aux);
__fifo__fac__expr__2__out__aux = __fifo__fac__expr__2__out.read();
out.write(__fifo__fac__expr__2__out__aux);
break;


} else {
__fifo__fac__rec__expr__3__1__in__n.write(n__aux);
__fifo__fac__rec__expr__3__2__in__n.write(n__aux);
__fifo__fac__rec__expr__3__2__in__a.write(a__aux);
__fifo__fac__rec__expr__3__1__out__aux = __fifo__fac__rec__expr__3__1__out.read();
__fifo__fac__rec__expr__3__2__out__aux = __fifo__fac__rec__expr__3__2__out.read();
n__aux = __fifo__fac__rec__expr__3__1__out__aux;
a__aux = __fifo__fac__rec__expr__3__2__out__aux;

}

}

}
}

#endif

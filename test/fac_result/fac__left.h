#ifndef FAC__LEFT_H_
#define FAC__LEFT_H_

#include "systemc.h"
#include "const_dec_11_.h"
#include "const_dec_02_.h"
#include "mul1_.h"
#include "sub1_.h"
#include "otherwise.h"
#include "equ2_.h"


SC_MODULE(fac__left) {
sc_fifo_in<sc_lv<32> > __i0;
sc_fifo_in<sc_lv<32> > __acc;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_dec_11_out__sub1_in2;
sc_fifo<sc_lv<32> > const_dec_01_out__equ1_in2;

const_dec_11_ const_dec_11;
const_dec_02_ const_dec_01;
mul1_ mul1;
sub1_ sub1;
otherwise otherwise1;
equ2_ equ1;

sc_lv<32> __i0__aux;
sc_lv<32> __acc__aux;
sc_fifo<sc_lv<32> > __fifo__fac__left__cond__1__in____i0;
sc_lv<1> __fifo__fac__left__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__fac__left__cond__1__out;
sc_lv<1> __fifo__fac__left__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__fac__left__cond__2__out;
sc_lv<2> cond;
sc_fifo<sc_lv<32> > __fifo__fac__left__rec__expr__2__1__in____i0;
sc_fifo<sc_lv<32> > __fifo__fac__left__rec__expr__2__2__in____i0;
sc_fifo<sc_lv<32> > __fifo__fac__left__rec__expr__2__2__in____acc;
sc_lv<32> __fifo__fac__left__rec__expr__2__1__out__aux;
sc_fifo<sc_lv<32> > __fifo__fac__left__rec__expr__2__1__out;
sc_lv<32> __fifo__fac__left__rec__expr__2__2__out__aux;
sc_fifo<sc_lv<32> > __fifo__fac__left__rec__expr__2__2__out;

void proc();
SC_CTOR(fac__left) : const_dec_11("const_dec_11"), const_dec_01("const_dec_01"), mul1("mul1"), sub1("sub1"), otherwise1("otherwise1"), equ1("equ1") {

mul1.in2(__fifo__fac__left__rec__expr__2__2__in____acc);
mul1.in1(__fifo__fac__left__rec__expr__2__2__in____i0);
mul1.out(__fifo__fac__left__rec__expr__2__2__out);
const_dec_11.out(const_dec_11_out__sub1_in2);
sub1.in2(const_dec_11_out__sub1_in2);

sub1.in1(__fifo__fac__left__rec__expr__2__1__in____i0);
sub1.out(__fifo__fac__left__rec__expr__2__1__out);
otherwise1.out(__fifo__fac__left__cond__2__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

equ1.in1(__fifo__fac__left__cond__1__in____i0);
equ1.out(__fifo__fac__left__cond__1__out);

SC_THREAD(proc);
}
};
void fac__left::proc() {
while(true) {
__i0__aux = __i0.read();
__acc__aux = __acc.read();
while (true) {
__fifo__fac__left__cond__1__in____i0.write(__i0__aux);
__fifo__fac__left__cond__1__out__aux = __fifo__fac__left__cond__1__out.read();
__fifo__fac__left__cond__2__out__aux = __fifo__fac__left__cond__2__out.read();
cond = (__fifo__fac__left__cond__2__out__aux, __fifo__fac__left__cond__1__out__aux);
if (cond[0]==1) {
out.write(__acc__aux);
break;


} else {
__fifo__fac__left__rec__expr__2__1__in____i0.write(__i0__aux);
__fifo__fac__left__rec__expr__2__2__in____i0.write(__i0__aux);
__fifo__fac__left__rec__expr__2__2__in____acc.write(__acc__aux);
__fifo__fac__left__rec__expr__2__1__out__aux = __fifo__fac__left__rec__expr__2__1__out.read();
__fifo__fac__left__rec__expr__2__2__out__aux = __fifo__fac__left__rec__expr__2__2__out.read();
__i0__aux = __fifo__fac__left__rec__expr__2__1__out__aux;
__acc__aux = __fifo__fac__left__rec__expr__2__2__out__aux;

}

}

}
}

#endif

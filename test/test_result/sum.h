#ifndef SUM_H_
#define SUM_H_

#include "systemc.h"
#include "const_dec_12_.h"
#include "const_dec_03_.h"
#include "const_dec_01_.h"
#include "sli_1_31_2_.h"
#include "add1_.h"
#include "sli_0_0_1_.h"
#include "equ1_.h"


SC_MODULE(sum) {
sc_fifo_in<sc_lv<32> > s;
sc_fifo_in<sc_lv<31> > a;

sc_fifo_out<sc_lv<31> > out;

sc_fifo<sc_lv<1> > const_dec_03_out__equ2_in2;
sc_fifo<sc_lv<31> > const_dec_02_out__add1_in2;
sc_fifo<sc_lv<1> > const_dec_01_out__equ1_in2;

sc_fifo<sc_lv<31> > __fifo__5;
sc_fifo<sc_lv<31> > __fifo__4;
sc_fifo<sc_lv<31> > __fifo__3;
sc_fifo<sc_lv<1> > __fifo__2;
sc_fifo<sc_lv<1> > __fifo__1;
const_dec_12_ const_dec_11;
const_dec_03_ const_dec_03;
const_dec_01_ const_dec_02;
const_dec_03_ const_dec_01;
sli_1_31_2_ sli_1_31_3;
sli_1_31_2_ sli_1_31_2;
add1_ add3;
sli_1_31_2_ sli_1_31_1;
add1_ add2;
sli_0_0_1_ sli_0_0_2;
equ1_ equ2;
add1_ add1;
sli_0_0_1_ sli_0_0_1;
equ1_ equ1;

sc_lv<31> a__aux;
sc_lv<32> s__now__1;
sc_lv<32> s__now__2;
sc_fifo<sc_lv<32> > __fifo__sum__cond__1__in__now__1__s;
sc_fifo<sc_lv<32> > __fifo__sum__cond__2__in__now__2__s;
sc_lv<1> __fifo__sum__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__sum__cond__1__out;
sc_lv<1> __fifo__sum__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__sum__cond__2__out;
sc_lv<1> __fifo__sum__cond__3__out__aux;
sc_fifo<sc_lv<1> > __fifo__sum__cond__3__out;
sc_lv<3> cond;
sc_fifo<sc_lv<31> > __fifo__sum__expr__1__in__a;
sc_lv<31> __fifo__sum__expr__1__out__aux;
sc_fifo<sc_lv<31> > __fifo__sum__expr__1__out;
sc_lv<32> s__destroy;
sc_fifo<sc_lv<31> > __fifo__sum__expr__2__in__a;
sc_fifo<sc_lv<32> > __fifo__sum__expr__2__in__now__1__s;
sc_lv<31> __fifo__sum__expr__2__out__aux;
sc_fifo<sc_lv<31> > __fifo__sum__expr__2__out;
sc_fifo<sc_lv<32> > __fifo__sum__rec__expr__3__2__in__now__1__s;
sc_fifo<sc_lv<32> > __fifo__sum__rec__expr__3__2__in__now__2__s;
sc_lv<31> __fifo__sum__rec__expr__3__2__out__aux;
sc_fifo<sc_lv<31> > __fifo__sum__rec__expr__3__2__out;

void proc();
SC_CTOR(sum) : const_dec_11("const_dec_11"), const_dec_03("const_dec_03"), const_dec_02("const_dec_02"), const_dec_01("const_dec_01"), sli_1_31_3("sli_1_31_3"), sli_1_31_2("sli_1_31_2"), add3("add3"), sli_1_31_1("sli_1_31_1"), add2("add2"), sli_0_0_2("sli_0_0_2"), equ2("equ2"), add1("add1"), sli_0_0_1("sli_0_0_1"), equ1("equ1") {

sli_1_31_3.in1(__fifo__sum__rec__expr__3__2__in__now__2__s);
add3.in2(__fifo__5);
sli_1_31_3.out(__fifo__5);
sli_1_31_2.in1(__fifo__sum__rec__expr__3__2__in__now__1__s);
add3.in1(__fifo__4);
sli_1_31_2.out(__fifo__4);
add3.out(__fifo__sum__rec__expr__3__2__out);
const_dec_11.out(__fifo__sum__cond__3__out);
sli_1_31_1.in1(__fifo__sum__expr__2__in__now__1__s);
add2.in2(__fifo__3);
sli_1_31_1.out(__fifo__3);
add2.in1(__fifo__sum__expr__2__in__a);
add2.out(__fifo__sum__expr__2__out);
const_dec_03.out(const_dec_03_out__equ2_in2);
equ2.in2(const_dec_03_out__equ2_in2);

sli_0_0_2.in1(__fifo__sum__cond__2__in__now__2__s);
equ2.in1(__fifo__2);
sli_0_0_2.out(__fifo__2);
equ2.out(__fifo__sum__cond__2__out);
const_dec_02.out(const_dec_02_out__add1_in2);
add1.in2(const_dec_02_out__add1_in2);

add1.in1(__fifo__sum__expr__1__in__a);
add1.out(__fifo__sum__expr__1__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

sli_0_0_1.in1(__fifo__sum__cond__1__in__now__1__s);
equ1.in1(__fifo__1);
sli_0_0_1.out(__fifo__1);
equ1.out(__fifo__sum__cond__1__out);

SC_THREAD(proc);
}
};
void sum::proc() {
while(true) {
a__aux = a.read();
while (true) {
s__now__1 = s.read();
s__now__2 = s.read();
__fifo__sum__cond__1__in__now__1__s.write(s__now__1);
__fifo__sum__cond__2__in__now__2__s.write(s__now__2);
__fifo__sum__cond__1__out__aux = __fifo__sum__cond__1__out.read();
__fifo__sum__cond__2__out__aux = __fifo__sum__cond__2__out.read();
__fifo__sum__cond__3__out__aux = __fifo__sum__cond__3__out.read();
cond = (__fifo__sum__cond__3__out__aux, __fifo__sum__cond__2__out__aux, __fifo__sum__cond__1__out__aux);
if (cond[0]==1) {
__fifo__sum__expr__1__in__a.write(a__aux);
__fifo__sum__expr__1__out__aux = __fifo__sum__expr__1__out.read();
out.write(__fifo__sum__expr__1__out__aux);
while(s.nb_read(s__destroy)) {}

break;


} else if (cond[1]==1) {
__fifo__sum__expr__2__in__a.write(a__aux);
__fifo__sum__expr__2__in__now__1__s.write(s__now__1);
__fifo__sum__expr__2__out__aux = __fifo__sum__expr__2__out.read();
out.write(__fifo__sum__expr__2__out__aux);
while(s.nb_read(s__destroy)) {}

break;


} else {
__fifo__sum__rec__expr__3__2__in__now__1__s.write(s__now__1);
__fifo__sum__rec__expr__3__2__in__now__2__s.write(s__now__2);
__fifo__sum__rec__expr__3__2__out__aux = __fifo__sum__rec__expr__3__2__out.read();
a__aux = __fifo__sum__rec__expr__3__2__out__aux;

}

}

}
}

#endif

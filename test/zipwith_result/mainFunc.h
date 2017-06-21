#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "const_dec_12_.h"
#include "const_dec_02_.h"
#include "sli_1_31_2_.h"
#include "f.h"
#include "cat1_.h"
#include "sli_0_0_1_.h"
#include "equ1_.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > s1;
sc_fifo_in<sc_lv<32> > s2;
sc_fifo_in<sc_lv<32> > a;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<1> > const_dec_12_out__cat1_in2;
sc_fifo<sc_lv<1> > const_dec_01_out__equ1_in2;

sc_fifo<sc_lv<31> > __fifo__4;
sc_fifo<sc_lv<31> > __fifo__3;
sc_fifo<sc_lv<31> > __fifo__2;
sc_fifo<sc_lv<1> > __fifo__1;
const_dec_12_ const_dec_12;
const_dec_12_ const_dec_11;
const_dec_02_ const_dec_01;
sli_1_31_2_ sli_1_31_2;
sli_1_31_2_ sli_1_31_1;
f f1;
cat1_ cat1;
sli_0_0_1_ sli_0_0_1;
equ1_ equ1;

sc_fifo<sc_lv<32> > a__save;
sc_lv<32> a__save__val;
sc_lv<32> s1__now__1;
sc_lv<32> s2__now__1;
sc_fifo<sc_lv<32> > __fifo__main__cond__1__in__now__1__s1;
sc_lv<1> __fifo__main__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__1__out;
sc_lv<1> __fifo__main__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__2__out;
sc_lv<2> cond;
sc_lv<32> a__save__copy__val;
sc_lv<32> s1__destroy;
sc_lv<32> s2__destroy;
sc_lv<32> a__destroy;
sc_fifo<sc_lv<32> > __fifo__main__rec__expr__2__3__consR__1__in__now__1__s1;
sc_fifo<sc_lv<32> > __fifo__main__rec__expr__2__3__consR__1__in__now__1__s2;
sc_lv<32> __fifo__main__rec__expr__2__3__consR__1__out__aux;
sc_fifo<sc_lv<32> > __fifo__main__rec__expr__2__3__consR__1__out;

void proc();
SC_CTOR(mainFunc) : const_dec_12("const_dec_12"), const_dec_11("const_dec_11"), const_dec_01("const_dec_01"), sli_1_31_2("sli_1_31_2"), sli_1_31_1("sli_1_31_1"), f1("f1"), cat1("cat1"), sli_0_0_1("sli_0_0_1"), equ1("equ1") {

const_dec_12.out(const_dec_12_out__cat1_in2);
cat1.in2(const_dec_12_out__cat1_in2);

sli_1_31_2.in1(__fifo__main__rec__expr__2__3__consR__1__in__now__1__s2);
f1.y(__fifo__4);
sli_1_31_2.out(__fifo__4);
sli_1_31_1.in1(__fifo__main__rec__expr__2__3__consR__1__in__now__1__s1);
f1.x(__fifo__3);
sli_1_31_1.out(__fifo__3);
cat1.in1(__fifo__2);
f1.out(__fifo__2);
cat1.out(__fifo__main__rec__expr__2__3__consR__1__out);
const_dec_11.out(__fifo__main__cond__2__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

sli_0_0_1.in1(__fifo__main__cond__1__in__now__1__s1);
equ1.in1(__fifo__1);
sli_0_0_1.out(__fifo__1);
equ1.out(__fifo__main__cond__1__out);

SC_THREAD(proc);
}
};
void mainFunc::proc() {
while(true) {
wait(SC_ZERO_TIME);
while (a.nb_read(a__save__val)) {
a__save.write(a__save__val);
}

while (true) {
s1__now__1 = s1.read();
s2__now__1 = s2.read();
__fifo__main__cond__1__in__now__1__s1.write(s1__now__1);
__fifo__main__cond__1__out__aux = __fifo__main__cond__1__out.read();
__fifo__main__cond__2__out__aux = __fifo__main__cond__2__out.read();
cond = (__fifo__main__cond__2__out__aux, __fifo__main__cond__1__out__aux);
if (cond[0]==1) {
while (a__save.nb_read(a__save__copy__val)) {
out.write(a__save__copy__val);
}

while(s1.nb_read(s1__destroy)) {}
while(s2.nb_read(s2__destroy)) {}
while(a.nb_read(a__destroy)) {}

break;


} else {
__fifo__main__rec__expr__2__3__consR__1__in__now__1__s1.write(s1__now__1);
__fifo__main__rec__expr__2__3__consR__1__in__now__1__s2.write(s2__now__1);
__fifo__main__rec__expr__2__3__consR__1__out__aux = __fifo__main__rec__expr__2__3__consR__1__out.read();
out.write(__fifo__main__rec__expr__2__3__consR__1__out__aux);

}

}

}
}

#endif

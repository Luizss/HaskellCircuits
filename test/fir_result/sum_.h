#ifndef SUM__H_
#define SUM__H_

#include "systemc.h"
#include "const_dec_12_.h"
#include "const_dec_02_.h"
#include "sli_1_31_2_.h"
#include "add1_.h"
#include "sli_0_0_1_.h"
#include "equ1_.h"


SC_MODULE(sum_) {
sc_fifo_in<sc_lv<32> > s;
sc_fifo_in<sc_lv<31> > a;

sc_fifo_out<sc_lv<31> > out;

sc_fifo<sc_lv<1> > const_dec_01_out__equ1_in2;

sc_fifo<sc_lv<31> > __fifo__2;
sc_fifo<sc_lv<1> > __fifo__1;
const_dec_12_ const_dec_11;
const_dec_02_ const_dec_01;
sli_1_31_2_ sli_1_31_1;
add1_ add1;
sli_0_0_1_ sli_0_0_1;
equ1_ equ1;

sc_lv<31> a__aux;
sc_lv<32> s__now__1;
sc_fifo<sc_lv<32> > __fifo__sum___cond__1__in__now__1__s;
sc_lv<1> __fifo__sum___cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__sum___cond__1__out;
sc_lv<1> __fifo__sum___cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__sum___cond__2__out;
sc_lv<2> cond;
sc_lv<32> s__destroy;
sc_fifo<sc_lv<31> > __fifo__sum___rec__expr__2__2__in__a;
sc_fifo<sc_lv<32> > __fifo__sum___rec__expr__2__2__in__now__1__s;
sc_lv<31> __fifo__sum___rec__expr__2__2__out__aux;
sc_fifo<sc_lv<31> > __fifo__sum___rec__expr__2__2__out;

void proc();
SC_CTOR(sum_) : const_dec_11("const_dec_11"), const_dec_01("const_dec_01"), sli_1_31_1("sli_1_31_1"), add1("add1"), sli_0_0_1("sli_0_0_1"), equ1("equ1") {

add1.in2(__fifo__sum___rec__expr__2__2__in__a);
sli_1_31_1.in1(__fifo__sum___rec__expr__2__2__in__now__1__s);
add1.in1(__fifo__2);
sli_1_31_1.out(__fifo__2);
add1.out(__fifo__sum___rec__expr__2__2__out);
const_dec_11.out(__fifo__sum___cond__2__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

sli_0_0_1.in1(__fifo__sum___cond__1__in__now__1__s);
equ1.in1(__fifo__1);
sli_0_0_1.out(__fifo__1);
equ1.out(__fifo__sum___cond__1__out);

SC_THREAD(proc);
}
};
void sum_::proc() {
while(true) {
a__aux = a.read();
while (true) {
s__now__1 = s.read();
__fifo__sum___cond__1__in__now__1__s.write(s__now__1);
__fifo__sum___cond__1__out__aux = __fifo__sum___cond__1__out.read();
__fifo__sum___cond__2__out__aux = __fifo__sum___cond__2__out.read();
cond = (__fifo__sum___cond__2__out__aux, __fifo__sum___cond__1__out__aux);
if (cond[0]==1) {
out.write(a__aux);
if (s__now__1 != 0) {
s__destroy = s.read();
if (s__destroy != 0) {
while(s.nb_read(s__destroy)) { if (s__destroy == 0) break; }
}
}

break;


} else {
__fifo__sum___rec__expr__2__2__in__a.write(a__aux);
__fifo__sum___rec__expr__2__2__in__now__1__s.write(s__now__1);
__fifo__sum___rec__expr__2__2__out__aux = __fifo__sum___rec__expr__2__2__out.read();
a__aux = __fifo__sum___rec__expr__2__2__out__aux;

}

}

}
}

#endif

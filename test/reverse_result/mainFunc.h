#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "const_dec_12_.h"
#include "const_dec_02_.h"
#include "sli_0_0_1_.h"
#include "equ1_.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > s;
sc_fifo_in<sc_lv<32> > a;
sc_fifo_in<sc_lv<32> > b;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<1> > const_dec_01_out__equ1_in2;

sc_fifo<sc_lv<1> > __fifo__1;
const_dec_12_ const_dec_11;
const_dec_02_ const_dec_01;
sli_0_0_1_ sli_0_0_1;
equ1_ equ1;

sc_lv<32> b__aux;
sc_fifo<sc_lv<32> > a__save;
sc_lv<32> a__save__val;
sc_lv<32> s__now__1;
sc_fifo<sc_lv<32> > __fifo__main__cond__1__in__now__1__s;
sc_lv<1> __fifo__main__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__1__out;
sc_lv<1> __fifo__main__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__2__out;
sc_lv<2> cond;
sc_lv<32> a__save__copy__val;
sc_lv<32> s__destroy;
sc_lv<32> a__destroy;

void proc();
SC_CTOR(mainFunc) : const_dec_11("const_dec_11"), const_dec_01("const_dec_01"), sli_0_0_1("sli_0_0_1"), equ1("equ1") {

const_dec_11.out(__fifo__main__cond__2__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

sli_0_0_1.in1(__fifo__main__cond__1__in__now__1__s);
equ1.in1(__fifo__1);
sli_0_0_1.out(__fifo__1);
equ1.out(__fifo__main__cond__1__out);

SC_THREAD(proc);
}
};
void mainFunc::proc() {
while(true) {
b__aux = b.read();
wait(SC_ZERO_TIME);
while (a.nb_read(a__save__val)) {
a__save.write(a__save__val);
}

while (true) {
s__now__1 = s.read();
__fifo__main__cond__1__in__now__1__s.write(s__now__1);
__fifo__main__cond__1__out__aux = __fifo__main__cond__1__out.read();
__fifo__main__cond__2__out__aux = __fifo__main__cond__2__out.read();
cond = (__fifo__main__cond__2__out__aux, __fifo__main__cond__1__out__aux);
if (cond[0]==1) {
while (a__save.nb_read(a__save__copy__val)) {
out.write(a__save__copy__val);
}

while(s.nb_read(s__destroy)) {}
while(a.nb_read(a__destroy)) {}

break;


} else {
//blob
out.write(s__now__1);
//blob

}

}

}
}

#endif

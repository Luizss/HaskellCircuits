#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "const_dec_12_.h"
#include "const_dec_11_.h"
#include "add1_.h"
#include "equ1_.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > n;
sc_fifo_in<sc_lv<32> > k;
sc_fifo_in<sc_lv<32> > s;
sc_fifo_in<sc_lv<32> > a;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_dec_12_out__add1_in2;

const_dec_12_ const_dec_12;
const_dec_11_ const_dec_11;
add1_ add1;
equ1_ equ1;

sc_lv<32> n__aux;
sc_lv<32> k__aux;
sc_fifo<sc_lv<32> > a__save;
sc_lv<32> a__save__val;
sc_lv<32> s__now__1;
sc_fifo<sc_lv<32> > __fifo__main__cond__1__in__n;
sc_fifo<sc_lv<32> > __fifo__main__cond__1__in__k;
sc_lv<1> __fifo__main__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__1__out;
sc_lv<1> __fifo__main__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__2__out;
sc_lv<2> cond;
sc_lv<32> a__save__copy__val;
sc_lv<32> s__destroy;
sc_lv<32> a__destroy;
sc_fifo<sc_lv<32> > __fifo__main__rec__expr__2__2__in__k;
sc_lv<32> __fifo__main__rec__expr__2__2__out__aux;
sc_fifo<sc_lv<32> > __fifo__main__rec__expr__2__2__out;

void proc();
SC_CTOR(mainFunc) : const_dec_12("const_dec_12"), const_dec_11("const_dec_11"), add1("add1"), equ1("equ1") {

const_dec_12.out(const_dec_12_out__add1_in2);
add1.in2(const_dec_12_out__add1_in2);

add1.in1(__fifo__main__rec__expr__2__2__in__k);
add1.out(__fifo__main__rec__expr__2__2__out);
const_dec_11.out(__fifo__main__cond__2__out);
equ1.in2(__fifo__main__cond__1__in__k);
equ1.in1(__fifo__main__cond__1__in__n);
equ1.out(__fifo__main__cond__1__out);

SC_THREAD(proc);
}
};
void mainFunc::proc() {
while(true) {
n__aux = n.read();
k__aux = k.read();
wait(SC_ZERO_TIME);
while (a.nb_read(a__save__val)) {
a__save.write(a__save__val);
}

while (true) {
s__now__1 = s.read();
__fifo__main__cond__1__in__n.write(n__aux);
__fifo__main__cond__1__in__k.write(k__aux);
__fifo__main__cond__1__out__aux = __fifo__main__cond__1__out.read();
__fifo__main__cond__2__out__aux = __fifo__main__cond__2__out.read();
cond = (__fifo__main__cond__2__out__aux, __fifo__main__cond__1__out__aux);
if (cond[0]==1) {
out.write(a__save.read());
while (a__save.nb_read(a__save__copy__val)) {
out.write(a__save__copy__val);
}

while(s.nb_read(s__destroy)) {}
while(a.nb_read(a__destroy)) {}

break;


} else {
__fifo__main__rec__expr__2__2__in__k.write(k__aux);
//blob
__fifo__main__rec__expr__2__2__out__aux = __fifo__main__rec__expr__2__2__out.read();
out.write(s__now__1);
//blob
k__aux = __fifo__main__rec__expr__2__2__out__aux;

}

}

}
}

#endif

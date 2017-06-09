#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "const_dec_1.h"
#include "g.h"
#include "and_.h"
#include "add.h"
#include "equ.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > x;
sc_fifo_in<sc_lv<32> > y;

sc_fifo_out<sc_lv<32> > out;


sc_fifo<sc_lv<32> > __fifo__1;
const_dec_1 const_dec_11;
g g1;
and_ and_1;
add add1;
equ equ1;

sc_lv<32> x__aux;
sc_lv<32> y__aux;
sc_fifo<sc_lv<32> > __fifo__main__cond__1__in__x;
sc_fifo<sc_lv<32> > __fifo__main__cond__1__in__y;
sc_lv<1> __fifo__main__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__1__out;
sc_lv<1> __fifo__main__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__2__out;
sc_lv<2> cond;
sc_fifo<sc_lv<32> > __fifo__main__expr__1__in__x;
sc_fifo<sc_lv<32> > __fifo__main__expr__1__in__y;
sc_lv<32> __fifo__main__expr__1__out__aux;
sc_fifo<sc_lv<32> > __fifo__main__expr__1__out;
sc_fifo<sc_lv<32> > __fifo__main__expr__2__in__x;
sc_fifo<sc_lv<32> > __fifo__main__expr__2__in__y;
sc_lv<32> __fifo__main__expr__2__out__aux;
sc_fifo<sc_lv<32> > __fifo__main__expr__2__out;

void proc();
SC_CTOR(mainFunc) : const_dec_11("const_dec_11"), g1("g1"), and_1("and_1"), add1("add1"), equ1("equ1") {

g1.x(__fifo__main__expr__2__in__y);
and_1.in2(__fifo__1);
g1.out(__fifo__1);
and_1.in1(__fifo__main__expr__2__in__x);
and_1.out(__fifo__main__expr__2__out);
const_dec_11.out(__fifo__main__cond__2__out);
add1.in2(__fifo__main__expr__1__in__y);
add1.in1(__fifo__main__expr__1__in__x);
add1.out(__fifo__main__expr__1__out);
equ1.in2(__fifo__main__cond__1__in__y);
equ1.in1(__fifo__main__cond__1__in__x);
equ1.out(__fifo__main__cond__1__out);

SC_THREAD(proc);
}
};
void mainFunc::proc() {
while(true) {
x__aux = x.read();
y__aux = y.read();
__fifo__main__cond__1__in__x.write(x__aux);
__fifo__main__cond__1__in__y.write(y__aux);
__fifo__main__cond__1__out__aux = __fifo__main__cond__1__out.read();
__fifo__main__cond__2__out__aux = __fifo__main__cond__2__out.read();
cond = (__fifo__main__cond__1__out__aux, __fifo__main__cond__2__out__aux);
if (cond[1]==1) {
__fifo__main__expr__1__in__x.write(x__aux);
__fifo__main__expr__1__in__y.write(y__aux);
__fifo__main__expr__1__out__aux = __fifo__main__expr__1__out.read();
out.write(__fifo__main__expr__1__out__aux);


} else {
__fifo__main__expr__2__in__x.write(x__aux);
__fifo__main__expr__2__in__y.write(y__aux);
__fifo__main__expr__2__out__aux = __fifo__main__expr__2__out.read();
out.write(__fifo__main__expr__2__out__aux);

}

}
}

#endif

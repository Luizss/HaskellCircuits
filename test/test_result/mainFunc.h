#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "const_dec_51_.h"
#include "Nil_Int.h"
#include "add1_.h"
#include "Cons_Int.h"
#include "True.h"
#include "equ2_.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > __i0;
sc_fifo_in<sc_lv<32> > __i1;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<33> > Nil_Int2_out__Cons_Int2__x1;
sc_fifo<sc_lv<33> > Nil_Int1_out__Cons_Int1__x1;
sc_fifo<sc_lv<32> > const_dec_51_out__Cons_Int1__x0;

sc_fifo<sc_lv<32> > __fifo__1;
const_dec_51_ const_dec_51;
Nil_Int Nil_Int2;
add1_ add1;
Cons_Int Cons_Int2;
True True1;
Nil_Int Nil_Int1;
Cons_Int Cons_Int1;
equ2_ equ1;

sc_lv<32> __i0__aux;
sc_lv<32> __i1__aux;
sc_fifo<sc_lv<32> > __fifo__main__cond__1__in____i0;
sc_fifo<sc_lv<32> > __fifo__main__cond__1__in____i1;
sc_lv<1> __fifo__main__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__1__out;
sc_lv<1> __fifo__main__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__2__out;
sc_lv<2> cond;
sc_fifo<sc_lv<33> > __fifo__main__expr__1__out;
sc_lv<33> __fifo__main__expr__1__out__copy__val;
sc_fifo<sc_lv<32> > __fifo__main__expr__2__in____i0;
sc_fifo<sc_lv<32> > __fifo__main__expr__2__in____i1;
sc_fifo<sc_lv<33> > __fifo__main__expr__2__out;
sc_lv<33> __fifo__main__expr__2__out__copy__val;

void proc();
SC_CTOR(mainFunc) : const_dec_51("const_dec_51"), Nil_Int2("Nil_Int2"), add1("add1"), Cons_Int2("Cons_Int2"), True1("True1"), Nil_Int1("Nil_Int1"), Cons_Int1("Cons_Int1"), equ1("equ1") {

Nil_Int2.out(Nil_Int2_out__Cons_Int2__x1);
Cons_Int2._x1(Nil_Int2_out__Cons_Int2__x1);

add1.in2(__fifo__main__expr__2__in____i1);
add1.in1(__fifo__main__expr__2__in____i0);
Cons_Int2._x0(__fifo__1);
add1.out(__fifo__1);
Cons_Int2.out(__fifo__main__expr__2__out);
True1.out(__fifo__main__cond__2__out);
Nil_Int1.out(Nil_Int1_out__Cons_Int1__x1);
Cons_Int1._x1(Nil_Int1_out__Cons_Int1__x1);

const_dec_51.out(const_dec_51_out__Cons_Int1__x0);
Cons_Int1._x0(const_dec_51_out__Cons_Int1__x0);

Cons_Int1.out(__fifo__main__expr__1__out);
equ1.in2(__fifo__main__cond__1__in____i1);
equ1.in1(__fifo__main__cond__1__in____i0);
equ1.out(__fifo__main__cond__1__out);

SC_THREAD(proc);
}
};
void mainFunc::proc() {
while(true) {
__i0__aux = __i0.read();
__i1__aux = __i1.read();
__fifo__main__cond__1__in____i0.write(__i0__aux);
__fifo__main__cond__1__in____i1.write(__i1__aux);
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



} else {
__fifo__main__expr__2__in____i0.write(__i0__aux);
__fifo__main__expr__2__in____i1.write(__i1__aux);
__fifo__main__expr__2__out__copy__val = __fifo__main__expr__2__out.read();
out.write(__fifo__main__expr__2__out__copy__val);
if (__fifo__main__expr__2__out__copy__val != 0) {
while (__fifo__main__expr__2__out.nb_read(__fifo__main__expr__2__out__copy__val)) {
out.write(__fifo__main__expr__2__out__copy__val);
if (__fifo__main__expr__2__out__copy__val == 0) break;
}
}


}

}
}

#endif

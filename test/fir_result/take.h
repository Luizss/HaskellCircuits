#ifndef TAKE_H_
#define TAKE_H_

#include "systemc.h"
#include "const_dec_13_.h"
#include "const_dec_12_.h"
#include "const_dec_03_.h"
#include "sub1_.h"
#include "equ2_.h"


SC_MODULE(take) {
sc_fifo_in<sc_lv<31> > n;
sc_fifo_in<sc_lv<32> > s;
sc_fifo_in<sc_lv<32> > a;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<31> > const_dec_12_out__sub1_in2;
sc_fifo<sc_lv<31> > const_dec_01_out__equ1_in2;

const_dec_13_ const_dec_12;
const_dec_12_ const_dec_11;
const_dec_03_ const_dec_01;
sub1_ sub1;
equ2_ equ1;

sc_lv<31> n__aux;
std::vector<sc_lv<32> > a__savev;
sc_lv<32> a__savev__val;
sc_lv<32> s__now__1;
sc_fifo<sc_lv<31> > __fifo__take__cond__1__in__n;
sc_lv<1> __fifo__take__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__take__cond__1__out;
sc_lv<1> __fifo__take__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__take__cond__2__out;
sc_lv<2> cond;
sc_lv<32> s__destroy;
sc_fifo<sc_lv<31> > __fifo__take__rec__expr__2__1__in__n;
sc_lv<31> __fifo__take__rec__expr__2__1__out__aux;
sc_fifo<sc_lv<31> > __fifo__take__rec__expr__2__1__out;

void proc();
SC_CTOR(take) : const_dec_12("const_dec_12"), const_dec_11("const_dec_11"), const_dec_01("const_dec_01"), sub1("sub1"), equ1("equ1") {

const_dec_12.out(const_dec_12_out__sub1_in2);
sub1.in2(const_dec_12_out__sub1_in2);

sub1.in1(__fifo__take__rec__expr__2__1__in__n);
sub1.out(__fifo__take__rec__expr__2__1__out);
const_dec_11.out(__fifo__take__cond__2__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

equ1.in1(__fifo__take__cond__1__in__n);
equ1.out(__fifo__take__cond__1__out);

SC_THREAD(proc);
}
};
void take::proc() {
while(true) {
n__aux = n.read();
a__savev__val = a.read();
a__savev.push_back(a__savev__val);
if (a__savev__val != 0) {
while (a.nb_read(a__savev__val)) {
a__savev.push_back(a__savev__val);
if (a__savev__val == 0) break;
}
}

//blob 555
while (true) {
s__now__1 = s.read();
__fifo__take__cond__1__in__n.write(n__aux);
__fifo__take__cond__1__out__aux = __fifo__take__cond__1__out.read();
__fifo__take__cond__2__out__aux = __fifo__take__cond__2__out.read();
cond = (__fifo__take__cond__2__out__aux, __fifo__take__cond__1__out__aux);
if (cond[0]==1) {
for(std::vector<sc_lv<32> >::iterator a__savev__it_ = a__savev.begin(); a__savev__it_ != a__savev.end(); ++a__savev__it_) {
out.write(*a__savev__it_);
}

if (s__now__1 != 0) {
s__destroy = s.read();
if (s__destroy != 0) {
while(s.nb_read(s__destroy)) { if (s__destroy == 0) break; }
}
}

a__savev.clear();
break;


} else {
__fifo__take__rec__expr__2__1__in__n.write(n__aux);
__fifo__take__rec__expr__2__1__out__aux = __fifo__take__rec__expr__2__1__out.read();
out.write(s__now__1);
n__aux = __fifo__take__rec__expr__2__1__out__aux;

}

}

}
}

#endif

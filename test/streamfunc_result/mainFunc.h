#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "const_dec_12_.h"
#include "const_dec_01_.h"
#include "mrest_1_2_.h"
#include "g.h"
#include "mrest_1_1_.h"
#include "equ1_.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<32> > s;
sc_fifo_in<sc_lv<32> > l;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_dec_01_out__equ1_in2;

sc_fifo<sc_lv<32> > __fifo__1;
const_dec_12_ const_dec_11;
const_dec_01_ const_dec_01;
mrest_1_2_ mrest_1_2;
g g1;
mrest_1_1_ mrest_1_1;
equ1_ equ1;

std::vector<sc_lv<32> > s__savev;
sc_lv<32> s__savev__val;
std::vector<sc_lv<32> > l__savev;
sc_lv<32> l__savev__val;
sc_lv<32> s__now__1;
sc_fifo<sc_lv<32> > __fifo__main__cond__1__in__now__1__s;
sc_lv<1> __fifo__main__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__1__out;
sc_lv<1> __fifo__main__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__main__cond__2__out;
sc_lv<2> cond;
sc_lv<32> s__destroy;
sc_lv<32> l__destroy;
sc_fifo<sc_lv<32> > __fifo__main__rec__expr__2__1__in__s;
sc_fifo<sc_lv<32> > __fifo__main__rec__expr__2__2__in__l;
sc_lv<32> __fifo__main__rec__expr__2__1__out__savev__val;
sc_fifo<sc_lv<32> > __fifo__main__rec__expr__2__1__out;
sc_lv<32> __fifo__main__rec__expr__2__2__out__savev__val;
sc_fifo<sc_lv<32> > __fifo__main__rec__expr__2__2__out;

void proc();
SC_CTOR(mainFunc) : const_dec_11("const_dec_11"), const_dec_01("const_dec_01"), mrest_1_2("mrest_1_2"), g1("g1"), mrest_1_1("mrest_1_1"), equ1("equ1") {

mrest_1_2.in1(__fifo__main__rec__expr__2__2__in__l);
g1.s(__fifo__1);
mrest_1_2.out(__fifo__1);
g1.out(__fifo__main__rec__expr__2__2__out);
mrest_1_1.in1(__fifo__main__rec__expr__2__1__in__s);
mrest_1_1.out(__fifo__main__rec__expr__2__1__out);
const_dec_11.out(__fifo__main__cond__2__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

equ1.in1(__fifo__main__cond__1__in__now__1__s);
equ1.out(__fifo__main__cond__1__out);

SC_THREAD(proc);
}
};
void mainFunc::proc() {
while(true) {
s__savev.push_back(s.read());
while (s.nb_read(s__savev__val)) {
s__savev.push_back(s__savev__val);
}

//blob 666
l__savev.push_back(l.read());
while (l.nb_read(l__savev__val)) {
l__savev.push_back(l__savev__val);
}

//blob 555
while (true) {
s__now__1 = *(s__savev.begin() + 0);
__fifo__main__cond__1__in__now__1__s.write(s__now__1);
__fifo__main__cond__1__out__aux = __fifo__main__cond__1__out.read();
__fifo__main__cond__2__out__aux = __fifo__main__cond__2__out.read();
cond = (__fifo__main__cond__2__out__aux, __fifo__main__cond__1__out__aux);
if (cond[0]==1) {
for(std::vector<sc_lv<32> >::iterator l__savev__it_ = l__savev.begin(); l__savev__it_ != l__savev.end(); ++l__savev__it_) {
out.write(*l__savev__it_);
}

//while(s.nb_read(s__destroy)) {}
//while(l.nb_read(l__destroy)) {}

break;


} else {
for(std::vector<sc_lv<32> >::iterator s__savev__it_ = s__savev.begin(); s__savev__it_ != s__savev.end(); ++s__savev__it_) {
__fifo__main__rec__expr__2__1__in__s.write(*s__savev__it_);
}

for(std::vector<sc_lv<32> >::iterator l__savev__it_ = l__savev.begin(); l__savev__it_ != l__savev.end(); ++l__savev__it_) {
__fifo__main__rec__expr__2__2__in__l.write(*l__savev__it_);
}

s__savev.clear();
s__savev.push_back(__fifo__main__rec__expr__2__1__out.read());
while (__fifo__main__rec__expr__2__1__out.nb_read(__fifo__main__rec__expr__2__1__out__savev__val)) {
s__savev.push_back(__fifo__main__rec__expr__2__1__out__savev__val);
}

l__savev.clear();
l__savev.push_back(__fifo__main__rec__expr__2__2__out.read());
while (__fifo__main__rec__expr__2__2__out.nb_read(__fifo__main__rec__expr__2__2__out__savev__val)) {
l__savev.push_back(__fifo__main__rec__expr__2__2__out__savev__val);
}


}

}

}
}

#endif

#ifndef H__0G_H_
#define H__0G_H_

#include "systemc.h"
#include "const_dec_12_.h"
#include "const_dec_01_.h"
#include "k__1g.h"
#include "equ1_.h"


SC_MODULE(h__0g) {
sc_fifo_in<sc_lv<32> > s;
sc_fifo_in<sc_lv<32> > a;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_dec_03_out__equ3_in2;
sc_fifo<sc_lv<32> > const_dec_02_out__equ2_in2;
sc_fifo<sc_lv<32> > const_dec_01_out__equ1_in2;

const_dec_12_ const_dec_11;
const_dec_01_ const_dec_03;
const_dec_01_ const_dec_02;
const_dec_01_ const_dec_01;
k__1g k__1g1;
equ1_ equ3;
equ1_ equ2;
equ1_ equ1;

sc_lv<32> s__now__2;
sc_lv<32> s__now__3;
std::vector<sc_lv<32> > a__savev;
sc_lv<32> a__savev__val;
sc_lv<32> s__now__1;
sc_fifo<sc_lv<32> > __fifo__h__0g__cond__1__in__now__1__s;
sc_fifo<sc_lv<32> > __fifo__h__0g__cond__2__in__now__2__s;
sc_fifo<sc_lv<32> > __fifo__h__0g__cond__3__in__now__3__s;
sc_lv<1> __fifo__h__0g__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__h__0g__cond__1__out;
sc_lv<1> __fifo__h__0g__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__h__0g__cond__2__out;
sc_lv<1> __fifo__h__0g__cond__3__out__aux;
sc_fifo<sc_lv<1> > __fifo__h__0g__cond__3__out;
sc_lv<1> __fifo__h__0g__cond__4__out__aux;
sc_fifo<sc_lv<1> > __fifo__h__0g__cond__4__out;
sc_lv<4> cond;
sc_lv<32> s__destroy;
sc_fifo<sc_lv<32> > __fifo__h__0g__rec__expr__4__2__consR__1__in__now__1__s;
sc_fifo<sc_lv<32> > __fifo__h__0g__rec__expr__4__2__consR__1__in__now__2__s;
sc_fifo<sc_lv<32> > __fifo__h__0g__rec__expr__4__2__consR__1__in__now__3__s;
sc_lv<32> __fifo__h__0g__rec__expr__4__2__consR__1__out__aux;
sc_fifo<sc_lv<32> > __fifo__h__0g__rec__expr__4__2__consR__1__out;

void proc();
SC_CTOR(h__0g) : const_dec_11("const_dec_11"), const_dec_03("const_dec_03"), const_dec_02("const_dec_02"), const_dec_01("const_dec_01"), k__1g1("k__1g1"), equ3("equ3"), equ2("equ2"), equ1("equ1") {

k__1g1.z(__fifo__h__0g__rec__expr__4__2__consR__1__in__now__3__s);
k__1g1.y(__fifo__h__0g__rec__expr__4__2__consR__1__in__now__2__s);
k__1g1.x(__fifo__h__0g__rec__expr__4__2__consR__1__in__now__1__s);
k__1g1.out(__fifo__h__0g__rec__expr__4__2__consR__1__out);
const_dec_11.out(__fifo__h__0g__cond__4__out);
const_dec_03.out(const_dec_03_out__equ3_in2);
equ3.in2(const_dec_03_out__equ3_in2);

equ3.in1(__fifo__h__0g__cond__3__in__now__3__s);
equ3.out(__fifo__h__0g__cond__3__out);
const_dec_02.out(const_dec_02_out__equ2_in2);
equ2.in2(const_dec_02_out__equ2_in2);

equ2.in1(__fifo__h__0g__cond__2__in__now__2__s);
equ2.out(__fifo__h__0g__cond__2__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

equ1.in1(__fifo__h__0g__cond__1__in__now__1__s);
equ1.out(__fifo__h__0g__cond__1__out);

SC_THREAD(proc);
}
};
void h__0g::proc() {
while(true) {
s__now__2 = s.read();
if (s__now__2 == 0) {
s__now__3 = 0;
} else {

s__now__3 = s.read();}
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
s__now__1 = s__now__2;
s__now__2 = s__now__3;
s__now__3 = s.read();
__fifo__h__0g__cond__1__in__now__1__s.write(s__now__1);
__fifo__h__0g__cond__2__in__now__2__s.write(s__now__2);
__fifo__h__0g__cond__3__in__now__3__s.write(s__now__3);
__fifo__h__0g__cond__1__out__aux = __fifo__h__0g__cond__1__out.read();
__fifo__h__0g__cond__2__out__aux = __fifo__h__0g__cond__2__out.read();
__fifo__h__0g__cond__3__out__aux = __fifo__h__0g__cond__3__out.read();
__fifo__h__0g__cond__4__out__aux = __fifo__h__0g__cond__4__out.read();
cond = (__fifo__h__0g__cond__4__out__aux, __fifo__h__0g__cond__3__out__aux, __fifo__h__0g__cond__2__out__aux, __fifo__h__0g__cond__1__out__aux);
if (cond[0]==1) {
for(std::vector<sc_lv<32> >::iterator a__savev__it_ = a__savev.begin(); a__savev__it_ != a__savev.end(); ++a__savev__it_) {
out.write(*a__savev__it_);
}

if (s__now__1 != 0 && s__now__2 != 0 && s__now__3 != 0) {
s__destroy = s.read();
if (s__destroy != 0) {
while(s.nb_read(s__destroy)) { if (s__destroy == 0) break; }
}
}

a__savev.clear();
break;


} else if (cond[1]==1) {
for(std::vector<sc_lv<32> >::iterator a__savev__it_ = a__savev.begin(); a__savev__it_ != a__savev.end(); ++a__savev__it_) {
out.write(*a__savev__it_);
}

if (s__now__1 != 0 && s__now__2 != 0 && s__now__3 != 0) {
s__destroy = s.read();
if (s__destroy != 0) {
while(s.nb_read(s__destroy)) { if (s__destroy == 0) break; }
}
}

a__savev.clear();
break;


} else if (cond[2]==1) {
for(std::vector<sc_lv<32> >::iterator a__savev__it_ = a__savev.begin(); a__savev__it_ != a__savev.end(); ++a__savev__it_) {
out.write(*a__savev__it_);
}

if (s__now__1 != 0 && s__now__2 != 0 && s__now__3 != 0) {
s__destroy = s.read();
if (s__destroy != 0) {
while(s.nb_read(s__destroy)) { if (s__destroy == 0) break; }
}
}

a__savev.clear();
break;


} else {
__fifo__h__0g__rec__expr__4__2__consR__1__in__now__1__s.write(s__now__1);
__fifo__h__0g__rec__expr__4__2__consR__1__in__now__2__s.write(s__now__2);
__fifo__h__0g__rec__expr__4__2__consR__1__in__now__3__s.write(s__now__3);
__fifo__h__0g__rec__expr__4__2__consR__1__out__aux = __fifo__h__0g__rec__expr__4__2__consR__1__out.read();
out.write(__fifo__h__0g__rec__expr__4__2__consR__1__out__aux);

}

}

}
}

#endif

#ifndef ZIPWITH_H_
#define ZIPWITH_H_

#include "systemc.h"
#include "const_dec_12_.h"
#include "const_dec_02_.h"
#include "sli_1_31_2_.h"
#include "f.h"
#include "cat1_.h"
#include "sli_0_0_1_.h"
#include "equ1_.h"


SC_MODULE(zipWith) {
sc_fifo_in<sc_lv<32> > s1;
sc_fifo_in<sc_lv<32> > s2;
sc_fifo_in<sc_lv<32> > a;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<1> > const_dec_12_out__cat1_in2;
sc_fifo<sc_lv<1> > const_dec_02_out__equ2_in2;
sc_fifo<sc_lv<1> > const_dec_01_out__equ1_in2;

sc_fifo<sc_lv<31> > __fifo__5;
sc_fifo<sc_lv<31> > __fifo__4;
sc_fifo<sc_lv<31> > __fifo__3;
sc_fifo<sc_lv<1> > __fifo__2;
sc_fifo<sc_lv<1> > __fifo__1;
const_dec_12_ const_dec_12;
const_dec_12_ const_dec_11;
const_dec_02_ const_dec_02;
const_dec_02_ const_dec_01;
sli_1_31_2_ sli_1_31_2;
sli_1_31_2_ sli_1_31_1;
f f1;
cat1_ cat1;
sli_0_0_1_ sli_0_0_2;
equ1_ equ2;
sli_0_0_1_ sli_0_0_1;
equ1_ equ1;

std::vector<sc_lv<32> > a__savev;
sc_lv<32> a__savev__val;
sc_lv<32> s1__now__1;
sc_lv<32> s2__now__1;
sc_fifo<sc_lv<32> > __fifo__zipWith__cond__1__in__now__1__s1;
sc_fifo<sc_lv<32> > __fifo__zipWith__cond__2__in__now__1__s2;
sc_lv<1> __fifo__zipWith__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__zipWith__cond__1__out;
sc_lv<1> __fifo__zipWith__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__zipWith__cond__2__out;
sc_lv<1> __fifo__zipWith__cond__3__out__aux;
sc_fifo<sc_lv<1> > __fifo__zipWith__cond__3__out;
sc_lv<3> cond;
sc_lv<32> s1__destroy;
sc_lv<32> s2__destroy;
sc_fifo<sc_lv<32> > __fifo__zipWith__rec__expr__3__3__consR__1__in__now__1__s1;
sc_fifo<sc_lv<32> > __fifo__zipWith__rec__expr__3__3__consR__1__in__now__1__s2;
sc_lv<32> __fifo__zipWith__rec__expr__3__3__consR__1__out__aux;
sc_fifo<sc_lv<32> > __fifo__zipWith__rec__expr__3__3__consR__1__out;

void proc();
SC_CTOR(zipWith) : const_dec_12("const_dec_12"), const_dec_11("const_dec_11"), const_dec_02("const_dec_02"), const_dec_01("const_dec_01"), sli_1_31_2("sli_1_31_2"), sli_1_31_1("sli_1_31_1"), f1("f1"), cat1("cat1"), sli_0_0_2("sli_0_0_2"), equ2("equ2"), sli_0_0_1("sli_0_0_1"), equ1("equ1") {

const_dec_12.out(const_dec_12_out__cat1_in2);
cat1.in2(const_dec_12_out__cat1_in2);

sli_1_31_2.in1(__fifo__zipWith__rec__expr__3__3__consR__1__in__now__1__s2);
f1.y(__fifo__5);
sli_1_31_2.out(__fifo__5);
sli_1_31_1.in1(__fifo__zipWith__rec__expr__3__3__consR__1__in__now__1__s1);
f1.x(__fifo__4);
sli_1_31_1.out(__fifo__4);
cat1.in1(__fifo__3);
f1.out(__fifo__3);
cat1.out(__fifo__zipWith__rec__expr__3__3__consR__1__out);
const_dec_11.out(__fifo__zipWith__cond__3__out);
const_dec_02.out(const_dec_02_out__equ2_in2);
equ2.in2(const_dec_02_out__equ2_in2);

sli_0_0_2.in1(__fifo__zipWith__cond__2__in__now__1__s2);
equ2.in1(__fifo__2);
sli_0_0_2.out(__fifo__2);
equ2.out(__fifo__zipWith__cond__2__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

sli_0_0_1.in1(__fifo__zipWith__cond__1__in__now__1__s1);
equ1.in1(__fifo__1);
sli_0_0_1.out(__fifo__1);
equ1.out(__fifo__zipWith__cond__1__out);

SC_THREAD(proc);
}
};
void zipWith::proc() {
while(true) {
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
s1__now__1 = s1.read();
s2__now__1 = s2.read();
__fifo__zipWith__cond__1__in__now__1__s1.write(s1__now__1);
__fifo__zipWith__cond__2__in__now__1__s2.write(s2__now__1);
__fifo__zipWith__cond__1__out__aux = __fifo__zipWith__cond__1__out.read();
__fifo__zipWith__cond__2__out__aux = __fifo__zipWith__cond__2__out.read();
__fifo__zipWith__cond__3__out__aux = __fifo__zipWith__cond__3__out.read();
cond = (__fifo__zipWith__cond__3__out__aux, __fifo__zipWith__cond__2__out__aux, __fifo__zipWith__cond__1__out__aux);
if (cond[0]==1) {
for(std::vector<sc_lv<32> >::iterator a__savev__it_ = a__savev.begin(); a__savev__it_ != a__savev.end(); ++a__savev__it_) {
out.write(*a__savev__it_);
}

if (s1__now__1 != 0) {
s1__destroy = s1.read();
if (s1__destroy != 0) {
while(s1.nb_read(s1__destroy)) { if (s1__destroy == 0) break; }
}
}

if (s2__now__1 != 0) {
s2__destroy = s2.read();
if (s2__destroy != 0) {
while(s2.nb_read(s2__destroy)) { if (s2__destroy == 0) break; }
}
}

a__savev.clear();
break;


} else if (cond[1]==1) {
for(std::vector<sc_lv<32> >::iterator a__savev__it_ = a__savev.begin(); a__savev__it_ != a__savev.end(); ++a__savev__it_) {
out.write(*a__savev__it_);
}

if (s1__now__1 != 0) {
s1__destroy = s1.read();
if (s1__destroy != 0) {
while(s1.nb_read(s1__destroy)) { if (s1__destroy == 0) break; }
}
}

if (s2__now__1 != 0) {
s2__destroy = s2.read();
if (s2__destroy != 0) {
while(s2.nb_read(s2__destroy)) { if (s2__destroy == 0) break; }
}
}

a__savev.clear();
break;


} else {
__fifo__zipWith__rec__expr__3__3__consR__1__in__now__1__s1.write(s1__now__1);
__fifo__zipWith__rec__expr__3__3__consR__1__in__now__1__s2.write(s2__now__1);
__fifo__zipWith__rec__expr__3__3__consR__1__out__aux = __fifo__zipWith__rec__expr__3__3__consR__1__out.read();
out.write(__fifo__zipWith__rec__expr__3__3__consR__1__out__aux);

}

}

}
}

#endif

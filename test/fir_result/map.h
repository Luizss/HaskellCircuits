#ifndef MAP_H_
#define MAP_H_

#include "systemc.h"
#include "const_dec_12_.h"
#include "const_dec_02_.h"
#include "sli_1_31_2_.h"
#include "y.h"
#include "cat1_.h"
#include "sli_0_0_1_.h"
#include "equ1_.h"


SC_MODULE(map) {
sc_fifo_in<sc_lv<32> > bs;
sc_fifo_in<sc_lv<32> > xs;
sc_fifo_in<sc_lv<32> > a;
sc_fifo_in<sc_lv<32> > c;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<1> > const_dec_12_out__cat1_in2;
sc_fifo<sc_lv<1> > const_dec_01_out__equ1_in2;

sc_fifo<sc_lv<31> > __fifo__3;
sc_fifo<sc_lv<31> > __fifo__2;
sc_fifo<sc_lv<1> > __fifo__1;
const_dec_12_ const_dec_12;
const_dec_12_ const_dec_11;
const_dec_02_ const_dec_01;
sli_1_31_2_ sli_1_31_1;
y y1;
cat1_ cat1;
sli_0_0_1_ sli_0_0_1;
equ1_ equ1;

std::vector<sc_lv<32> > bs__savev;
sc_lv<32> bs__savev__val;
std::vector<sc_lv<32> > xs__savev;
sc_lv<32> xs__savev__val;
std::vector<sc_lv<32> > a__savev;
sc_lv<32> a__savev__val;
sc_lv<32> c__now__1;
sc_fifo<sc_lv<32> > __fifo__map__cond__1__in__now__1__c;
sc_lv<1> __fifo__map__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__map__cond__1__out;
sc_lv<1> __fifo__map__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__map__cond__2__out;
sc_lv<2> cond;
sc_lv<32> c__destroy;
sc_fifo<sc_lv<32> > __fifo__map__rec__expr__2__3__consR__1__in__bs;
sc_fifo<sc_lv<32> > __fifo__map__rec__expr__2__3__consR__1__in__xs;
sc_fifo<sc_lv<32> > __fifo__map__rec__expr__2__3__consR__1__in__a;
sc_fifo<sc_lv<32> > __fifo__map__rec__expr__2__3__consR__1__in__now__1__c;
sc_lv<32> __fifo__map__rec__expr__2__3__consR__1__out__aux;
sc_fifo<sc_lv<32> > __fifo__map__rec__expr__2__3__consR__1__out;

void proc();
SC_CTOR(map) : const_dec_12("const_dec_12"), const_dec_11("const_dec_11"), const_dec_01("const_dec_01"), sli_1_31_1("sli_1_31_1"), y1("y1"), cat1("cat1"), sli_0_0_1("sli_0_0_1"), equ1("equ1") {

const_dec_12.out(const_dec_12_out__cat1_in2);
cat1.in2(const_dec_12_out__cat1_in2);

sli_1_31_1.in1(__fifo__map__rec__expr__2__3__consR__1__in__now__1__c);
y1.n(__fifo__3);
sli_1_31_1.out(__fifo__3);
y1.a(__fifo__map__rec__expr__2__3__consR__1__in__a);
y1.xs(__fifo__map__rec__expr__2__3__consR__1__in__xs);
y1.bs(__fifo__map__rec__expr__2__3__consR__1__in__bs);
cat1.in1(__fifo__2);
y1.out(__fifo__2);
cat1.out(__fifo__map__rec__expr__2__3__consR__1__out);
const_dec_11.out(__fifo__map__cond__2__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

sli_0_0_1.in1(__fifo__map__cond__1__in__now__1__c);
equ1.in1(__fifo__1);
sli_0_0_1.out(__fifo__1);
equ1.out(__fifo__map__cond__1__out);

SC_THREAD(proc);
}
};
void map::proc() {
while(true) {
bs__savev__val = bs.read();
bs__savev.push_back(bs__savev__val);
if (bs__savev__val != 0) {
while (bs.nb_read(bs__savev__val)) {
bs__savev.push_back(bs__savev__val);
if (bs__savev__val == 0) break;
}
}

//blob 555
xs__savev__val = xs.read();
xs__savev.push_back(xs__savev__val);
if (xs__savev__val != 0) {
while (xs.nb_read(xs__savev__val)) {
xs__savev.push_back(xs__savev__val);
if (xs__savev__val == 0) break;
}
}

//blob 555
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
c__now__1 = c.read();
__fifo__map__cond__1__in__now__1__c.write(c__now__1);
__fifo__map__cond__1__out__aux = __fifo__map__cond__1__out.read();
__fifo__map__cond__2__out__aux = __fifo__map__cond__2__out.read();
cond = (__fifo__map__cond__2__out__aux, __fifo__map__cond__1__out__aux);
if (cond[0]==1) {
for(std::vector<sc_lv<32> >::iterator a__savev__it_ = a__savev.begin(); a__savev__it_ != a__savev.end(); ++a__savev__it_) {
out.write(*a__savev__it_);
}

bs__savev.clear();
xs__savev.clear();
a__savev.clear();
if (c__now__1 != 0) {
c__destroy = c.read();
if (c__destroy != 0) {
while(c.nb_read(c__destroy)) { if (c__destroy == 0) break; }
}
}

break;


} else {
for(std::vector<sc_lv<32> >::iterator bs__savev__it_ = bs__savev.begin(); bs__savev__it_ != bs__savev.end(); ++bs__savev__it_) {
__fifo__map__rec__expr__2__3__consR__1__in__bs.write(*bs__savev__it_);
}

for(std::vector<sc_lv<32> >::iterator xs__savev__it_ = xs__savev.begin(); xs__savev__it_ != xs__savev.end(); ++xs__savev__it_) {
__fifo__map__rec__expr__2__3__consR__1__in__xs.write(*xs__savev__it_);
}

for(std::vector<sc_lv<32> >::iterator a__savev__it_ = a__savev.begin(); a__savev__it_ != a__savev.end(); ++a__savev__it_) {
__fifo__map__rec__expr__2__3__consR__1__in__a.write(*a__savev__it_);
}

__fifo__map__rec__expr__2__3__consR__1__in__now__1__c.write(c__now__1);
__fifo__map__rec__expr__2__3__consR__1__out__aux = __fifo__map__rec__expr__2__3__consR__1__out.read();
out.write(__fifo__map__rec__expr__2__3__consR__1__out__aux);

}

}

}
}

#endif

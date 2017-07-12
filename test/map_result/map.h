#ifndef MAP_H_
#define MAP_H_

#include "systemc.h"
#include "const_dec_13_.h"
#include "const_dec_03_.h"
#include "sli_1_31_2_.h"
#include "f.h"
#include "cat1_.h"
#include "sli_0_0_1_.h"
#include "equ1_.h"


SC_MODULE(map) {
sc_fifo_in<sc_lv<32> > s;
sc_fifo_in<sc_lv<32> > a;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<1> > const_dec_12_out__cat1_in2;
sc_fifo<sc_lv<1> > const_dec_01_out__equ1_in2;

sc_fifo<sc_lv<31> > __fifo__3;
sc_fifo<sc_lv<31> > __fifo__2;
sc_fifo<sc_lv<1> > __fifo__1;
const_dec_13_ const_dec_12;
const_dec_13_ const_dec_11;
const_dec_03_ const_dec_01;
sli_1_31_2_ sli_1_31_1;
f f1;
cat1_ cat1;
sli_0_0_1_ sli_0_0_1;
equ1_ equ1;

std::vector<sc_lv<32> > a__savev;
sc_lv<32> a__savev__val;
sc_lv<32> s__now__1;
sc_fifo<sc_lv<32> > __fifo__map__cond__1__in__now__1__s;
sc_lv<1> __fifo__map__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__map__cond__1__out;
sc_lv<1> __fifo__map__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__map__cond__2__out;
sc_lv<2> cond;
sc_lv<32> s__destroy;
sc_fifo<sc_lv<32> > __fifo__map__rec__expr__2__2__consR__1__in__now__1__s;
sc_lv<32> __fifo__map__rec__expr__2__2__consR__1__out__aux;
sc_fifo<sc_lv<32> > __fifo__map__rec__expr__2__2__consR__1__out;

void proc();
SC_CTOR(map) : const_dec_12("const_dec_12"), const_dec_11("const_dec_11"), const_dec_01("const_dec_01"), sli_1_31_1("sli_1_31_1"), f1("f1"), cat1("cat1"), sli_0_0_1("sli_0_0_1"), equ1("equ1") {

const_dec_12.out(const_dec_12_out__cat1_in2);
cat1.in2(const_dec_12_out__cat1_in2);

sli_1_31_1.in1(__fifo__map__rec__expr__2__2__consR__1__in__now__1__s);
f1.x(__fifo__3);
sli_1_31_1.out(__fifo__3);
cat1.in1(__fifo__2);
f1.out(__fifo__2);
cat1.out(__fifo__map__rec__expr__2__2__consR__1__out);
const_dec_11.out(__fifo__map__cond__2__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

sli_0_0_1.in1(__fifo__map__cond__1__in__now__1__s);
equ1.in1(__fifo__1);
sli_0_0_1.out(__fifo__1);
equ1.out(__fifo__map__cond__1__out);

SC_THREAD(proc);
}
};
void map::proc() {
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
s__now__1 = s.read();
__fifo__map__cond__1__in__now__1__s.write(s__now__1);
__fifo__map__cond__1__out__aux = __fifo__map__cond__1__out.read();
__fifo__map__cond__2__out__aux = __fifo__map__cond__2__out.read();
cond = (__fifo__map__cond__2__out__aux, __fifo__map__cond__1__out__aux);
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
__fifo__map__rec__expr__2__2__consR__1__in__now__1__s.write(s__now__1);
__fifo__map__rec__expr__2__2__consR__1__out__aux = __fifo__map__rec__expr__2__2__consR__1__out.read();
out.write(__fifo__map__rec__expr__2__2__consR__1__out__aux);

}

}

}
}

#endif

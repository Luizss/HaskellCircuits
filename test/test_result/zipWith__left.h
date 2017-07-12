#ifndef ZIPWITH__LEFT_H_
#define ZIPWITH__LEFT_H_

#include "systemc.h"
#include "const_bin_11_.h"
#include "sli_1_32_21_.h"
#include "sli_1_32_20_.h"
#include "g.h"
#include "cat3_.h"
#include "sli_0_0_19_.h"
#include "sli_0_0_18_.h"
#include "and_1_.h"
#include "sli_0_0_17_.h"
#include "not_1_.h"
#include "sli_0_0_16_.h"


SC_MODULE(zipWith__left) {
sc_fifo_in<sc_lv<33> > __i0;
sc_fifo_in<sc_lv<33> > __i1;
sc_fifo_in<sc_lv<33> > __acc;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<1> > const_bin_11_out__cat1_in2;

sc_fifo<sc_lv<32> > __fifo__7;
sc_fifo<sc_lv<32> > __fifo__6;
sc_fifo<sc_lv<32> > __fifo__5;
sc_fifo<sc_lv<1> > __fifo__4;
sc_fifo<sc_lv<1> > __fifo__3;
sc_fifo<sc_lv<1> > __fifo__2;
sc_fifo<sc_lv<1> > __fifo__1;
const_bin_11_ const_bin_11;
sli_1_32_21_ sli_1_32_2;
sli_1_32_20_ sli_1_32_1;
g g1;
cat3_ cat1;
sli_0_0_19_ sli_0_0_4;
sli_0_0_18_ sli_0_0_3;
and_1_ and_1;
sli_0_0_17_ sli_0_0_2;
not_1_ not_2;
sli_0_0_16_ sli_0_0_1;
not_1_ not_1;

std::vector<sc_lv<33> > __acc__savev;
sc_lv<33> __acc__savev__val;
sc_lv<33> __i0__now__1;
sc_lv<33> __i1__now__1;
sc_fifo<sc_lv<33> > __fifo__zipWith__left__cond__1__in__now__1____i0;
sc_fifo<sc_lv<33> > __fifo__zipWith__left__cond__2__in__now__1____i1;
sc_fifo<sc_lv<33> > __fifo__zipWith__left__cond__3__in__now__1____i0;
sc_fifo<sc_lv<33> > __fifo__zipWith__left__cond__3__in__now__1____i1;
sc_lv<1> __fifo__zipWith__left__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__zipWith__left__cond__1__out;
sc_lv<1> __fifo__zipWith__left__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__zipWith__left__cond__2__out;
sc_lv<1> __fifo__zipWith__left__cond__3__out__aux;
sc_fifo<sc_lv<1> > __fifo__zipWith__left__cond__3__out;
sc_lv<3> cond;
sc_lv<33> __i0__destroy;
sc_lv<33> __i1__destroy;
sc_fifo<sc_lv<33> > __fifo__zipWith__left__rec__expr__3__3__consR__1__in__now__1____i0;
sc_fifo<sc_lv<33> > __fifo__zipWith__left__rec__expr__3__3__consR__1__in__now__1____i1;
sc_lv<33> __fifo__zipWith__left__rec__expr__3__3__consR__1__out__aux;
sc_fifo<sc_lv<33> > __fifo__zipWith__left__rec__expr__3__3__consR__1__out;

void proc();
SC_CTOR(zipWith__left) : const_bin_11("const_bin_11"), sli_1_32_2("sli_1_32_2"), sli_1_32_1("sli_1_32_1"), g1("g1"), cat1("cat1"), sli_0_0_4("sli_0_0_4"), sli_0_0_3("sli_0_0_3"), and_1("and_1"), sli_0_0_2("sli_0_0_2"), not_2("not_2"), sli_0_0_1("sli_0_0_1"), not_1("not_1") {

const_bin_11.out(const_bin_11_out__cat1_in2);
cat1.in2(const_bin_11_out__cat1_in2);

sli_1_32_2.in1(__fifo__zipWith__left__rec__expr__3__3__consR__1__in__now__1____i1);
g1.__i1(__fifo__7);
sli_1_32_2.out(__fifo__7);
sli_1_32_1.in1(__fifo__zipWith__left__rec__expr__3__3__consR__1__in__now__1____i0);
g1.__i0(__fifo__6);
sli_1_32_1.out(__fifo__6);
cat1.in1(__fifo__5);
g1.out(__fifo__5);
cat1.out(__fifo__zipWith__left__rec__expr__3__3__consR__1__out);
sli_0_0_4.in1(__fifo__zipWith__left__cond__3__in__now__1____i0);
and_1.in2(__fifo__4);
sli_0_0_4.out(__fifo__4);
sli_0_0_3.in1(__fifo__zipWith__left__cond__3__in__now__1____i1);
and_1.in1(__fifo__3);
sli_0_0_3.out(__fifo__3);
and_1.out(__fifo__zipWith__left__cond__3__out);
sli_0_0_2.in1(__fifo__zipWith__left__cond__2__in__now__1____i1);
not_2.in1(__fifo__2);
sli_0_0_2.out(__fifo__2);
not_2.out(__fifo__zipWith__left__cond__2__out);
sli_0_0_1.in1(__fifo__zipWith__left__cond__1__in__now__1____i0);
not_1.in1(__fifo__1);
sli_0_0_1.out(__fifo__1);
not_1.out(__fifo__zipWith__left__cond__1__out);

SC_THREAD(proc);
}
};
void zipWith__left::proc() {
while(true) {
__acc__savev__val = __acc.read();
__acc__savev.push_back(__acc__savev__val);
if (__acc__savev__val != 0) {
while (__acc.nb_read(__acc__savev__val)) {
__acc__savev.push_back(__acc__savev__val);
if (__acc__savev__val == 0) break;
}
}

//blob 555
while (true) {
__i0__now__1 = __i0.read();
__i1__now__1 = __i1.read();
__fifo__zipWith__left__cond__1__in__now__1____i0.write(__i0__now__1);
__fifo__zipWith__left__cond__2__in__now__1____i1.write(__i1__now__1);
__fifo__zipWith__left__cond__3__in__now__1____i0.write(__i0__now__1);
__fifo__zipWith__left__cond__3__in__now__1____i1.write(__i1__now__1);
__fifo__zipWith__left__cond__1__out__aux = __fifo__zipWith__left__cond__1__out.read();
__fifo__zipWith__left__cond__2__out__aux = __fifo__zipWith__left__cond__2__out.read();
__fifo__zipWith__left__cond__3__out__aux = __fifo__zipWith__left__cond__3__out.read();
cond = (__fifo__zipWith__left__cond__3__out__aux, __fifo__zipWith__left__cond__2__out__aux, __fifo__zipWith__left__cond__1__out__aux);
if (cond[0]==1) {
for(std::vector<sc_lv<33> >::iterator __acc__savev__it_ = __acc__savev.begin(); __acc__savev__it_ != __acc__savev.end(); ++__acc__savev__it_) {
out.write(*__acc__savev__it_);
}

if (__i0__now__1 != 0) {
__i0__destroy = __i0.read();
if (__i0__destroy != 0) {
while(__i0.nb_read(__i0__destroy)) { if (__i0__destroy == 0) break; }
}
}

if (__i1__now__1 != 0) {
__i1__destroy = __i1.read();
if (__i1__destroy != 0) {
while(__i1.nb_read(__i1__destroy)) { if (__i1__destroy == 0) break; }
}
}

__acc__savev.clear();
break;


} else if (cond[1]==1) {
for(std::vector<sc_lv<33> >::iterator __acc__savev__it_ = __acc__savev.begin(); __acc__savev__it_ != __acc__savev.end(); ++__acc__savev__it_) {
out.write(*__acc__savev__it_);
}

if (__i0__now__1 != 0) {
__i0__destroy = __i0.read();
if (__i0__destroy != 0) {
while(__i0.nb_read(__i0__destroy)) { if (__i0__destroy == 0) break; }
}
}

if (__i1__now__1 != 0) {
__i1__destroy = __i1.read();
if (__i1__destroy != 0) {
while(__i1.nb_read(__i1__destroy)) { if (__i1__destroy == 0) break; }
}
}

__acc__savev.clear();
break;


} else {
__fifo__zipWith__left__rec__expr__3__3__consR__1__in__now__1____i0.write(__i0__now__1);
__fifo__zipWith__left__rec__expr__3__3__consR__1__in__now__1____i1.write(__i1__now__1);
__fifo__zipWith__left__rec__expr__3__3__consR__1__out__aux = __fifo__zipWith__left__rec__expr__3__3__consR__1__out.read();
out.write(__fifo__zipWith__left__rec__expr__3__3__consR__1__out__aux);

}

}

}
}

#endif

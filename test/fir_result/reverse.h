#ifndef REVERSE_H_
#define REVERSE_H_

#include "systemc.h"
#include "const_dec_12_.h"
#include "const_dec_04_.h"
#include "equ3_.h"


SC_MODULE(reverse) {
sc_fifo_in<sc_lv<32> > s;
sc_fifo_in<sc_lv<32> > a;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_dec_01_out__equ1_in2;

const_dec_12_ const_dec_11;
const_dec_04_ const_dec_01;
equ3_ equ1;

std::vector<sc_lv<32> > a__savev;
sc_lv<32> a__savev__val;
sc_lv<32> s__now__1;
sc_fifo<sc_lv<32> > __fifo__reverse__cond__1__in__now__1__s;
sc_lv<1> __fifo__reverse__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__reverse__cond__1__out;
sc_lv<1> __fifo__reverse__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__reverse__cond__2__out;
sc_lv<2> cond;
sc_lv<32> s__destroy;
std::vector<sc_lv<32> >::iterator a__savev__it;

void proc();
SC_CTOR(reverse) : const_dec_11("const_dec_11"), const_dec_01("const_dec_01"), equ1("equ1") {

const_dec_11.out(__fifo__reverse__cond__2__out);
const_dec_01.out(const_dec_01_out__equ1_in2);
equ1.in2(const_dec_01_out__equ1_in2);

equ1.in1(__fifo__reverse__cond__1__in__now__1__s);
equ1.out(__fifo__reverse__cond__1__out);

SC_THREAD(proc);
}
};
void reverse::proc() {
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
__fifo__reverse__cond__1__in__now__1__s.write(s__now__1);
__fifo__reverse__cond__1__out__aux = __fifo__reverse__cond__1__out.read();
__fifo__reverse__cond__2__out__aux = __fifo__reverse__cond__2__out.read();
cond = (__fifo__reverse__cond__2__out__aux, __fifo__reverse__cond__1__out__aux);
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
a__savev__it = a__savev.begin();
a__savev.insert(a__savev__it,s__now__1);


}

}

}
}

#endif

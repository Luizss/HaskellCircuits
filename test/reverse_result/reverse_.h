#ifndef REVERSE__H_
#define REVERSE__H_

#include "systemc.h"
#include "const_bin_11_.h"
#include "sli_1_32_8_.h"
#include "cat3_.h"
#include "sli_0_0_7_.h"
#include "sli_0_0_6_.h"
#include "not_1_.h"


SC_MODULE(reverse_) {
sc_fifo_in<sc_lv<33> > __i0;
sc_fifo_in<sc_lv<33> > __i1;

sc_fifo_out<sc_lv<33> > out;

sc_fifo<sc_lv<1> > const_bin_11_out__cat1_in2;

sc_fifo<sc_lv<32> > __fifo__2;
sc_fifo<sc_lv<1> > __fifo__1;
const_bin_11_ const_bin_11;
sli_1_32_8_ sli_1_32_1;
cat3_ cat1;
sli_0_0_7_ sli_0_0_2;
sli_0_0_6_ sli_0_0_1;
not_1_ not_1;

std::vector<sc_lv<33> > __i1__savev;
sc_lv<33> __i1__savev__val;
sc_lv<33> __i0__now__1;
sc_fifo<sc_lv<33> > __fifo__reverse___cond__1__in__now__1____i0;
sc_fifo<sc_lv<33> > __fifo__reverse___cond__2__in__now__1____i0;
sc_lv<1> __fifo__reverse___cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__reverse___cond__1__out;
sc_lv<1> __fifo__reverse___cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__reverse___cond__2__out;
sc_lv<2> cond;
sc_lv<33> __i0__destroy;
sc_fifo<sc_lv<33> > __fifo__reverse___rec__expr__2__2__consR__1__in__now__1____i0;
sc_lv<33> __fifo__reverse___rec__expr__2__2__consR__1__out__aux;
sc_fifo<sc_lv<33> > __fifo__reverse___rec__expr__2__2__consR__1__out;
std::vector<sc_lv<33> >::iterator __i1__savev__it;

void proc();
SC_CTOR(reverse_) : const_bin_11("const_bin_11"), sli_1_32_1("sli_1_32_1"), cat1("cat1"), sli_0_0_2("sli_0_0_2"), sli_0_0_1("sli_0_0_1"), not_1("not_1") {

const_bin_11.out(const_bin_11_out__cat1_in2);
cat1.in2(const_bin_11_out__cat1_in2);

sli_1_32_1.in1(__fifo__reverse___rec__expr__2__2__consR__1__in__now__1____i0);
cat1.in1(__fifo__2);
sli_1_32_1.out(__fifo__2);
cat1.out(__fifo__reverse___rec__expr__2__2__consR__1__out);
sli_0_0_2.in1(__fifo__reverse___cond__2__in__now__1____i0);
sli_0_0_2.out(__fifo__reverse___cond__2__out);
sli_0_0_1.in1(__fifo__reverse___cond__1__in__now__1____i0);
not_1.in1(__fifo__1);
sli_0_0_1.out(__fifo__1);
not_1.out(__fifo__reverse___cond__1__out);

SC_THREAD(proc);
}
};
void reverse_::proc() {
while(true) {
__i1__savev__val = __i1.read();
__i1__savev.push_back(__i1__savev__val);
if (__i1__savev__val != 0) {
while (__i1.nb_read(__i1__savev__val)) {
__i1__savev.push_back(__i1__savev__val);
if (__i1__savev__val == 0) break;
}
}

//blob 555
while (true) {
__i0__now__1 = __i0.read();
__fifo__reverse___cond__1__in__now__1____i0.write(__i0__now__1);
__fifo__reverse___cond__2__in__now__1____i0.write(__i0__now__1);
__fifo__reverse___cond__1__out__aux = __fifo__reverse___cond__1__out.read();
__fifo__reverse___cond__2__out__aux = __fifo__reverse___cond__2__out.read();
cond = (__fifo__reverse___cond__2__out__aux, __fifo__reverse___cond__1__out__aux);
if (cond[0]==1) {
for(std::vector<sc_lv<33> >::iterator __i1__savev__it_ = __i1__savev.begin(); __i1__savev__it_ != __i1__savev.end(); ++__i1__savev__it_) {
out.write(*__i1__savev__it_);
}

if (__i0__now__1 != 0) {
__i0__destroy = __i0.read();
if (__i0__destroy != 0) {
while(__i0.nb_read(__i0__destroy)) { if (__i0__destroy == 0) break; }
}
}

__i1__savev.clear();
break;


} else {
__fifo__reverse___rec__expr__2__2__consR__1__in__now__1____i0.write(__i0__now__1);
__fifo__reverse___rec__expr__2__2__consR__1__out__aux = __fifo__reverse___rec__expr__2__2__consR__1__out.read();
__i1__savev__it = __i1__savev.begin();
__i1__savev.insert(__i1__savev__it,__fifo__reverse___rec__expr__2__2__consR__1__out__aux);


}

}

}
}

#endif

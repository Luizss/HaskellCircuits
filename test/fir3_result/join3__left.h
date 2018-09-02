#ifndef JOIN3__LEFT_H_
#define JOIN3__LEFT_H_

#include "systemc.h"
#include "const_bin_11_.h"
#include "otherwise.h"
#include "sli_1_32_11_.h"
#include "sli_1_32_10_.h"
#include "sli_1_32_9_.h"
#include "f.h"
#include "cat3_.h"
#include "sli_0_0_8_.h"
#include "sli_0_0_7_.h"
#include "and_1_.h"
#include "sli_0_0_6_.h"


SC_MODULE(join3__left) {
  sc_fifo_in<sc_lv<33> > __i0;
  sc_fifo_in<sc_lv<33> > __acc;

  sc_fifo_out<sc_lv<33> > out;

  sc_fifo<sc_lv<1> > const_bin_11_out__cat1_in2;

  sc_fifo<sc_lv<32> > __fifo__8;
  sc_fifo<sc_lv<32> > __fifo__7;
  sc_fifo<sc_lv<32> > __fifo__6;
  sc_fifo<sc_lv<32> > __fifo__5;
  sc_fifo<sc_lv<1> > __fifo__4;
  sc_fifo<sc_lv<1> > __fifo__3;
  sc_fifo<sc_lv<1> > __fifo__2;
  sc_fifo<sc_lv<1> > __fifo__1;
  const_bin_11_ const_bin_11;
  otherwise otherwise1;
  sli_1_32_11_ sli_1_32_3;
  sli_1_32_10_ sli_1_32_2;
  sli_1_32_9_ sli_1_32_1;
  f f1;
  cat3_ cat1;
  sli_0_0_8_ sli_0_0_3;
  sli_0_0_7_ sli_0_0_2;
  and_1_ and_2;
  sli_0_0_6_ sli_0_0_1;
  and_1_ and_1;

  sc_lv<33> __i0__now__2;
  sc_lv<33> __i0__now__3;
  std::vector<sc_lv<33> > __acc__savev;
  sc_lv<33> __acc__savev__val;
  sc_lv<33> __i0__now__1;
  sc_fifo<sc_lv<33> > __fifo__join3__left__cond__1__in__now__1____i0;
  sc_fifo<sc_lv<33> > __fifo__join3__left__cond__1__in__now__2____i0;
  sc_fifo<sc_lv<33> > __fifo__join3__left__cond__1__in__now__3____i0;
  sc_lv<1> __fifo__join3__left__cond__1__out__aux;
  sc_fifo<sc_lv<1> > __fifo__join3__left__cond__1__out;
  sc_lv<1> __fifo__join3__left__cond__2__out__aux;
  sc_fifo<sc_lv<1> > __fifo__join3__left__cond__2__out;
  sc_lv<2> cond;
  sc_fifo<sc_lv<33> > __fifo__join3__left__rec__expr__1__2__consR__1__in__now__1____i0;
  sc_fifo<sc_lv<33> > __fifo__join3__left__rec__expr__1__2__consR__1__in__now__2____i0;
  sc_fifo<sc_lv<33> > __fifo__join3__left__rec__expr__1__2__consR__1__in__now__3____i0;
  sc_lv<33> __fifo__join3__left__rec__expr__1__2__consR__1__out__aux;
  sc_fifo<sc_lv<33> > __fifo__join3__left__rec__expr__1__2__consR__1__out;
  sc_lv<33> __i0__destroy;

  void proc();
 SC_CTOR(join3__left) : const_bin_11("const_bin_11"), otherwise1("otherwise1"), sli_1_32_3("sli_1_32_3"), sli_1_32_2("sli_1_32_2"), sli_1_32_1("sli_1_32_1"), f1("f1"), cat1("cat1"), sli_0_0_3("sli_0_0_3"), sli_0_0_2("sli_0_0_2"), and_2("and_2"), sli_0_0_1("sli_0_0_1"), and_1("and_1") {

    otherwise1.out(__fifo__join3__left__cond__2__out);
    const_bin_11.out(const_bin_11_out__cat1_in2);
    cat1.in2(const_bin_11_out__cat1_in2);

    sli_1_32_3.in1(__fifo__join3__left__rec__expr__1__2__consR__1__in__now__3____i0);
    f1.__i2(__fifo__8);
    sli_1_32_3.out(__fifo__8);
    sli_1_32_2.in1(__fifo__join3__left__rec__expr__1__2__consR__1__in__now__2____i0);
    f1.__i1(__fifo__7);
    sli_1_32_2.out(__fifo__7);
    sli_1_32_1.in1(__fifo__join3__left__rec__expr__1__2__consR__1__in__now__1____i0);
    f1.__i0(__fifo__6);
    sli_1_32_1.out(__fifo__6);
    cat1.in1(__fifo__5);
    f1.out(__fifo__5);
    cat1.out(__fifo__join3__left__rec__expr__1__2__consR__1__out);
    sli_0_0_3.in1(__fifo__join3__left__cond__1__in__now__3____i0);
    and_2.in2(__fifo__4);
    sli_0_0_3.out(__fifo__4);
    sli_0_0_2.in1(__fifo__join3__left__cond__1__in__now__2____i0);
    and_2.in1(__fifo__3);
    sli_0_0_2.out(__fifo__3);
    and_1.in2(__fifo__2);
    and_2.out(__fifo__2);
    sli_0_0_1.in1(__fifo__join3__left__cond__1__in__now__1____i0);
    and_1.in1(__fifo__1);
    sli_0_0_1.out(__fifo__1);
    and_1.out(__fifo__join3__left__cond__1__out);

    SC_THREAD(proc);
  }
};
void join3__left::proc() {
  while(true) {
    __i0__now__2 = __i0.read();
    if (__i0__now__2 == 0) {
      __i0__now__3 = 0;
    } else {
      __i0__now__3 = __i0.read();
    }
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
      __i0__now__1 = __i0__now__2;
      __i0__now__2 = __i0__now__3;
      __i0__now__3 = __i0.read();
      __fifo__join3__left__cond__1__in__now__1____i0.write(__i0__now__1);
      __fifo__join3__left__cond__1__in__now__2____i0.write(__i0__now__2);
      __fifo__join3__left__cond__1__in__now__3____i0.write(__i0__now__3);
      __fifo__join3__left__cond__1__out__aux = __fifo__join3__left__cond__1__out.read();
      __fifo__join3__left__cond__2__out__aux = __fifo__join3__left__cond__2__out.read();
      cond = (__fifo__join3__left__cond__2__out__aux, __fifo__join3__left__cond__1__out__aux);
      if (cond[0]==1) {
	__fifo__join3__left__rec__expr__1__2__consR__1__in__now__1____i0.write(__i0__now__1);
	__fifo__join3__left__rec__expr__1__2__consR__1__in__now__2____i0.write(__i0__now__2);
	__fifo__join3__left__rec__expr__1__2__consR__1__in__now__3____i0.write(__i0__now__3);
	__fifo__join3__left__rec__expr__1__2__consR__1__out__aux = __fifo__join3__left__rec__expr__1__2__consR__1__out.read();
	out.write(__fifo__join3__left__rec__expr__1__2__consR__1__out__aux);


      } else {
	for(std::vector<sc_lv<33> >::iterator __acc__savev__it_ = __acc__savev.begin(); __acc__savev__it_ != __acc__savev.end(); ++__acc__savev__it_) {
	  out.write(*__acc__savev__it_);
	}

	if (__i0__now__1 != 0 && __i0__now__2 != 0 && __i0__now__3 != 0) {
	  __i0__destroy = __i0.read();
	  if (__i0__destroy != 0) {
	    while(__i0.nb_read(__i0__destroy)) { if (__i0__destroy == 0) break; }
	  }
	}

	__acc__savev.clear();
	break;

      }

    }

  }
}

#endif

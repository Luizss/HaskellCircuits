#ifndef F_H_
#define F_H_

#include "systemc.h"
#include "const_dec_32_.h"
#include "const_dec_22_.h"
#include "const_dec_12_.h"
#include "mul1_.h"
#include "add1_.h"


SC_MODULE(f) {
sc_fifo_in<sc_lv<32> > __i0;
sc_fifo_in<sc_lv<32> > __i1;
sc_fifo_in<sc_lv<32> > __i2;

sc_fifo_out<sc_lv<32> > out;

sc_fifo<sc_lv<32> > const_dec_31_out__mul3_in2;
sc_fifo<sc_lv<32> > const_dec_21_out__mul2_in2;
sc_fifo<sc_lv<32> > const_dec_11_out__mul1_in2;

sc_fifo<sc_lv<32> > __fifo__4;
sc_fifo<sc_lv<32> > __fifo__3;
sc_fifo<sc_lv<32> > __fifo__2;
sc_fifo<sc_lv<32> > __fifo__1;
const_dec_32_ const_dec_31;
const_dec_22_ const_dec_21;
const_dec_12_ const_dec_11;
mul1_ mul3;
mul1_ mul2;
mul1_ mul1;
add1_ add2;
add1_ add1;



SC_CTOR(f) : const_dec_31("const_dec_31"), const_dec_21("const_dec_21"), const_dec_11("const_dec_11"), mul3("mul3"), mul2("mul2"), mul1("mul1"), add2("add2"), add1("add1") {

const_dec_31.out(const_dec_31_out__mul3_in2);
mul3.in2(const_dec_31_out__mul3_in2);

mul3.in1(__i2);
add1.in2(__fifo__4);
mul3.out(__fifo__4);
const_dec_21.out(const_dec_21_out__mul2_in2);
mul2.in2(const_dec_21_out__mul2_in2);

mul2.in1(__i1);
add2.in2(__fifo__3);
mul2.out(__fifo__3);
const_dec_11.out(const_dec_11_out__mul1_in2);
mul1.in2(const_dec_11_out__mul1_in2);

mul1.in1(__i0);
add2.in1(__fifo__2);
mul1.out(__fifo__2);
add1.in1(__fifo__1);
add2.out(__fifo__1);
add1.out(out);


}
};

#endif

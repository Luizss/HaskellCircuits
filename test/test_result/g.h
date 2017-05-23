#ifndef G_H_
#define G_H_

#include "systemc.h"
#include "const3.h"
#include "h.h"
#include "add.h"


SC_MODULE(g) {
sc_fifo_in<int> y;

sc_fifo_out<int> out;

sc_fifo<int> h1_out__add1_in2;
sc_fifo<int> const31_out__add2_in2;

sc_fifo<int> __fifo__1;
const3 const31;
h h1;
add add2;
add add1;


SC_CTOR(g) : const31("const31"), h1("h1"), add2("add2"), add1("add1") {

h1.out(h1_out__add1_in2);
add1.in2(h1_out__add1_in2);

const31.out(const31_out__add2_in2);
add2.in2(const31_out__add2_in2);

add2.in1(y);
add1.in1(__fifo__1);
add2.out(__fifo__1);
add1.out(out);


}
};

#endif

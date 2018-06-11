#ifndef MAINFUNC_H_
#define MAINFUNC_H_

#include "systemc.h"
#include "userNot.h"


SC_MODULE(mainFunc) {
sc_fifo_in<sc_lv<1> > __i0;

sc_fifo_out<sc_lv<1> > out;

sc_fifo<sc_lv<1> > main1___i0__userNot1___i0;
sc_fifo<sc_lv<1> > userNot1_out__main1_out;

userNot userNot1;



SC_CTOR(mainFunc) : userNot1("userNot1") {

userNot1.__i0(__i0);
userNot1.out(out);


}
};

#endif

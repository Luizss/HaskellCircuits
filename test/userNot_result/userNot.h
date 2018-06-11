#ifndef USERNOT_H_
#define USERNOT_H_

#include "systemc.h"
#include "UserTrue.h"
#include "__is__UserFalse.h"
#include "UserFalse.h"
#include "__is__UserTrue.h"


SC_MODULE(userNot) {
sc_fifo_in<sc_lv<1> > __i0;

sc_fifo_out<sc_lv<1> > out;


UserTrue UserTrue1;
__is__UserFalse __is__UserFalse1;
UserFalse UserFalse1;
__is__UserTrue __is__UserTrue1;

sc_lv<1> __i0__aux;
sc_fifo<sc_lv<1> > __fifo__userNot__cond__1__in____i0;
sc_fifo<sc_lv<1> > __fifo__userNot__cond__2__in____i0;
sc_lv<1> __fifo__userNot__cond__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__userNot__cond__1__out;
sc_lv<1> __fifo__userNot__cond__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__userNot__cond__2__out;
sc_lv<2> cond;
sc_lv<1> __fifo__userNot__expr__1__out__aux;
sc_fifo<sc_lv<1> > __fifo__userNot__expr__1__out;
sc_lv<1> __fifo__userNot__expr__2__out__aux;
sc_fifo<sc_lv<1> > __fifo__userNot__expr__2__out;

void proc();
SC_CTOR(userNot) : UserTrue1("UserTrue1"), __is__UserFalse1("__is__UserFalse1"), UserFalse1("UserFalse1"), __is__UserTrue1("__is__UserTrue1") {

UserTrue1.out(__fifo__userNot__expr__2__out);
__is__UserFalse1._x0(__fifo__userNot__cond__2__in____i0);
__is__UserFalse1.out(__fifo__userNot__cond__2__out);
UserFalse1.out(__fifo__userNot__expr__1__out);
__is__UserTrue1._x0(__fifo__userNot__cond__1__in____i0);
__is__UserTrue1.out(__fifo__userNot__cond__1__out);

SC_THREAD(proc);
}
};
void userNot::proc() {
while(true) {
__i0__aux = __i0.read();
__fifo__userNot__cond__1__in____i0.write(__i0__aux);
__fifo__userNot__cond__2__in____i0.write(__i0__aux);
__fifo__userNot__cond__1__out__aux = __fifo__userNot__cond__1__out.read();
__fifo__userNot__cond__2__out__aux = __fifo__userNot__cond__2__out.read();
cond = (__fifo__userNot__cond__2__out__aux, __fifo__userNot__cond__1__out__aux);
if (cond[0]==1) {
__fifo__userNot__expr__1__out__aux = __fifo__userNot__expr__1__out.read();
out.write(__fifo__userNot__expr__1__out__aux);


} else {
__fifo__userNot__expr__2__out__aux = __fifo__userNot__expr__2__out.read();
out.write(__fifo__userNot__expr__2__out__aux);

}

}
}

#endif

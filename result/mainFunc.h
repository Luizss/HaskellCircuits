#include "systemc.h"

#include "special_add.h"
#include "special_mul.h"
#include "constant_5.h"
#include "constant_3.h"

SC_MODULE(mainFunc) {

sc_fifo_in<int> in_0;
sc_fifo_out<int> out_0;

sc_fifo<int> special_add_out_0_main_out_0;
sc_fifo<int> main_in_0_special_mul_in_0;
sc_fifo<int> constant_5_out_0_special_mul_in_1;
sc_fifo<int> constant_3_out_0_special_add_in_1;

special_add _special_add;
special_mul _special_mul;
constant_5 _constant_5;
constant_3 _constant_3;
sc_fifo<int> fifo;

SC_CTOR(mainFunc) : _special_add("special_add"), _special_mul("special_mul"), _constant_5("constant_5"), _constant_3("constant_3") {

_special_add.out_0(out_0);
_special_mul.out_0(fifo);
_special_add.in_0(fifo);
_special_mul.in_0(in_0);
_constant_5.out_0(constant_5_out_0_special_mul_in_1);
_special_mul.in_1(constant_5_out_0_special_mul_in_1);

_constant_3.out_0(constant_3_out_0_special_add_in_1);
_special_add.in_1(constant_3_out_0_special_add_in_1);
}
};
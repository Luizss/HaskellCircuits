#include "systemc.h"
#include "const8.h"


SC_MODULE(h) {

sc_fifo_out<int> out;


const8 const81;


SC_CTOR(h) : const81("const81") {

const81.out(out);


}
};


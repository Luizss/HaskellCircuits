/* fixed point examples */  
#include "systemc.h"  
#include <stdio.h>
#include "sysc/datatypes/fx/sc_fix.h"

int sc_main(int argc, char *argv[]) {  
  float a = 1/3.0;  
  float b = 1/7.0;  
  float float_out;  
  sc_dt::sc_fixed<32, 4,SC_RND,SC_WRAP> fixed_out0; // 32 bit, 4 bit integer, 28 fract  
  sc_fixed<16, 4,SC_RND,SC_WRAP> fixed_out1; // 16 bit, 4 bit int, 12 bit fract  
  sc_fixed<8, 4,SC_RND,SC_WRAP> fixed_out2; // 8 bit, 4 bit int;  
  printf(" fixed point example\n");  
  cout << " Hello world\n";  
  float_out = a + b;  
  fixed_out0 = a + b;  
  fixed_out1 = a + b;  
  fixed_out2 = a + b;  
  cout << " float_out is " << float_out << endl;  
  cout << " fixed_out0 is " << fixed_out0 << endl;  
  cout << " fixed_out1 is " << fixed_out1 << endl;  
  cout << " fixed_out2 is " << fixed_out2 << endl;  
  return 0;  
}  

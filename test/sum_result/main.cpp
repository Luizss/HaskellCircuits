#include "systemc.h"
#include "top.h"

int sc_main (int argc, char *argv[]) {
top t("t");
sc_start(1000, SC_PS);
return 0;
}
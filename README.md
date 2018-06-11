# HaskellCircuits

Author: Luiz Gustavo Soares de Sá

Prototype project showing how to transform Haskell functions into hardware descriptions in a process called synthesis. The project is a "proof of concept" and therefore does not synthesize all the Haskell language. This compiler is backed by a theory on how to transform functions into hardware and is the implementation of the most important concepts of the theory.

-----

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

* `stack`
* `systemC`

### Installing

The project can be compiled using `stack`. The command below will generate the executable.

```
cd HaskellCircuits && stack build
```
### How to use the tool

To generate a SystemC executable from a source file the user must provide the file path to the source code followed by testbench information.

```
./HaskellCircuits <source_code_file_path> (list|nolist) <testbench_input>
```
UT 
Use `list` if your function has type list or use `nolist` for any other type. `<testbench_input>` is a Haskell list representing all the  inputs given to the hardware in the testbench. For example, if a function `factorial` is being synthesized a possible testbench would be `./HaskellCircuits factorial nolist '[2,5,8,9,10]'` applying factorial to the inputs 2,5,8,9 and 10 in order.

Any more complex type of testbench can be tested by modifying the `testbench.h` SystemC file generated in every compilation.

### Compiling the SystemC result

In order to compile the result the user must have the SystemC libraries installed.

Firstly move to the result directory named whatever the name of the input file followed by `_result`. The compilation command may vary from computer to computer but the commands below may work (for 32 bits and then for 64 bits linux and replacing `$SYSTEMC_HOME` by the path to the SystemC libraries). 

```
g++ -I. -I$SYSTEMC_HOME/include -L. -L$SYSTEMC_HOME/lib-linux -Wl,-rpath=$SYSTEMC_HOME/lib-linux -o main *.cpp -lsystemc -lm
```

```
g++ -I. -I$SYSTEMC_HOME/include -L. -L$SYSTEMC_HOME/lib-linux64 -Wl,-rpath=$SYSTEMC_HOME/lib-linux64 -o main *.cpp -lsystemc -lm
```

### Running the testbench

To run the testbench just run the SystemC executable.

```
./main
```

Testbench results will be printed in the screen in binary form. In other to test more complex testbenches it's possible to change the `testbench.h` SystemC file (but this requires a little bit of SystemC knowledge) and compile the result again.

----

## Tests

In the `test` directory there are code examples and their respective results (for example, the file `map` and its result `map_result`).

----

## Brief explanation of the technique


             Haskell
                +
                |
                |
            +---v---+
            |Parsing|
            +-------+
                |
                |
       +--------v-----------+
       |Code Simplification |
       +--------------------+
                |
                |
     +----------v------------+
     |Type Inference/Checking|
     +-----------------------+
                |
                |
         +------v-------+
         |Type Synthesis|
         +--------------+
                |
                |
      +---------v----------+
      |Functional Synthesis|
      +--------------------+
                |
                |
        +-------v-------+
        |Code Generation|
        +---------------+
                |
                |
                v
             SystemC

`Parsing` and `Code Simplification` are similar to what a software compiler would do. Synthesis starts with `Type Synthesis`, or the transformation from Algebraic Data Types to Vectors and Streams of Bits. Now that every function operates with bit structured types the `Functional Synthesis` analyses the kind of recursion (if the function is recursive) and extract the necessary information (basically, states and state transitions) to synthesize it. Function application is generally synthesized with buffered connections. Hardware-like modules are the result from the functional synthesis step. `Code Generation` basically takes each module and translates its behaviour to SystemC modules. A paper detailing the method will be published soon.

----

## Unsupported 

* Polymorphic Definitions
* Higher-Order Declarations
* Infinite lists
* Where, Let
* Currying
* Irregular recursive functions and data types (meaning any type of recursion other than simple)

----

## Known Bugs

* Filter test function
* Use list definition not synthesizing the right way
* Drop test function

----

## Built With

* [Haskell](https://www.haskell.org/) - The web framework used
* [Happy](https://www.haskell.org/happy/) - Parser Generator
* [Alex](https://www.haskell.org/alex/) - A lexical analyser generator for Haskell

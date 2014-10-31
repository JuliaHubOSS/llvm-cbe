//===-- CBackend.cpp - Library for converting LLVM code to C ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===------------------------------------------------------------------------------===//
//
// This code tests to see that the CBE will execute data-packing in a structure correctly.
// *TW
//===------------------------------------------------------------------------------===//

struct packed_test {
 unsigned int var1:4;
 unsigned int var2:4;
 unsigned int var3:4;
};

int main(){
    struct packed_test variable;
    variable.var2 = 5;
    variable.var3 = 6;
    variable.var1 = 9;

    return variable.var3;
}


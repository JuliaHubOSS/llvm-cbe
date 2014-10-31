//===-- CBackend.cpp - Library for converting LLVM code to C ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===------------------------------------------------------------------------------===//
//
// This code tests to see that the CBE will access and return union members correctly.
// *TW
//===------------------------------------------------------------------------------===//

union Data{
   int i;
   float f;
};

int main(){
   union Data data;

   data.i = 6; //Invalid due to data.f
   data.f = 6;

   return data.f;
}


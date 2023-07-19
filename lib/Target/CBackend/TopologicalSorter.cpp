//===------------------ TopologicalSorter.cpp--------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements a DFS based Topological Sorter to order dependencies in
// the C Backend
//
//===----------------------------------------------------------------------===//
#include "TopologicalSorter.h"

namespace llvm_cbe {

void TopologicalSorter::addEdge(int Start, int End) {
  AdjacencyMatrix[Start].push_back(End);
}

TopologicalSorter::TopologicalSorter(int Size) {
  this->Size = Size;
  AdjacencyMatrix.resize(Size);
  Marks.resize(Size, Mark::Unvisited);
  Result.reserve(Size);
}

std::optional<std::vector<int>> TopologicalSorter::sort() {
  for (int I = 0; I < Size; ++I) {
    if (visit(I)) {
      return std::nullopt;
    }
  }
  return Result;
}

bool TopologicalSorter::visit(int Node) {
  if (Marks[Node] == Mark::Permanent) {
    return false;
  }
  if (Marks[Node] == Mark::Temp) { // Cycle
    return true;
  }
  Marks[Node] = Mark::Temp;
  for (const int I : AdjacencyMatrix[Node]) {
    if (visit(I)) {
      return true;
    }
  }
  Marks[Node] = Mark::Permanent;
  Result.push_back(Node);
  return false;
}

} // namespace llvm_cbe

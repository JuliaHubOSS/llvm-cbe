//===------------------ TopologicalSorter.h----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares a DFS based Topological Sorter to order dependencies in
// the C Backend
//
//===----------------------------------------------------------------------===//
#ifndef TOPOLOGICALSORTER_H
#define TOPOLOGICALSORTER_H

#include <llvm/Config/llvm-config.h>
#include <optional>
#include <vector>

namespace llvm_cbe {

class TopologicalSorter {
  int Size;
  std::vector<std::vector<int>> AdjacencyMatrix;
  enum class Mark {
    Unvisited,
    Temp,
    Permanent,
  };
  std::vector<Mark> Marks;
  std::vector<int> Result;
  bool visit(int Node); // Returns true on cycle detection

public:
  explicit TopologicalSorter(int Size);

  void addEdge(int Start, int End);
  std::optional<std::vector<int>> sort(); // Returns None if there are cycles
};

} // namespace llvm_cbe

#endif // TOPOLOGICALSORTER_H

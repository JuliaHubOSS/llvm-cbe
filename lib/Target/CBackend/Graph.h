#ifndef GRAPH_H
#define GRAPH_H


#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <algorithm>
#include <cstdio>

#include <iostream>
#include <queue>
#include <list>
#include <stack>
#include <set>



using namespace llvm;


enum Color {WHITE, GRAY, BLACK};
enum TypeC {ENDIF, ELSE, IF, ELSEIF, UNKNOWN, STARTLOOP, ENDLOOP};



struct DFSNode {
 BasicBlock *Bb;
 Color C;
 int DTime;
 int FTime;
 std::vector<DFSNode *> Vertex; // holds the next nodes if has not been visited
 TypeC T;
};

class Graph {
 public:
  Graph(Module * Mod);
  Graph(Function * F);
  ~Graph();
  void printGraphFromMod();
  bool branchCheck(BasicBlock *BB);
  void createMap();
  void updateMap(BasicBlock *BB);
  void printGraph();
  void makeGraph();
  void print();
  bool isLoop(BasicBlock *BB);
  bool shouldReturn(BasicBlock *Pred, BasicBlock *Succ, bool Start);
  bool shouldReturn(BasicBlock *Succ, BasicBlock *Pred);
  bool checkIf(BasicBlock *Succ, BasicBlock *Pred);
  bool shouldPrint(BasicBlock *BB);
  int returnType(BasicBlock *BB);
  int returnType(BranchInst &I);
  void printInstrTotal();

 private:
  Module * M;
  std::map<BasicBlock *, BasicBlock *> Pred;
  std::map<BasicBlock *, BasicBlock *> Succ;
  std::map<BasicBlock *, std::vector<BasicBlock *> > Pred_Vec;
  std::map<BasicBlock *, std::vector<BasicBlock *> > Succ_Vec;
  std::list<BasicBlock *> BBList;
  std::list<DFSNode *> DFSList;
  std::stack<TypeC> TypeStack;
  std::map<Value *, BasicBlock *> BranchMap;

  std::set<BasicBlock *> BFS_Set(BasicBlock * BB);
  void handlePhiNodes();
  void DFS();
  void DFS2();
  void DFS_visit(DFSNode *DFS, std::list<DFSNode *> dfslist);
  void DFS2_visit(DFSNode *DFS);
  TypeC getLoopTypeC(DFSNode *DFS);
  int Time;
  bool SearchDFS(DFSNode *DFS, BasicBlock *Pred);
  bool search(BasicBlock *Pred, BasicBlock *Succ, std::set<BasicBlock *> Succs);
};

#endif

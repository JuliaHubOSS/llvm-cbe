#include "Graph.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/CFG.h"
#include "llvm/Support/Debug.h"
#include "llvm/IR/InstIterator.h"

//Jackson Korba 9/30/14
#ifndef DEBUG_TYPE
#define DEBUG_TYPE ""
#endif
//End Modification

Graph::Graph(Module *Mod)
:M(Mod),
Time(0){}


Graph::~Graph() {
  for (std::list<DFSNode *>::iterator It = DFSList.begin(), E = DFSList.end(); It != E; ++It) {
    delete *It;
  }
}



#ifdef DEBUG
void Graph::printGraph() {
 for (iplist<Function>::iterator It = M->getFunctionList().begin(); It != M->getFunctionList().end(); ++It) {
  Function * Fn = It;
  for (iplist<BasicBlock>::iterator I = Fn->getBasicBlockList().begin(); I != Fn->getBasicBlockList().end(); ++I) {
   DEBUG(errs() << "This is the BasicBlock: " << I << '\n' << "Predecessors: \n");

   for (pred_iterator PI = pred_begin(I); PI != pred_end(I); ++PI) {
     DEBUG(errs() << *PI << '\n');
   }
  }
 }
}
#endif

void Graph::makeGraph() {
 for (iplist<Function>::iterator It = M->getFunctionList().begin(); It != M->getFunctionList().end(); ++It) {
  Function * Fn = It;
  for (iplist<BasicBlock>::iterator I = Fn->getBasicBlockList().begin(); I != Fn->getBasicBlockList().end(); ++I) {
    BBList.push_back(I); 
    for (pred_iterator PI = pred_begin(I); PI != pred_end(I); ++PI){
     Pred_Vec[I].push_back(*PI);
     Succ_Vec[*PI].push_back(I);
    }
  }
 }
 DFS();
 DFS2();
}

#ifdef DEBUG
void Graph::print() {
 std::cout << "\n\n\n";
 for (iplist<Function>::iterator It = M->getFunctionList().begin(); It != M->getFunctionList().end(); ++It) {
  Function * Fn = It;
  for (iplist<BasicBlock>::iterator I = Fn->getBasicBlockList().begin(); I != Fn->getBasicBlockList().end(); ++I) {
    DEBUG (errs() << I << "\t\t" );
    std::vector<BasicBlock *> BBpvec = Pred_Vec[I];
    DEBUG (errs() << "Predecessors: \t");
    for (std::vector<BasicBlock *>::iterator Bi = BBpvec.begin(); Bi != BBpvec.end(); ++Bi) {
      DEBUG (errs() << *Bi << " ; ");
    }
    DEBUG (errs() << "\t\t\tSucessors: \t");
    std::vector<BasicBlock *> BBsvec = Succ_Vec[I];
    for (std::vector<BasicBlock *>::iterator Bi = BBsvec.begin(); Bi != BBsvec.end(); ++Bi) {
      DEBUG (errs() << *Bi << " ; ");
    }
    DEBUG (errs() << '\n');
   }
 }
}

void Graph::printGraphFromMod() {
 int InstCount = 0;
 for (iplist<Function>::iterator It = M->getFunctionList().begin(); It != M->getFunctionList().end(); ++It) {
  Function * Fn = It;
   
  for (iplist<BasicBlock>::iterator I = Fn->getBasicBlockList().begin(); I != Fn->getBasicBlockList().end(); ++I) {
    BasicBlock * Bb = I;
    DEBUG (errs() << Bb << '\n');
    BasicBlock * PSBB = Bb->getSinglePredecessor();
    if (PSBB != NULL) {
      DEBUG (errs() << "Single Predecessor: \n" << PSBB << "\n_______________________" << '\n');
    }
    else if ((PSBB = (Bb->getUniquePredecessor())) != NULL)
      DEBUG (errs() << "Unique Predecessor: " << PSBB << '\n');
    else {
      DEBUG (errs() << "No Predecessor\n_______________________\n");
    }
    // print the instructions
    for (iplist<Instruction>::iterator X = Bb->getInstList().begin(); X != Bb->getInstList().end(); ++X) {
       InstCount++;
       DEBUG (errs() << (*X).getOpcodeName() << '\n');
    }
    DEBUG (errs() << "\n\n\n");
  }
 }
 DEBUG (errs() << "Instruction Count = " << InstCount << "\n");
}



void Graph::printInstrTotal() {
 int InstCount = 0;
 for (iplist<Function>::iterator It = M->getFunctionList().begin(); It != M->getFunctionList().end(); ++It) {
  Function * Fn = It;
  for (iplist<BasicBlock>::iterator I = Fn->getBasicBlockList().begin(); I != Fn->getBasicBlockList().end(); ++I) {
    BasicBlock * Bb = I;
    for (iplist<Instruction>::iterator X = Bb->getInstList().begin(); X != Bb->getInstList().end(); ++X) {
       InstCount++;
    }
  }
 }
 DEBUG (errs() << "Instruction Count = " << InstCount << "\n");
}
#endif

void Graph::createMap() {
 for (iplist<Function>::iterator It = M->getFunctionList().begin(); It != M->getFunctionList().end(); ++It) {
  Function * Fn = It;
  for (iplist<BasicBlock>::iterator I = Fn->getBasicBlockList().begin(); I != Fn->getBasicBlockList().end(); ++I) {
   BasicBlock * Bb = I;
   BasicBlock * PSBB = Bb->getSinglePredecessor();

   Pred[Bb] = PSBB;
   if (PSBB != NULL)
     Succ[PSBB] = Bb;

  }
 }
}


bool Graph::branchCheck(BasicBlock *BB)
{
 BasicBlock * PS = Pred[BB];
 return Pred[PS] == NULL;
}


std::set<BasicBlock *> Graph::BFS_Set(BasicBlock * BB){
 std::set<BasicBlock * > BBL;
 // 1. place the starting node s on the queue
 std::queue<BasicBlock * > BBQ;

 BBQ.push(BB);

 // 2. if the queue is empty, return failure and stop
 while (!BBQ.empty()){

  // 3. remove and expand the first element from the queue and
  //place all the children at the end of the queue in any order
  BasicBlock * Pop = BBQ.front();
  BBQ.pop();
  BBL.insert(Pop);
  DEBUG (errs() << "Insert into BFS set: " << Pop <<'\n');

  std::vector<BasicBlock *> Svec = Succ_Vec[Pop];
  for (std::vector<BasicBlock *>::iterator It = Svec.begin(); It != Svec.end(); ++It){
   bool C = (BBL.end() == BBL.find(*It)); // true if does not contain
   if (C){
     BBL.insert(*It);
     BBQ.push(*It);
   }
  }
  // 4. return to 2
 }
 return BBL;
}


void Graph::DFS()
{
 DEBUG (errs() << "\n\n\n\n\n Initializing DFS nodes \n\n\n\n\n\n");
 // for(std::map<BasicBlock *, std::vector<BasicBlock *> >::iterator it = succ_vec.begin(); it != succ_vec.end(); ++it){
 // add all BasicBlock nodes to the list to create a list of dfsnodes
 for (std::list<BasicBlock *>::iterator It = BBList.begin(); It != BBList.end(); ++It){
  DFSNode * DFSN = new DFSNode;
  DFSN->Bb = *It;
  DFSN->C = WHITE;
  DFSN->T = UNKNOWN;
  DFSList.push_back(DFSN);
  DEBUG (errs() << "added " << DFSN->Bb << " to the list\n");
 }
 Time = 0;
 for (std::list<DFSNode *>::iterator It = DFSList.begin(); It != DFSList.end(); ++It){
 DFSNode *P = *It;
 if (P->C == WHITE) 
  DFS_visit(P, DFSList);
 }
}

void Graph::DFS_visit(DFSNode * DFS, std::list<DFSNode *> DFSList) {
 DFS->C = GRAY;
 Time++;
 DFS->DTime = Time;
 // for each vector adjacent to dfs
 std::vector<BasicBlock *> Svec = Succ_Vec[DFS->Bb];
 for (std::vector<BasicBlock *>::iterator It = Svec.begin(); It != Svec.end(); ++It) {
  for (std::list<DFSNode *>::iterator I = DFSList.begin(); I != DFSList.end(); ++I) {
    if ((*I)->Bb == *It) {
      if ((*I)->C == BLACK)
        (*I)->T = ENDIF;
      else if ((*I)->C == GRAY){
        DEBUG (errs() << "\nWe are trying to go to a Gray BB\nWe are in: " << DFS->Bb << "Which is " << DFS->T << " We want to go to: " << (*I)->Bb << " Which is " << (*I)->T << "\n");
        if(DFS->T != STARTLOOP) DFS->T = ENDLOOP;
        (*I)->T = STARTLOOP;
      }
      else if ((*I)->C == WHITE) {
        DFS->Vertex.push_back(*I);
        DFS_visit(*I, DFSList);
      }
    }
   }
 }
 DFS->C = BLACK;
 DFS->FTime = ++Time; 
 DEBUG (errs() << "DFS node: " << DFS->Bb << " Dtime: " << DFS->DTime << " Ftime: " << DFS->FTime << " TypeC: " << DFS->T << '\n');
}


void Graph::DFS2() {
 // add all BasicBlock nodes to the list to create a list of dfsnodes
 for (std::list<DFSNode *>::iterator It = DFSList.begin(); It != DFSList.end(); ++It){
  (*It)->C = WHITE;
 }
 Time = 0;
 for (std::list<DFSNode *>::iterator It = DFSList.begin(); It != DFSList.end(); ++It){
  DFSNode *P = *It;
  if (P->C == WHITE) 
  DFS2_visit(P);
 }
}
  

void Graph::DFS2_visit(DFSNode * DFS) {
 DFS->C = GRAY;
 if (DFS->T == UNKNOWN){
  Instruction *I = dynamic_cast<Instruction *>(DFS->Bb->getTerminator());
  BranchInst *BI = static_cast<BranchInst *>(I);
  if (BI->isConditional()){
   //branchmap[BI] = dfs->bb;
   BranchMap[BI->getCondition()] = DFS->Bb;
   if (TypeStack.empty())
    DFS->T = IF;
   else if (TypeStack.top() != ENDIF)
    DFS->T = IF;
   //else if (TypeStack.top() == ENDIF && Time == DFS->DTime - 2)
   // DFS->T = ELSEIF;
   else if (DFS->Bb->getName().substr(0,7) == "if.else")
     DFS->T = ELSEIF;
   //else if (TypeStack.top() == ELSEIF)
   // DFS->T = ELSEIF;

   else if(TypeStack.top() == ENDIF && (Time != DFS->DTime - 1))
    DFS->T = IF;
   else
    DEBUG (errs() << "\n\n\n\n\n\n\nError at: " << " Top = " << TypeStack.top() << " Time = " << Time << " DTime = " << DFS->DTime << "\n\n\n\n\n\n\n\n\n");
   }
   else {
    DEBUG (errs() << "Bb: " <<DFS->Bb << " is not conditional\n");
   }
 }
 if (DFS->T != UNKNOWN){
  Time = DFS->DTime;
  TypeStack.push(DFS->T);
  DEBUG (errs() << "Stack: " << TypeStack.top() <<'\n');
 }

 DEBUG (errs() << "Type of: " << DFS->Bb << " is: " << DFS->T 
               << " global time: " << Time << " dfstime: " << DFS->DTime << '\n');

 // for each vector adjacent to dfs
 std::vector<DFSNode *> Svec = DFS->Vertex;
 for (std::vector<DFSNode *>::iterator It = Svec.begin(); It != Svec.end(); ++It)
 {
  DFSNode *DN = *It; 
  if (DN->C == WHITE)
  DFS2_visit(DN);
 }
 DFS->C = BLACK;
 if (DFS->T != UNKNOWN)
  Time = DFS->FTime; 
}


int Graph::returnType(BranchInst &I){
 return returnType(BranchMap[I.getCondition()]);
}

int Graph::returnType(BasicBlock *BB)
{
 //find BasicBlock
 for (std::list<DFSNode *>::iterator It = DFSList.begin(); It != DFSList.end(); ++It){
  if ((*It)->Bb == BB)
   return (*It)->T;
  }

 return UNKNOWN;
}


bool Graph::shouldPrint(BasicBlock* BB)
{
 for (std::list<DFSNode *>::iterator It = DFSList.begin(); It != DFSList.end(); ++It){
  if ((*It)->Bb/*was bb*/ == BB) {
   return (*It)->T == ENDIF;
  }
 }

 DEBUG (errs() << "Cannot find this BasicBlock" << "\n");
 return false;
}


bool Graph::shouldReturn(BasicBlock *Succ, BasicBlock *Pred, bool Start)
{
 if (Start)
 //  return checkIf(Succ, Pred);
  return true;
 else
  return shouldReturn(Succ, Pred);
}

bool Graph::shouldReturn(BasicBlock *Succ, BasicBlock *Pred)
{
 DFSNode *PredDfs;
 DFSNode *SuccDfs;
 
 for (std::list<DFSNode *>::iterator It = DFSList.begin(); It != DFSList.end(); ++It){
  if( (*It)->Bb/*bb*/ == Pred ) {
   PredDfs = *It;
  }
  else if ( (*It)->Bb/*bb*/ == Succ ) {
   SuccDfs = *It;
  }
 }
 DEBUG(errs() << "Checking Pred: " << PredDfs->Bb/*bb*/ << " at " << PredDfs->DTime << "\n"
              << "Against: " << SuccDfs->Bb << " at " << SuccDfs->DTime << "\n" );

 // if discovery time of Pred is less than that of Succ we want to print
 return PredDfs->DTime < SuccDfs->DTime;
}

// want to check if the succ node is seen by the pred in DFS
bool Graph::SearchDFS(DFSNode *DFS, BasicBlock *Succ) {
 if ( DFS->Bb/*bb*/ == Succ )
  return true;

 std::vector<DFSNode *> DFSVec = DFS->Vertex;

 // reccurse on all succs
 for (std::vector<DFSNode *>::iterator It = DFSVec.begin(); It != DFSVec.end(); ++It){
  // if we found the BB return true
  if ( SearchDFS(*It, Succ) )
   return true;
  }
 return false;
}

bool Graph::checkIf(BasicBlock *Succ, BasicBlock *Pred)
{
 std::set<BasicBlock *> Succs;
 return search(Pred, Succ, Succs);

}

bool Graph::search(BasicBlock *Pred, BasicBlock *Succ, std::set<BasicBlock *> Succs) {

 Succs.insert(Pred);

 // expand Pred 
 std::vector<BasicBlock *> Svec = Succ_Vec[Pred];
 for (std::vector<BasicBlock *>::iterator It = Svec.begin(); It != Svec.end(); ++It){
  Succs.insert(*It);
 }

 bool NContain = (Succs.find(Succ) == Succs.end());
 // have not found Succ
 if (NContain){
  for (std::vector<BasicBlock *>::iterator It = Svec.begin(); It != Svec.end(); ++It){
   if ( search(*It, Succ, Succs) )
   return true;
  }
 DEBUG (errs() << "Error: Cannot find Succ BasicBlock" << "\n");
}
 else {
 // check its preds to make sure all of them are in the set
  std::vector<BasicBlock *> Pvec = Pred_Vec[Succ];
  for(std::vector<BasicBlock *>::iterator It = Pvec.begin(); It != Pvec.end(); ++It){
   bool NCont = (Succs.find(*It) == Succs.end());
   if ( NCont )  return false;
  }
  return true;
 }
 return false;
} 
   
bool Graph::isLoop(BasicBlock * BB){
 std::vector<BasicBlock *> Svec = Succ_Vec[BB];
 std::set<BasicBlock *> BBSet;
 for (std::vector<BasicBlock *>::iterator It = Svec.begin(); It != Svec.end(); ++It){
  std::set<BasicBlock *> BFS = BFS_Set(*It);
  BBSet.insert(BFS.begin(), BFS.end());
 }
 bool NotContain = (BBSet.find(BB) == BBSet.end());
 return !NotContain;
}

#!/usr/bin/python
import os
import re
import subprocess

LLVMToolDir = os.getenv('LLVMToolDir')
if not LLVMToolDir:
    LLVMToolDir = '.'

cexe_res = []
ll_res = []
cbe_res = []
failure_list = []

def getList(fileExt):
  fl = []
  for name in os.listdir("."):
    if re.search(fileExt, name):
      fl.append(name)
  fl.sort()
  return fl

def genResults(fileExt, fl):
  for names in fl:
    if os.path.isfile(names):
      if (fileExt == ".cexe"):
        res = subprocess.call(["./%s" % (names)])
        cexe_res.append(res) # append for each optimization level
        cexe_res.append(res)
        cexe_res.append(res)
        cexe_res.append(res)

      elif (fileExt == ".ll"):
        res = subprocess.call([LLVMToolDir + "/lli", names])
        ll_res.append(res)

      elif (fileExt == ".cbe"):
        res = subprocess.call(["./%s" % (names)])
        cbe_res.append(res)

      if (res != 6 and res != 25):
	catstring = names + ":\tFailure [" + str(res) + "]"
        failure_list.append(catstring)

def main():
  print "Hunting for bugs... Please wait..."

  cexeRegex = ".cexe$"
  cexe = ".cexe"
  file_list = getList(cexeRegex)
  genResults(cexe, file_list)
  print 'Running tests:', file_list

  irRegex = ".ll$"
  llIR = ".ll"
  file_list = getList(irRegex)
  genResults(llIR, file_list)

  cbeRegex = ".cbe$"
  cbe = ".cbe"
  file_list = getList(cbeRegex)
  genResults(cbe, file_list)

  print 'cexe_res:', cexe_res
  print 'll_res:  ', ll_res
  print 'cbe_res: ', cbe_res

  if (cexe_res == ll_res == cbe_res):
    print("Success!")

  elif (cexe_res != ll_res == cbe_res):
    print("Failure Detected between C and IR! Incorrect Return Values!")
    for bug in failure_list:
      print(bug)

  elif (cexe_res == ll_res != cbe_res):
    print("Failure Detected between IR and CBE! Incorrect Return Values!")
    for bug in failure_list:
      print(bug)

  elif (cexe_res != ll_res != cbe_res):
    print("Discrepancy between all three types! Incorrect Return Values!")
    for bug in failure_list:
      print(bug)

if __name__ == "__main__":
  main()

#!/usr/bin/python
import os
import re
import subprocess

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
        cexe_res.append(res)
                
      elif (fileExt == ".ll"):
        res = subprocess.call(["lli", names])
        ll_res.append(res) 

      elif (fileExt == ".cbe"):
        res = subprocess.call(["./%s" % (names)])
        cbe_res.append(res) 

      if (res != 6 and res != 25):
	catstring = names + ":\tFailure [" + str(res) + "]"
        failure_list.append(catstring)

def main():
  print "Hunting for bugs... Please wait..."
  file_list = []
  llIR = ".ll"
  cbe = ".cbe"
  cexe = ".cexe"
  irRegex = ".ll$"
  cbeRegex = ".cbe$"
  cexeRegex = ".cexe$"

  #Find a better way to work with the C return value array
  #Currently fills in array with values correct values since
  #all of the exisiting cases are tested to work... Not accurate 
  #if additions are made.
  for x in range(0, 309):
    cexe_res.append(6)

  file_list = getList(cexeRegex)
  genResults(cexe, file_list)
  file_list = getList(irRegex)
  genResults(llIR, file_list)
  file_list = getList(cbeRegex)
  genResults(cbe, file_list)

  for x in range(0,3):
    cexe_res.append(25)

  if (cexe_res == ll_res == cbe_res):
    print("Success!")

  elif (cexe_res != ll_res == cbe_res):
    print("Failure Detected between C abd IR! Incorrect Return Values!")
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

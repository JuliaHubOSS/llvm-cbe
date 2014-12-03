#!/usr/bin/python
import os
import re
import subprocess

def getList(fileExt):
  fl = []
  for name in os.listdir("."):
    if re.search(fileExt, name):
      fl.append(name)
      fl.sort()
  return fl

def printResults(fileExt, fl):
  for names in fl:
    if os.path.isfile(names):
      if (fileExt == ".ll"):
        res = subprocess.call(["lli", names])
      elif (fileExt == ".cbe"):
        res = subprocess.call(["./%s" % (names)])
    
      if (res == 6 or res == 25):
        print ("%s:\tSuccess [%d]" % (names, res))
      else:
        print ("%s:\tFailure [%d]<<<<----" % (names, res))

def main():
  file_list = []
  llIR = ".ll"
  cbe = ".cbe"
  irRegex = ".ll$"
  cbeRegex = ".cbe$"  

  file_list = getList(irRegex)
  printResults(llIR, file_list)
  file_list = getList(cbeRegex)
  printResults(cbe, file_list)

if __name__ == "__main__":
  main()

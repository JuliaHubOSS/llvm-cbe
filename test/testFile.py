#!/usr/bin/python
import os
import subprocess

def getFile(name):
    (name, end) = name.split(".c", 1)
    name = name + ".cbe"
    return name

def testFile(name):
    if os.path.isfile(name):
        return True
    else:
        print "%s does not exist" % (name)
        return False     
        
def doSubprocess(name): 
    res = subprocess.call(["./%s" % (name)])
    if (res == 6 or res == 25):
        print ("%s: Success" % name)
    else:
        print ("%s: %d, Fail" %  (name, res))
        
        
def main():
    file_names = []
    for name in os.listdir("."):
        if name.find(".cbe.c") != -1:
            name = getFile(name)
            file_names.append(name)
    file_names.sort()
    for names in file_names:
        if testFile(names) == True:
            doSubprocess(getFile(names))
        
if __name__ == "__main__":
    main()
    

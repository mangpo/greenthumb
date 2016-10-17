#!/usr/bin/python

import sys, os
import os.path

def create(isa,c,filename,lang):
    
    if os.path.isfile("template/"+c+".rkt"):
        filename = "template/"+c+".rkt"
        fin = open(filename,"r")
        fout = open(isa + "/" + isa + "-" + c + ".rkt","w")
        text = fin.read().replace("$-",isa+"-")
        print >>fout, text
        fin.close()
        fout.close()
    else:
        fin = open(filename,"r")
        fout = open(isa + "/" + isa + "-" + c + ".rkt","w")
        text = fin.read().replace("$1",isa).replace("$2",c)
        print >>fout, "#lang", lang
        print >>fout, text
        fin.close()
        fout.close()

def main(isa):
    print "Create template files for", isa
    os.system("mkdir " + isa)

    for name in ["test-simulator.rkt", "test-search.rkt", "main.rkt", "optimize.rkt"]:
        fin = open("template/" + name,"r")
        fout = open(isa + "/" + name,"w")
        text = fin.read().replace("$",isa)
        print >>fout, text
        fin.close()
        fout.close()

    # racket
    for c in ["machine", "simulator-racket"]:
        create(isa,c,"template/class-constructor.rkt","racket")
        
    for c in ["parser", "printer", "stochastic", "forwardbackward"]:
        create(isa,c,"template/class.rkt","racket")
        
        
    # # rosette
    for c in ["simulator-rosette", "validator"]:
        create(isa,c,"template/class-constructor.rkt","s-exp rosette")
        
    for c in ["symbolic", "inverse", "enumerator"]:
        create(isa,c,"template/class.rkt","s-exp rosette")

if __name__ == "__main__":
    main(sys.argv[1])

#!/usr/bin/env python3
# For Python 3

import sys, os
import random

USAGE = """Usage: rand-sig.py [NUMBER] [FILE]

In FILE, treat blocks with NUMBER of lines a signature, and randomly
choose and print one.  An empty line between signatures is needed.

If only one argument is given, rand-sig assume it is FILE.  And NUMBER
is then the default 4.

If no argument is given, then NUMBER is 4; FILE is `$HOME/.signature'.
"""

def splitEvenly(n, l):
    """Splits a sequence `l' evenly by every `n' element, and returns
    the result as a generator.
    """
    for i in range(0, len(l), n):
        yield l[i : i+n]

def main():
    SigLinesCount = 4
    SigFileName = os.environ["HOME"] + "/.signature"
    
    if len(sys.argv) == 3:
        SigLinesCount = int(sys.argv[1])
        SigFileName = sys.argv[2]

    elif len(sys.argv) == 2:
        if sys.argv[1] in ["-h", "--help"]:
            print(USAGE)
            return 0
        
        SigFileName = sys.argv[1]

    elif len(sys.argv) > 3:
        print(USAGE, file=sys.stderr)
        return 1
        
    SigFile = open(SigFileName, 'r')
    SigsLines = [line.strip() for line in SigFile.readlines()]
    SigFile.close()

    Sigs = ['\n'.join(sig) \
            for sig in splitEvenly(SigLinesCount + 1, SigsLines)]

    print(random.choice(Sigs).strip())
    
    return 0



if __name__ == "__main__":
    sys.exit(main())

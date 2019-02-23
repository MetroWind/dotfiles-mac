#!/usr/bin/env python3

from __future__ import print_function
import re
import subprocess as SubP

def getKeychainPass(account=""):
    Cmd = ["security", "find-internet-password", "-w", "-a", account]
    Output = SubP.check_output(Cmd).decode("utf-8").strip('\n')
    return Output

if __name__ == "__main__":
    import sys
    print(getKeychainPass(sys.argv[1]))

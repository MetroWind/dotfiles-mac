#!/usr/bin/env python
# -*- coding: utf-8; -*-

from __future__ import print_function

import sys, os
import subprocess

sys.path.append("lib")
import utils

Log = utils.getLogger(__name__)

def install(section, os_type):
    """Run install scription for `section` and `os_type`.

    :type section: str
    :type os_type: OSType
    """
    Pwd = os.getcwd()
    os.chdir(section)
    subprocess.check_call([os.path.join('.', os_type.InstallName),])
    os.chdir(Pwd)

def promptInstallSections(sections, os_type, prompt_packages=True):
    install("bootstrap", os_type)

    for Section in sections:
        if prompt_packages is not True or \
           utils.promptYN("Install {}?".format(Section)) is True:
            install(Section, os_type)

    # Print report

    print("""

============================= Installation Report =============================
""")
    for Section in ["bootstrap",] + sections:
        FileOverwritten = os.path.join(Section, "report-overwritten.txt")
        if os.path.exists(FileOverwritten):
            print("{}: overwritten files:".format(Section))
            for File in utils.file2List(FileOverwritten):
                print(File)
            os.remove(FileOverwritten)

        FileIgnored = os.path.join(Section, "report-ignored.txt")
        if os.path.exists(FileIgnored):
            print("{}: ignored files:".format(Section))
            for File in utils.file2List(FileIgnored):
                print(File)
            os.remove(FileIgnored)

        FileError = os.path.join(Section, "report-error.txt")
        if os.path.exists(FileError):
            for Line in utils.file2List(FileError):
                print("[ERROR] {}: {}".format(Section, Line))
            os.remove(FileError)

        FileInfo = os.path.join(Section, "report-info.txt")
        if os.path.exists(FileInfo):
            for Line in utils.file2List(FileInfo):
                print("[INFO] {}: {}".format(Section, Line))
            os.remove(FileInfo)

    print()

AllPkgs = ["zsh", "git", "python", "emacs", "tmux", "iterm2", "bin", "mail"]

def main():
    import argparse

    Parser = argparse.ArgumentParser(description='Install configuration files '
                                     'by linking.')
    Parser.add_argument("-s", "--os", type=str, dest="OS", required=True,
                        choices=["mac", "windows", "arch"],
                        help="Type of the target operating system.")
    Parser.add_argument("-a", "--all", dest="All", default=False,
                        action="store_true",
                        help="Install all packages without asking.")
    Parser.add_argument("-e", "--exclude", dest="Excludes", default=[],
                        nargs="+", metavar="PKG",
                        help="Exclude PKGs from installation.")

    Args = Parser.parse_args()

    promptInstallSections([Pkg for Pkg in AllPkgs if Pkg not in Args.Excludes],
                          utils.OSType(Args.OS), not Args.All)
    return 0

if __name__ == "__main__":
    sys.exit(main())

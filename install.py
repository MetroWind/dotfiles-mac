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
    if os.path.exists(os_type.InstallName):
        subprocess.check_call([os.path.join('.', os_type.InstallName),],
                              shell=True)
    else:
        Log.warning("Do not know how to install {} for {}. Skipping..."
                    .format(section, os_type.Name))
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

AllPkgs = ["zsh", "git", "python", "rust", "emacs", "tmux", "iterm2", "bin",
           "mail", "security", "rime", "firefox", "xmonad", "xorg", "kitty"]

def main():
    import argparse

    Parser = argparse.ArgumentParser(description='Install configuration files '
                                     'by linking.')
    Parser.add_argument("-s", "--os", type=str, dest="OS", required=True,
                        choices=["mac", "linux"],
                        help="Type of the target operating system.")
    Parser.add_argument("-a", "--all", dest="All", default=False,
                        action="store_true",
                        help="Install all packages without asking.")
    Parser.add_argument("-e", "--exclude", dest="Excludes", default=[],
                        nargs="+", metavar="PKG",
                        help="Exclude PKGs from installation.")
    Parser.add_argument("-o", "--only", dest="Only", default=None,
                        nargs="+", metavar="PKG",
                        help="Install only PKGs.")

    Args = Parser.parse_args()

    Prompt = True
    if Args.All is True:
        Prompt = False

    if Args.Only is not None:
        Prompt = False

    Excludes = set(Args.Excludes)

    Pkgs = AllPkgs
    AllPkgsSet = set(AllPkgs)
    if Args.Only is not None:
        Pkgs = []
        for Pkg in Args.Only:
            if Pkg in AllPkgsSet:
                if Pkg in Excludes:
                    print("Excluding {}...".format(Pkg))
                else:
                    Pkgs.append(Pkg)
            else:
                print("Ignoring unknown package '{}'...".format(Pkg))


    promptInstallSections(Pkgs, utils.OSType(Args.OS), Prompt)
    return 0

if __name__ == "__main__":
    sys.exit(main())

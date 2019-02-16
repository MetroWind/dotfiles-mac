#!/usr/bin/env python3
# -*- coding: utf-8; -*-

from __future__ import print_function

import sys, os
import shutil
import glob
import logging
import subprocess

def get_logger(name=__name__, level=logging.DEBUG):
    logger = logging.getLogger(name)
    logger.setLevel(level)
    handler = logging.StreamHandler(sys.stderr)
    formatter = logging.Formatter("[%(asctime)s %(levelname)s] %(message)s",
                                  "%Y-%m-%d %H:%M:%S")
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    return logger

Log = get_logger()

def copyfile(src, dst, follow_symlinks=True):
    if sys.version_info >= (3, 3):
        shutil.copyfile(src, dst, follow_symlinks)
    else:
        shutil.copyfile(src, dst)

def chmod(path, mode, dir_fd=None, follow_symlinks=True):
    if sys.version_info >= (3, 3):
        os.chmod(path, mode, dir_fd, follow_symlinks)
    else:
        os.chmod(path, mode)

def promptWithChoices(msg, choices, answer=None):
    """Prompt user with `msg` and `choices` (list/tuple of strings). Return the item
    user chooses.

    :type msg: str
    :type choices: list
    :rtype: str
    """
    if answer is not None:
        return answer

    RealMsg = "{} [{}] ".format(msg, '/'.join(choices))
    if sys.version_info.major >= 3:
        Input = input(RealMsg)      # type: str
    else:
        Input = raw_input(RealMsg)

    try:
        Idx = tuple(item.lower() for item in choices).index(Input.lower())
    except ValueError:
        return None
    else:
        return choices[Idx]

def promptYN(msg, answer=None):
    Response = promptWithChoices(msg, ('y', 'n'), answer)
    if Response is None:
        print("Invalid response.")
        return promptYN(msg, answer)

    return Response == 'y'

class FileInstaller(object):
    def __init__(self, link=False, overwrite=False):
        self.Link = link        # Symlink instead of copying?

        # Overwrite if dest exists? Can be True, False, or "ask"
        self.Overwrite = overwrite

    def __call__(self, source, dest, new_name=None, mode=None):
        """Copy/symlink file `source` into directory `dest`, and rename the new file to
        `new_name` if it's not `None`. If `mode` is not None, set the mode of
        the new file. Return the path of the new file if copied/symlinked.
        Otherwise return None (because of not overwriting the dest).

        :type source: str
        :type dest: str
        :type new_name: str
        :type mode: int
        :rtype: str
        """

        if os.path.islink(dest):
            if promptYN("{} is a link. Delete it?".format(dest)):
                os.remove(dest)
            else:
                Log.error("Target dir {} is a link. Stopping...".format(dest))
                raise OSError("Target dir {} is a link.".format(dest))

        if not os.path.exists(dest):
            os.makedirs(dest)

        if new_name is None:
            Basename = os.path.basename(source)
        else:
            Basename = new_name

        Target = os.path.join(dest, Basename)

        if os.path.isdir(Target):
            Log.error("Target {} is a directory. Stopping...".format(Target))
            raise OSError("Target {} is a directory".format(Target))

        if os.path.exists(Target) or os.path.islink(Target):
            if self.Overwrite is True:
                os.remove(Target)
            elif self.Overwrite == "ask":
                if promptWithChoices("Overwrite {}?".format(Target), ('y', 'n')) == 'y':
                    os.remove(Target)
                else:
                    return None
            else:
                return None

        if self.Link is True:
            os.symlink(os.path.abspath(source), Target)
        else:
            copyfile(source, Target, follow_symlinks=True)
            if mode is not None:
                chmod(Target, mode, follow_symlinks=False)

        Log.debug("{} --> {}".format(source, Target))

        return Target

class FileSetInstaller(object):
    def __init__(self, src_dir, glob="*"):
        self.SrcDir = src_dir
        self.install = FileInstaller(True, "ask")
        self.Glob = glob

    @property
    def Link(self):
        return self.install.Link

    @Link.setter
    def Link(self, value):
        self.install.Link = value
        return value

    @property
    def Overwrite(self):
        return self.install.Overwrite

    @Overwrite.setter
    def Overwrite(self, value):
        self.install.Overwrite = value
        return value

    def installTo(self, dest_dir):
        Files = glob.glob(os.path.join(self.SrcDir, self.Glob))
        for File in Files:
            if os.path.isdir(File):
                continue

            self.install(File, dest_dir)

class OSType(object):
    def __init__(self, name):
        self.Name = name
        self.Suffix = name

    @property
    def install_name(self):
        return "install." + self.Suffix

def install(section, os_type):
    """Run install scription for `section` and `os_type`.

    :type section: str
    :type os_type: OSType
    """
    Pwd = os.getcwd()
    os.chdir(section)
    subprocess.check_call([os.path.join('.', os_type.install_name),])
    os.chdir(Pwd)


def promptInstallSections(sections, os_type):
    install("bootstrap", os_type)

    for Section in sections:
        if promptYN("Install {}?".format(Section)) is True:
            install(Section, os_type)

def main():
    import argparse

    Parser = argparse.ArgumentParser(description='Install configuration files '
                                     'by linking.')
    Parser.add_argument("-s", "--os", type=str, dest="OS", required=True,
                                choices=["mac", "windows", "arch"],
                                help="Type of the target operating system.")

    Args = Parser.parse_args()

    promptInstallSections(["zsh", "git",], OSType(Args.OS))
    return 0

if __name__ == "__main__":
    sys.exit(main())

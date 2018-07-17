#!/usr/bin/env python3
# -*- coding: utf-8; -*-

from __future__ import print_function

import sys, os
import shutil
import glob
import logging

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
    Input = input(RealMsg)      # type: str
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

        if not os.path.exists(dest):
            os.makedirs(dest)

        if new_name is None:
            Basename = os.path.basename(source)
        else:
            Basename = new_name

        Target = os.path.join(dest, Basename)

        if os.path.isdir(Target):
            Log.fatal("Target {} is a directory. Stopping...".format(Target))
            raise OSError("Target {} is a directory".format(Target))

        if os.path.exists(Target):
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
            shutil.copyfile(source, Target, follow_symlinks=True)
            if mode is not None:
                os.chmod(Target, mode, follow_symlinks=False)

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


def doInstall(link, overwrite, all=False):
    Link = link
    Overwrite = overwrite
    RepoDir = os.path.dirname(__file__)
    Home = os.environ["HOME"]

    if all is True:
        SectionAnswer = 'y'
    else:
        SectionAnswer = None

    if promptYN("Install Emacs configuration?", SectionAnswer):
        Installer = FileSetInstaller(os.path.join(RepoDir, ".emacs-pkgs"))
        Installer.Link = Link
        Installer.Overwrite = Overwrite
        Installer.installTo(os.path.join(Home, ".emacs-pkgs"))
        DotEmacs = Installer.install(os.path.join(RepoDir, ".emacs.el"), Home)

        if DotEmacs and os.path.exists(os.path.join(Home, ".emacs")):
            if promptYN(".emacs.el installed. Remove ~/.emacs?"):
                os.remove(os.path.join(Home, ".emacs"))

        if not os.path.exists(os.path.join(Home, ".emacs-pkgs", "mw-user.el")):
            if promptYN("There's one more step for Emacs: copy the example "
                        "user file and modify it. Copy the example user file?"):
                FileInstaller(False, True)(
                    os.path.join(Home, ".emacs-pkgs", "mw-user-example.el"),
                    os.path.join(Home, ".emacs-pkgs"), "mw-user.el", 0o600)
                print("Now you need to edit {} and uncomment the last line. "
                      "It *should* work without modification, "
                      "but that’s not fun. (But do uncomment the last line.)\n"
                      .format(os.path.join(Home, ".emacs-pkgs", "mw-user.el")))

    if promptYN("Install Zsh configuration?", SectionAnswer):
        Installer = FileSetInstaller(os.path.join(RepoDir, ".zsh"))
        Installer.Link = Link
        Installer.Overwrite = Overwrite

        Installer.installTo(os.path.join(Home, ".zsh"))
        Installer.install(os.path.join(RepoDir, ".zshrc"), Home)

        ProfileInstall = FileInstaller(True, Overwrite)
        ProfileInstall(os.path.join(Home, ".profile"), Home, ".zprofile")

    if promptYN("Install Git configuration?", SectionAnswer):
        Installer = FileSetInstaller(RepoDir, ".git*")
        Installer.Link = Link
        Installer.Overwrite = Overwrite
        Installer.installTo(Home)

    if promptYN("Insatll tmux configuration?", SectionAnswer):
        install = FileInstaller(Link, Overwrite)
        install(os.path.join(RepoDir, ".tmux.conf"), Home)

    return 0

def main():
    import argparse

    Parser = argparse.ArgumentParser(description='Install configuration files '
                                     'by linking.')
    OverwriteGroup = Parser.add_mutually_exclusive_group()
    OverwriteGroup.add_argument('-y', "--overwrite", dest='Overwrite',
                                action='store_true', default=False,
                                help="Overwrite files without asking. Default: ask.")
    OverwriteGroup.add_argument('-n', "--no-overwrite", dest='NoOverwrite',
                                action='store_true', default=False,
                                help="Never overwrite files. Default: ask")

    Parser.add_argument("-c", "--copy", dest="Link", action="store_false",
                        default=True, help="Copy file instead of linking.")

    Args = Parser.parse_args()

    if Args.Overwrite is True:
        Overwrite = True
    elif Args.NoOverwrite is True:
        Overwrite = False
    else:
        Overwrite = "ask"

    return doInstall(Args.Link, Overwrite)


if __name__ == "__main__":
    sys.exit(main())

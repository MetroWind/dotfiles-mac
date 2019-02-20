#!/usr/bin/env python

import sys, os
import unittest
import subprocess

sys.path.append(os.path.join("..", "lib"))
import utils

class TestInstallZsh(utils.ConfigInstallTest):
    def test_config(self):
        Home = os.environ["HOME"]
        PkgDir = os.path.dirname(__file__)
        self.checkLinks(os.path.join(PkgDir, "files"), "", Home)
        self.checkLinks(os.path.join(PkgDir, "files"), ".zsh", Home)

    def test_login_shell(self):
        Output = subprocess.check_output(["id", "-P"]).decode()
        LoginShell = Output.split(':')[-1].strip()
        Zsh = subprocess.check_output("which zsh", shell=True).decode().strip()
        self.assertEqual(LoginShell, Zsh)

if __name__ == '__main__':
    unittest.main()

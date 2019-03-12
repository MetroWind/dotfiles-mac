#!/usr/bin/env python

import sys, os
import unittest

sys.path.append(os.path.join("..", "lib"))
import utils

class TestInstallEmacs(utils.ConfigInstallTest):
    def test_executable(self):
        self.assertTrue(os.path.exists("/Applications/Emacs.app"))

    def test_config(self):
        Home = os.environ["HOME"]
        PkgDir = os.path.dirname(__file__)
        self.checkLinks(os.path.join(PkgDir, "files"), "", Home)
        self.checkLinks(os.path.join(PkgDir, "files"), ".emacs-pkgs", Home)

if __name__ == '__main__':
    unittest.main()

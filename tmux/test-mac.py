#!/usr/bin/env python

import sys, os
import subprocess
import unittest

sys.path.append(os.path.join("..", "lib"))
import utils

class TestInstallTmux(utils.ConfigInstallTest):
    def test_executable(self):
        self.assertEqual(subprocess.call(["tmux", "-V"]), 0)

    def test_config(self):
        Home = os.environ["HOME"]
        PkgDir = os.path.dirname(__file__)
        self.checkLinks(os.path.join(PkgDir, "files"), "", Home)

if __name__ == '__main__':
    unittest.main()

#!/usr/bin/env python

import sys, os
import unittest
import subprocess

sys.path.append(os.path.join("..", "lib"))
import utils

class TestInstalliTerm2(utils.ConfigInstallTest):
    def test_config(self):
        Home = os.environ["HOME"]
        PkgDir = os.path.dirname(__file__)
        Hash1 = subprocess.check_output(
            ["sha1sum", os.path.join(PkgDir, "files", "com.googlecode.iterm2.plist")])
        Hash2 = subprocess.check_output(
            ["sha1sum", os.path.join(os.environ["HOME"], "Library", "Preferences",
                                     "com.googlecode.iterm2.plist")])
        self.assertEqual(Hash1.split()[0], Hash2.split()[0])

    def test_app_install(self):
        self.assertTrue(os.path.exists("/Applications/iTerm.app"))

if __name__ == '__main__':
    unittest.main()

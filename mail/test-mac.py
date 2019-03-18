#!/usr/bin/env python

import sys, os
import unittest
import subprocess

sys.path.append(os.path.join("..", "lib"))
import utils

class TestInstallMail(utils.ConfigInstallTest):
    def test_executable(self):
        self.assertTrue(os.path.exists(os.path.join(
            os.getenv("HOME"), "bin", "getmails.py")))
        self.assertTrue(os.access(os.path.join(
            os.getenv("HOME"), "bin", "getmails.py"),
                        os.X_OK))
        self.assertTrue(os.path.exists(os.path.join(
            os.getenv("HOME"), "bin", "notmuch-notifier.py")))
        self.assertTrue(os.access(os.path.join(
            os.getenv("HOME"), "bin", "notmuch-notifier.py"),
                        os.X_OK))
        self.assertEqual(subprocess.call("which offlineimap", shell=True), 0)
        self.assertEqual(subprocess.call("which notmuch", shell=True), 0)


if __name__ == '__main__':
    unittest.main()

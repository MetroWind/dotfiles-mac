#!/usr/bin/env python

import sys, os
import subprocess
import unittest

sys.path.append(os.path.join("..", "lib"))
import utils

class TestInstallGPG(utils.ConfigInstallTest):
    def test_executable(self):
        self.assertEqual(subprocess.call(["gpg", "--version"]), 0)

if __name__ == '__main__':
    unittest.main()

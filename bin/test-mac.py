#!/usr/bin/env python

import sys, os
import unittest
import subprocess

sys.path.append(os.path.join("..", "lib"))
import utils

class TestInstallBin(utils.ConfigInstallTest):
    def test_file_install(self):
        PkgDir = os.path.dirname(__file__)
        for File in os.listdir(os.path.join(PkgDir, "files")):
            Target = os.path.join(os.getenv("HOME"), "bin", File)
            self.assertTrue(os.path.exists(Target))

        self.assertEqual(subprocess.call([os.path.join(
            os.getenv("HOME"), "bin", "appify.py"), "-h"]),
                         0)

if __name__ == '__main__':
    unittest.main()

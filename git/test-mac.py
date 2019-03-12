#!/usr/bin/env python

import sys, os
import unittest
import subprocess
import tempfile
import shutil

sys.path.append(os.path.join("..", "lib"))
import utils

class TestInstallGit(utils.ConfigInstallTest):
    @unittest.skipIf(os.getenv("IS_AZURE_PIPELINE") == '1',
                     "Azure pipline has custom gitconfig")
    def test_config(self):
        Home = os.environ["HOME"]
        PkgDir = os.path.dirname(__file__)
        self.checkLinks(os.path.join(PkgDir, "files"), "", Home)

    @unittest.skipIf(os.getenv("IS_AZURE_PIPELINE") == '1',
                     "Azure pipline has custom gitconfig")
    def test_graph(self):
        Pwd = os.getcwd()
        Tmp = os.environ["TMPDIR"]
        RepoDir = tempfile.mkdtemp()
        Env = dict(HOME=os.environ["HOME"], TERM="xterm")
        os.chdir(RepoDir)
        subprocess.check_call(["git", "init"])
        self.assertEqual(subprocess.call(["git", "graph"], env=Env), 0)
        shutil.rmtree(RepoDir)
        os.chdir(Pwd)

if __name__ == '__main__':
    unittest.main()

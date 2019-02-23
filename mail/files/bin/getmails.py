#!/usr/bin/env python3

import sys, os
import time
import json
import signal
import subprocess as SubP
import logging

import yaml

def getLogger(name=__name__, level=logging.DEBUG):
    logger = logging.getLogger(name)
    logger.setLevel(level)
    handler = logging.StreamHandler(sys.stderr)
    formatter = logging.Formatter("[%(asctime)s %(levelname)s] %(message)s",
                                  "%Y-%m-%d %H:%M:%S")
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    return logger

Logger = getLogger()

LockName = "/tmp/getmail.lock"
ProcTimeout = 1200                # 20 minutes
KillTimeout = 5

def run(lock, *args, **kargs):
    Proc = SubP.Popen(*args, **kargs)
    lock.setSubPid(Proc.pid)
    Proc.wait()
    if Proc.returncode != 0:
        Msg = "{} returned {}.".format(args[0][0], Proc.returncode)
        Logger.error(Msg)
        raise RuntimeError(Msg)

def findConfig():
    if os.getenv("XDG_CONFIG_HOME"):
        return os.path.join(os.getenv("XDG_CONFIG_HOME"), "getmails.yaml")
    else:
        return os.path.join(os.getenv("HOME"), ".config", "getmails.yaml")

def runImap(lock, dirs_inbox, dirs_outbox):
    run(lock, ["offlineimap",], preexec_fn=os.setsid)
    run(lock, ["notmuch", "new"])
    run(lock, ["notmuch", "tag", "+inbox", "--",
               "({}) and tag:new".format(
                   " or ".join("folder:" + inbox for inbox in dirs_inbox))])
    run(lock, ["notmuch", "tag", "+sent", "--",
               "({}) and tag:new".format(
                   " or ".join("folder:" + inbox for inbox in dirs_outbox))])
    try:
        run(lock, ["notmuch-notifier.py"])
    except RuntimeError as Err:
        Logger.error(str(Err))
    run(lock, ["notmuch", "tag", "-new", "--", "tag:new"])

class ProcessLock(object):
    def __init__(self, filename):
        self.LockName = filename

    def __enter__(self):
        Info = dict(pid=os.getpid(), time=time.time())
        with open(self.LockName, 'w') as f:
            json.dump(Info, f)
        return self

    def __exit__(self, type, value, traceback):
        os.remove(self.LockName)

    @staticmethod
    def checkLock(filename):
        if os.path.exists(filename):
            with open(filename, 'r') as f:
                Info = json.load(f)
            Logger.info("Previous proc exists at " + str(Info["pid"]))
            return Info
        else:
            return None

    def setSubPid(self, pid_sub):
        with open(self.LockName, 'r') as Lock:
            LockInfo = json.load(Lock)
            LockInfo["sub_pid"] = pid_sub
        with open(self.LockName, 'w') as Lock:
            json.dump(LockInfo, Lock)

def existsPid(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    finally:
        return True

def kill(pid, sig, wait=1):
    """Try to kill PID with signal `sig`.  Return True if succeed.  Otherwide
    return False.
    """
    try:
        os.kill(pid, sig)
    except ProcessLookupError:
        Logger.error("Cannot find PID %d for TERM.", )
        return False
    except:
        Logger.info("Failed to kill previous proc with TERM.")
        return False
    else:
        time.sleep(wait)
        return (not existsPid(pid))

def autoRun():
    Info = ProcessLock.checkLock(LockName)
    if Info is None:
        ConfFile = findConfig()
        if os.path.exists(ConfFile):
            with open(ConfFile, 'r') as f:
                Config = yaml.load(f)
        else:
            Logger.error("Failed to find config file at {}.".format(ConfFile))
            return 1

        with ProcessLock(LockName) as Lock:
            runImap(Lock, Config["dirs_inbox"], Config["dirs_outbox"])

    else:
        Now = time.time()
        if Now - Info["time"] > ProcTimeout:
            Pid = Info["pid"]
            if "sub_pid" in Info:
                Pid = Info["sub_pid"]

            if kill(Pid, signal.SIGTERM, KillTimeout):
                Logger.info("Killed process at %d with TERM.", Pid)
                os.remove(LockName)
                autoRun()
                return 0
            else:
                Logger.error("Failed to kill process at %d with TERM.", Pid)

            if kill(Pid, signal.SIGKILL):
                Logger.info("Killed process at %d with KILL.", Pid)
                os.remove(LockName)
                autoRun()
                return 0
            else:
                Logger.error("Failed to kill process at %d with KILL.", Pid)

            Logger.error("Cannot run subprocesses.")

    return 0

if __name__ == "__main__":
    sys.exit(autoRun())

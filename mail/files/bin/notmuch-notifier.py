#!/usr/bin/env python3

import sys, os
import json
import re
import subprocess as Sub

Notifier = (os.getenv("HOME") +
            "/Applications/Mini Notifier.app/Contents/MacOS/Mini Notifier")

def getMails(tag="new", query=None):
    if query is None:
        OutputRaw = Sub.check_output(["notmuch", "search", "--format=json",
                                      "tag:" + tag])
    else:
        OutputRaw = Sub.check_output(["notmuch", "search", "--format=json",
                                      query])
    Output = OutputRaw.decode()
    Mails = json.loads(Output)
    return Mails

def notify(author, subject):
    Sub.check_call([Notifier, "-t", "New Mail", "-s", author, subject])

def notifyMails(mails):
    for Mail in mails:
        Match = re.match(r"([^|]+)\|?.*", Mail["authors"])
        if not Match:
            print(Mail["authors"])
        Mail["author"] = Match.group(1)
        notify(Mail["author"], Mail["subject"])

def main():
    if os.path.exists(Notifier):
        notifyMails(getMails(query="tag:new AND tag:inbox"))
    else:
        # Silently ignore if notifier is not installed.
        pass

if __name__ == "__main__":
    main()

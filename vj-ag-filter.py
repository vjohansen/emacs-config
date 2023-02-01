import fileinput
import re
import os
import sys

def print_file_matches(dir, collect):
    print("\nEntering directory `{}'".format(dir))
    for suffix in collect:
        print(suffix)
    print("Leaving directory `{}'".format(dir))
    collect = []

collect = []
lastdir = None
hits = 0

for line in fileinput.input():
    line = line.rstrip()
    if len(line) == 0:
        continue
    match = re.match('^(..[^:]+)([:(]\d+[:,]\d+[:)].*)$', line)
    if not match:
        print(line)
        continue
    hits += 1
    dir = os.path.dirname(match.group(1)) # os.path.realpath( ?
    basename = os.path.basename(match.group(1))
    if lastdir is None:
        lastdir = dir
    if dir != lastdir:
        print_file_matches(lastdir, collect)
        collect = []
    collect.append(basename + match.group(2))
    lastdir = dir

if lastdir is not None:
    print_file_matches(lastdir, collect)

if hits == 0:
   sys.exit(1)
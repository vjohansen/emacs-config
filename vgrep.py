# Vagn Johansen (2017)

import sys
import os
import glob
import argparse
import re
import subprocess

parser = argparse.ArgumentParser(description='Grep directories.')
parser.add_argument('-e', default=':code')
parser.add_argument('-m')
parser.add_argument('-i', action='store_const', const='i', default='')
parser.add_argument('-r', action='append')
parser.add_argument('dirs', nargs='*')

args = parser.parse_args()
word = args.dirs.pop(0)

exts = re.sub(':code','c,cpp,cc,cxx,cs,h,hpp,hh,asm,el,pl,pm,js,py,ml,cs,java' +
              ',cls,bas,sh,zsh,rb,php,ts,fs,fsx,r,m,xaml',args.e)
exts = re.sub(':text','org,txt,htm,html,mak,csproj,sln,vcproj,proj,bat,zsh,' +
              'config,xslt,xsl,css,asp,xml,xsl,xslt,sql', exts)

ext_re = '|'.join(exts.split(','))
ext_re = '\.('+ext_re+')$'
exitcode = 1

def grep(dir_):
    if dir_ == 'NUL':
        return 0
    global exitcode
    count = 0
    os.chdir(dir_)
    filenames = list(filter(lambda f: re.search(ext_re,f.lower()), glob.glob('*.*')))
    if len(filenames) == 0:
        return 0
    count += len(filenames)
    shell_args = ['grep','-{}nH'.format(args.i), word]
    shell_args.extend(filenames)
    sys.stdout.flush()
    try:
        output = subprocess.check_output(shell_args, shell=True).decode('utf-8')
    except:
        return count

    # python 3.5 can use
    #    output = subprocess.run(shell_args, stdout=subprocess.PIPE).stdout.decode('utf-8')
    if len(output) > 2:
        exitcode = 0
        print('Entering directory `{}\''.format(dir_))
        print(output.rstrip())
        print('Leaving directory `{}\''.format(dir_))
        print()
    else:
        print('No matches in',dir_)

    return count

count = 0
for dir_ in args.dirs:
    count += grep(dir_)

if args.r is not None:
    for rdir in args.r:
        for root, dirs, files in os.walk(rdir, topdown=True):
            dirs[:] = [d for d in dirs if d != '.git']
            count += grep(root)

print('{} files searched'.format(count))
sys.exit(exitcode)

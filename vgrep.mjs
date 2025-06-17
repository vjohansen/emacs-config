#!/usr/bin/env node
// Vagn Johansen (2017) - ported to Node.js 2025

import fs from 'fs';
import path from 'path';
import { spawnSync } from 'child_process';

function parseArgs() {
  const args = process.argv.slice(2);
  let opts = {
    e: ':code',
    m: null,
    A: null,
    B: null,
    i: false,
    r: [],
    dirs: [],
  };
  let i = 0;
  while (i < args.length) {
    let arg = args[i];
    if (arg === '-e') {
      opts.e = args[++i];
    } else if (arg === '-m') {
      opts.m = args[++i];
    } else if (arg === '-A') {
      opts.A = parseInt(args[++i], 10);
    } else if (arg === '-B') {
      opts.B = parseInt(args[++i], 10);
    } else if (arg === '-i') {
      opts.i = true;
    } else if (arg === '-r') {
      opts.r.push(args[++i]);
    } else if (arg.startsWith('-')) {
      // ignore unknown
    } else {
      opts.dirs.push(arg);
    }
    i++;
  }
  return opts;
}

const args = parseArgs();
if (args.dirs.length === 0) {
  console.error('Usage: node vgrep.js <word> [dirs...]');
  process.exit(1);
}
const word = args.dirs.shift();

let exts = args.e.replace(
  /:code/,
  'c,cpp,cc,cxx,cs,h,hpp,hh,asm,el,pl,pm,js,py,ml,cs,java' +
    ',cls,bas,pas,frm,sh,zsh,rb,php,ts,fs,fsx,r,m,xaml' +
    ',ts,tsx,jsx,json',
);
exts = exts.replace(
  /:text/,
  'org,txt,log,htm,html,mak,csproj,sln,vcproj,proj,bat,zsh,' +
    'config,xslt,xsl,css,asp,xml,xsl,xslt,sql',
);

const ext_re = new RegExp('\\.(' + exts.split(',').join('|') + ')$', 'i');
let exitcode = 1;

const homepath = process.env.HOMEPATH
  ? process.env.HOMEDRIVE + process.env.HOMEPATH
  : null;

function grep(dir_) {
  if (dir_ === 'NUL') return 0;
  dir_ = dir_.replace('~', process.env.HOME || homepath || '~');
  if (!fs.existsSync(dir_) || !fs.statSync(dir_).isDirectory()) {
    console.log('Directory does not exists:', dir_);
    return 0;
  }
  let count = 0;
  const files = fs.readdirSync(dir_).filter((f) => ext_re.test(f.toLowerCase()));
  if (files.length === 0) return 0;
  count += files.length;
  let shell_args = ['-nH'];
  if (args.i) shell_args.unshift('-i');
  if (args.A) {
    shell_args.push('-A');
    shell_args.push(String(args.A));
  }
  if (args.B) {
    shell_args.push('-B');
    shell_args.push(String(args.B));
  }
  shell_args.push(word);
  shell_args = shell_args.concat(
    files.map((fn) => fn.replace('{', '\\{').replace('}', '\\}')),
  );
  const grepProc = spawnSync('grep', shell_args, { cwd: dir_, encoding: 'utf8' });
  let output = grepProc.stdout || '';
  if (grepProc.error) {
    // grep not found or failed
    return count;
  }
  if (output.length > 2) {
    exitcode = 0;
    console.log(`Entering directory \`${dir_}'`);
    process.stdout.write(output.trimEnd() + '\n');
    console.log(`Leaving directory \`${dir_}'\n`);
  } else {
    console.log('No matches in', dir_);
  }
  return count;
}

let count = 0;
for (const dir_ of args.dirs) {
  count += grep(dir_);
}

if (args.r && args.r.length > 0) {
  for (let rdir of args.r) {
    rdir = rdir.replace(/%20/g, ' ');
    function walk(root) {
      let subdirs;
      try {
        subdirs = fs.readdirSync(root, { withFileTypes: true });
      } catch (e) {
        return;
      }
      let dirs = [];
      for (const entry of subdirs) {
        if (
          entry.isDirectory() &&
          !['.git', 'node_modules', 'build', 'dist'].includes(entry.name)
        ) {
          dirs.push(entry.name);
        }
      }
      count += grep(root);
      for (const d of dirs) {
        walk(path.join(root, d));
      }
    }
    walk(rdir);
  }
}

console.log(`${count} files searched`);
process.exit(exitcode);

# Vagn Johansen (2004-2010)

my $exitcode = 1; # default to no-matches

sub usage
{
    die <<"EOD";
Usage: <vgrep.pl> [-i] [-s] [-f filename_pattern] [-e file_extensions] [-g opt]
                  [-p grep_program]
                  search_regexp ( dirs | ( -r <recurse_dir> ) )+

-i: case-insensitive text search
-f: include filenames that matches case-insitively (e.g. "makefile$")
-e: include filenames with extensions in comma-sep list (e.g. "c,h")
-s: Suppress error messages about nonexistent directories.
-g: extra grep options
-m: called from this `major-mode'

grep_pattern:   Search regexp
dirs:           Directory to search in
-r recurse_dir: Search recursively in directory <recurse_dir>

All options must come before search_regexp (except -r).

Uses of "-r" must always come after search_regexp. Searching is done in
the same order as specified on the command line.
EOD
# perl -S vgrep_new.pl -e cpp,c,h "peer" -r c:/cygwin/usr/include
}

use strict;
use Getopt::Std;
use File::Find;
$|++; # auto-flush stdout

my (%opt, $regexp_e, $pattern);

# parse_options
unshift @ARGV, "-r", "DUMMY_DIR"; # so getopt does not touch the other -r's
getopt('efrmgp', \%opt); # -e, -f, -r, -m, -g and -p takes arg.

my $regexp ||= $opt{f};
if ($opt{e}) {
  $opt{e} =~ s/:code/c,cpp,cc,cxx,cs,h,hpp,hh,asm,el,pl,pm,js,py,ml,cs,java,cls,bas,sh,zsh,rb,php,ts,fs,fsx/;
  $opt{e} =~ s/:text/org,txt,htm,html,mak,csproj,sln,vcproj,proj,bat,zsh,config,xslt,xsl,css,asp,xml,xsl,xslt,sql/;
  $opt{e} =~ s/,/\|/g;
  $regexp_e .= '(\.(?:'.$opt{e}.')$)';
} else {
#  print "no -e option\n";
}
#$regexp_e ||= ".";
$regexp = ($regexp ? "(?:$regexp)|":"") . $regexp_e if defined $regexp_e;
#print "regexp: \"$regexp\"\n";
$opt{i} = $opt{i} ? "i":"";
$opt{g} ||= "";
$opt{p} ||= "grep";
$opt{m} ||= "";

$pattern = shift @ARGV or usage();

if ($pattern =~ /^[a-z0-9_.|()\[\]\*\+-]+$/i) {
    my $hilock = $pattern;
    $hilock =~ s/ ([|]) /\\\\$1/gx;
    print "  Hi"."-lock: ((\":.*\\\\($hilock\\\\)\" (1 (quote hi-pink) t)))\n";
}
# custom hi-lock patterns for major-mode's
if ($opt{m} eq 'c++-mode') {
 print '  Hi'.'-lock: ((":[ ]*class[ ]+\\\\([^ ]+\\\\)" (1 (quote font-lock-type-face) t)))'."\n";
 print '  Hi'.'-lock: ((":[ ]*\\\\(//.*\\\\)" (1 (quote font-lock-comment-face) t)))'."\n";
}
if ($opt{m} eq 'emacs-lisp-mode') {
 print '  Hi'.'-lock: ((":[ ]*\\\\(;.*\\\\)" (1 (quote font-lock-comment-face) t)))'."\n";
}

# Change filename to basename
sub basename { $_[0] =~ s!^.*[/\\]!!; $_[0] };

my ($dir, $bdir, @filelist, $files, $output);

my @dirs = @ARGV;
while (@dirs) {
    $dir = shift @dirs;
    next if $dir =~ m!/(CVS|RCS|_darcs)(/|$)!;
    next if ($dir eq "NUL");                      # windows quirk
    $dir =~ s/%20/ /g;                            # convert to spaces

    if ($dir eq '-r') {
        $dir = shift @dirs;
#        print "dir: $dir\n";
        $dir =~s/^~/$ENV{HOME}/;
        chdir $dir or warn "chdir $dir failed";
        # Recursively add subdirectories to @dirs
        find sub {-d $_ and unshift @dirs, $File::Find::name}, $dir;
        next;
    }
    $dir =~ s/[\/\\]$//;                          # remove trailing slash
    ($bdir = $dir) =~ s/ /\\ /g;                  # add backslash before space
    # FIXME don't apply $regexp to path in next line !
    @filelist = map {basename($_)} grep { /$regexp/i && -f } glob("$bdir/*");
#    print "bdir : $bdir (",(scalar @filelist),")\n";
    if (@filelist) {
	    my $edir = $dir;
	    $edir =~s/^~/$ENV{HOME}/;
        if (not chdir $edir) {
            warn "cannot chdir $edir: $!" unless $opt{s};
        } else {
#            print join(" ", @filelist) if $bdir =~ m/lucene/i;
            $files = join(" ", map { "\"$_\"" } @filelist);
            $pattern =~ s/^-/\\-/;
            $output =  `$opt{p} -$opt{i}$opt{g}Hn \"$pattern\" $files`;
            if ($output) {
#                print "\nDir: $dir\n" . $output;
                print "\nEntering directory `$dir'\n" . $output;
				print "Leaving directory `$dir'\n";
                $exitcode = 0 if ($?==0);         # set to zero if successful
            }
        }
    }
}
END {
   $? = $exitcode;
}


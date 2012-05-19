#! /usr/local/bin/perl -sw
use strict;
use Cwd;

######################################################################

### CONFIG
my $verbose = 0;

my @g_include_directories =
  (".", "/usr/include"
#   "c:/tools/milbin/boost_v1.30.2"
  );

######################################################################

my $filename = shift @ARGV or die "filename needed as first argument\n";
my $grepword = shift @ARGV or die "need word to search for (is text selected?)";

my %seen;

if ($ENV{EMACS})
{
  print '  Hi-lock: ((":[ ]*class[ ]+\\\\([^ ]+\\\\)" (1 (quote font-lock-type-face) t)))'."\n";
  print '  Hi-lock: ((":[ ]*\\\\(//.*\\\\)" (1 (quote font-lock-comment-face) t)))'."\n";
}

cpp_grep($filename, 0, ".");

# Recursively grep for in filename in @g_include_directories
# args: filename, function call level, current directory
# The latter two args are by the recursion, set them to 0 and "." initially
sub cpp_grep
{
  my ($filename, $level, $cwd) = @_;
  my ($fh, $line);
  if (not open $fh, "<", $filename)
  {
    print "$filename not found";
    return;
  }
  while ($line = <$fh>)
  {
    if ($line =~ m/^\s* \# \s* include \s* ([<\"]) ([^>\"]+) ([>\"])/x)
    {
      my $filename = $2;

      if ($filename =~ m/\b(list|string|vector|list|map)\b[^.]/)
      {
        print "SKIP $filename\n"  if $verbose >= 1;
        next;
       }

      print "  " x $level, "* $filename\n"  if $verbose >= 1;
      my @include_files = find_include_files($filename, $cwd);
      for my $include_file (@include_files)
      {
        if ($seen{$include_file})
        {
          print "already seen $include_file\n" if $verbose >= 2;
          next;                         # skip it
        }
        $seen{$include_file}++;
        my $output = `grep -n \"$grepword\" $include_file`;
        if ($output) {
          my $line_number = 0;
          if ($output =~ m/^(\d+)/)
          {
            $line_number = $1;
          }
          print "\n$include_file:$line_number: \n";
          print $output;
        }
        my $dir = $include_file;
        $dir =~ s/\/[^\/]+$//;
        cpp_grep($include_file, $level+1, $dir);
      }
      if (not @include_files)
      {
        print "# $filename not found\n" if $verbose;
      }
    }
  }
  close $fh;
}


sub find_include_files
{
  my ($filename, $cwd) = @_;
  print "FIND $filename in $cwd\n" if $cwd and $verbose >= 3;

  my @result;

  for my $include_directory (@g_include_directories)
  {
    my $fn = "${include_directory}/$filename";
    push @result, $fn if -e $fn;
  }

  # Not in include path. maybe it is next to the parent include
  if (not @result and $cwd and -e "$cwd/$filename")
  {
    push @result, "$cwd/$filename";
    print "LOCAL $cwd / $filename" if $verbose >= 2;
  }

  return @result;
}

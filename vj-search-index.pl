# Vagn Johansen 2007 (C)
use DB_File;
use strict;
my $inputSymbol = shift @ARGV;
my $symbol = lc($inputSymbol);
my @filenames = @ARGV or die "Usage: perl $0 symbol db-filename(s)\n";
my $no_matches = 1;

print "  Hi"."-lock: ((\":.*\\\\($inputSymbol\\\\)\" (1 (quote 'vj-grep-match) t)))\n";
#           ^---- split to avoid problem when viewing this file in Emacs

for my $filename (@filenames)
{
  my %database;
  tie %database, 'DB_File', $filename,O_RDWR, 0666, $DB_BTREE;
  %database or die "$0: tie $filename failed\n\nUse M-x vps-make-index RET\n\n";

  $database{$symbol} or do {print "\nSymbol $symbol not in $filename\n"; next;};
  $no_matches = 0;
  my @ids = split(/-/, $database{$symbol});
  @ids = splice(@ids, 1);               # skip first
  my %unique_ids;
  @unique_ids{@ids} = (1) x @ids;

  my %filenames_by_dir;
  for (map { $database{"-$_"} } keys %unique_ids) {
    m!^(.*)/([^/]*)$!;
    push @{$filenames_by_dir{$1}}, $2;
  }
  for my $dir (keys %filenames_by_dir) {
    my $filenames= join(" ",@{$filenames_by_dir{$dir}});
    print "\nEntering directory `$dir'\n";
    chdir $dir or warn "chdir $dir failed: $!";
    my $output = `grep -niH \"\\b$symbol\\b\" $filenames`;
    print $output;
    if ($output eq "") {
      print "$dir:0: Stale index: $filenames\ngrep *\n";
      my $output = `grep -niH \"\\b$symbol\\b\" *`;
      print $output;
    }
    print "Leaving directory `$dir'\n";
  }

  untie %database;
}

exit $no_matches;

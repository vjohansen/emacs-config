# Vagn Johansen 2013 (C)

# Usage: perl $0 <word-prefix [space substring]*> <project names..>

use DB_File;
use strict;
my $debug = 0;
my $inputSymbol = shift @ARGV;
my @otherWords;
($inputSymbol, @otherWords) = split /\s+/, $inputSymbol;
my $symbol = lc($inputSymbol);
my @filenames = @ARGV or die "Usage: perl $0 symbol vps-project-name(s)\n";

use Data::Dump qw(pp);

my %filenames;
for my $dbfilename (@filenames)
{
  my %database;
  my $db = tie %database, 'DB_File', $dbfilename, O_CREAT|O_RDWR, 0666, $DB_BTREE or die "tie failed";
  %database or die "$0: tie $dbfilename failed\n\nUse M-x vps-make-index RET\n\n";

  my $key = $inputSymbol;
  my $value = '?';
  my $status = $db->seq($key, $value, R_CURSOR());
  my @words;
  my $hits = 0;
  if ($key !~ /^-/) {
    do {
      my @ids = split(/-/, $database{$key});
      shift @ids;
      @ids = map { $filenames{$database{"-$_"}}++; $database{"-$_"} } @ids;
      pp($key, $status, $value, [@ids]) if $debug;
      push @words, $key;
      last unless $hits++ < 200;
      $status = $db->seq($key, $value, R_NEXT());
    } while ($key =~ m/^\Q$inputSymbol\E/);
  }
  pp(\@words) if $debug;

  undef $db;
  untie %database;
}

my $filenames = join(" ", keys %filenames);
pp(\%filenames) if $debug;
if ($filenames) {
  my $cmd = "grep -IHni $inputSymbol $filenames";
  print $cmd if $debug;

  while (@otherWords) {
    $cmd .= " | grep -i $otherWords[0]";
    shift @otherWords;
  }

  system($cmd);
  exit $? >> 8;
}

exit 1;

use DB_File;
use strict;

my $dbfilename = shift @ARGV || die "Usage: $0 vps-project-name";
my $dir = "$ENV{HOME}/.emacs.d/vps";
my %database;
tie %database,  'DB_File', $dbfilename, O_CREAT|O_RDWR, 0666, $DB_BTREE;
(my $filelist = $dbfilename) =~ s/\.db$/.txt/;
open FILELIST, "<", $filelist or die "open failed: $filelist : $!";
my $file_id = 0;

my $filename;
FILE: while ($filename = <FILELIST>)
{
  chomp $filename;
  next unless $filename =~ /\w/;
  open FILE, "<", $filename or do { warn "open failed: $filename : $!\n";
                                    next FILE };
  $database{'-'.++$file_id} = $filename; # Assign ID to the file
  my $text = join("", <FILE>);
  # Create hash with all the words
  my %tmp; 
  while ($text =~ m/([a-z_][a-zA0-9_-]{4,})/gi) # Ignore short words
  {
    $tmp{lc($1)}++;
  }
  for my $word (keys %tmp){
    $database{$word} .= "-$file_id"; # Update all the words with the file ID
  }
  close FILE;
}
close FILELIST;
untie %database;


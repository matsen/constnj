#! /usr/bin/env perl

$|++; #immediately flush buffer

use strict;

foreach my $fname (@ARGV) {

  my $base = `basename $fname .phy`;
  chomp $base;

  print `phyml $fname 0 s 1 0 F84 e e 4 e BIONJ y y`;

}

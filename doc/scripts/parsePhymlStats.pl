#! /usr/bin/env perl

use strict;

foreach my $fname ( @ARGV ) {
   
  chomp $fname;
  print "checking out $fname...\n";
  open IN, "$fname";

  foreach my $line ( <IN> ) {
    if($line =~ /transversion ratio/) {
      my ($tsTv) = $line =~ /transversion ratio : ([\d.]*)/ or die;
      my $halfTv = $tsTv / 2;
      print "half tv : $halfTv\n";
    }
    elsif($line =~ /Gamma shape parameter/) {
      my ($gam) = $line =~ /Gamma shape parameter : ([\d.]*)/ or die;
      my $cv = 1/(sqrt $gam);
      print "coeff of var of gam: $cv\n";
    }
  }

  print "\n";

  close IN;

}

#!/usr/bin/perl

while(<>) {
  s/\r//;
  s/\n//;
  /DROP (.*)/ and $ax = $1;
  /^time:\s*([0-9.]+) secs/ and $times{$ax} .= "$1,";
  /^num\. qa\. inst:\s*(\d+)/ and $inst{$ax} .= "$1,";
}

foreach my $k (keys %times) {
  my @tm = sort (split /,/, $times{$k});
  my @in = sort (split /,/, $inst{$k});
  my $kk = $k;
  $kk =~ s/,/./g;
  $kk =~ s/^axiom //;
  
  printf "%8.3f , %6d , %s , %s , %s\n", $tm[@tm / 2], $in[@in / 2], (join ",", @tm), (join ",", @in), $kk;
}

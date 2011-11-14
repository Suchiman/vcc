#!/usr/bin/perl

while(<>) {
  if (/const unique\s*([^\s:]+)\s*:\s*([^\s;]+)/ ) {
    $t{$1} = $2;
  }
}

foreach my $x (values %t) {
  $x =~ /\$token/ and next;
  my @v = ();
  foreach my $k (keys %t) {
    if ($t{$k} eq $x) {
      push @v, $k;
    }
  }
  for (my $i = 0; $i < @v; $i++) {
    for (my $j = $i + 1; $j < @v; $j++) {
      print "axiom $v[$i] != $v[$j];\r\n";
    }
  }
}

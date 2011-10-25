#!/usr/bin/perl


$rs = shift;

$opts = "";

while (1) {
  $_ = $ARGV[0];
  if (/^-/ or /^\//) {
    $opts .= "$_ ";
    shift;
  } else {
    last;
  }
}

print "OPTS $opts\n";

while (<>) {
push @lines, $_;
/^axiom/ and $ax++;
}

print "$ax axioms\n";

for($i = 0; $i < @lines; ++$i) {
  $_ = $lines[$i];
  /^axiom/ or next;
  open(F, "> tmp.bpl");
  for($j = 0; $j < @lines; ++$j) {
    print F $lines[$j] if $i != $j;
  }
  close(F);
  $ok = 0;
  open(B, "Boogie /z3opt:CASE_SPLIT=5 /typeEncoding:m /z3opt:/t:20 $opts tmp.bpl /proverLog:tmp.sx |");
  while(<B>){
    /  verified/ and $ok = 1;
  }
  close(B);

  $_ = $lines[$i];
  s/\r//;
  s/\n//;

  print "DROP " if $ok;
  print "NEED " if !$ok;
  print "$_\n";

  next if !$ok;

  system("sh subrun3 $rs tmp.sx");
}

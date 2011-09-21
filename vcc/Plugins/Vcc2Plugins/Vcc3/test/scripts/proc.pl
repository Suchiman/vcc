while (<>) {
s/\r//;
s/\n//;
/RUN (\S+) CS=(\d) RS=(\d+)/ and $k = "$1 CS-$2";
/RUN (\d+) RS=(\d+)/ and $k = sprintf "%03d", $1;
/time:\s*([\d\.]+)/ and $r{"time $k"} .= "," . int($1*1000);
/num\. conflicts:\s*([\d]+)/ and $r{"cnfl $k"} .= ",$1";
/num\. qa\. inst:\s*([\d]+)/ and $r{"inst $k"} .= ",$1";
}

printf "%20s,%8s,  dev,  data...\n", "key", "avg";

for my $k (sort (keys %r)) {
  my $v = $r{$k};
  $v =~ s/^,//;
  my $cnt = 0;
  my $sum = 0;
  my $min= -1;
  my $max = 0;
  my $r = "";

  foreach my $n (split /,/, $v) {
    $sum += $n;
    $max = $n if($n > $max);
    $min = $n if($min == -1  || $n < $min);
    $cnt++;
    $r .= sprintf "%8d,", $n;
  }

  next if ($cnt == 0) ;
  $sum /= $cnt;
  $dev = 100 * ($max - $min) / $sum;
  printf "%20s,%8d,%5d,%s\n", $k, $sum, $dev, $r;
}

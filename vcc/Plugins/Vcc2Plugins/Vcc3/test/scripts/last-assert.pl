#!/usr/bin/perl

$pre = "";

while(<>) {
  if ($done) {
    /^\s*(assume|assert)\s+/ and next;
    print;
    next;
  }

  if (/^\s*assert\s+/) {
    $pre =~ s/\bassert\b/   assume/g;
    print $pre;
    $pre = "";
  }

  $pre .= $_;

  if (/^\s*assume\s+false/) {
    print $pre;
    $done = 1;
  }
}


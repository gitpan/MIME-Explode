#!/usr/bin/perl -w

use strict;
use IO::File;
use MIME::Explode;
use Benchmark;

my $start = new Benchmark;

my $mail = shift(@ARGV) || die("no args");
die("Unable to open file \"$mail\"") unless(-e $mail);

my $output = "file.tmp";
my $explode = MIME::Explode->new(
	output_dir         => "tmp",
	mkdir              => 0755,
	check_content_type => 1,
	exclude_types      => ["image/gif", "image/jpeg"],
);

open(MAIL, "<$mail") or die("Couldn't open $mail for reading: $!\n");
open(OUTPUT, ">$output") or die("Couldn't open $output for writing: $!\n");
#my $headers = $explode->parse(\*MAIL);
my $headers = $explode->parse(\*MAIL, \*OUTPUT);
close(OUTPUT);
close(MAIL);


for my $msg (sort{ $a cmp $b } keys(%{$headers})) {
	print "---------------------------\n";
	for my $k (keys(%{$headers->{$msg}})) {
		if(ref($headers->{$msg}->{$k}) eq "ARRAY") {
			for my $i (0 .. $#{$headers->{$msg}->{$k}}) {
				print "$msg => $k => $i => " . $headers->{$msg}->{$k}->[$i] . "\n";
			}
		} elsif(ref($headers->{$msg}->{$k}) eq "HASH") {
			for my $ks (keys(%{$headers->{$msg}->{$k}})) {
				print "$msg => $k => $ks => " . $headers->{$msg}->{$k}->{$ks} . "\n";
			}
		} else {
			print "$msg => $k => " . $headers->{$msg}->{$k} . "\n";
		}
	}
}

my $finish = new Benchmark;
my $diff = timediff($finish, $start);
my $strtime = timestr($diff);
print STDERR "\n\nTime: $strtime\n";

exit(0);

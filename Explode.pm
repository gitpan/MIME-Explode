#
# Explode.pm
# Last Modification: Fri Dec 13 15:29:05 WET 2002
#
# Copyright (c) 2002 Henrique Dias <hdias@aesbuc.pt>. All rights reserved.
# This module is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.
#
package MIME::Explode;

use strict;
use Carp;

require Exporter;
require DynaLoader;
require AutoLoader;
use SelfLoader;

use vars qw($VERSION @ISA @EXPORT);

@ISA = qw(Exporter DynaLoader);
@EXPORT = qw(&rfc822_base64 &rfc822_qprint);
$VERSION = '0.14';

use constant BUFFSIZE => 64;

my %h_hash = (
	'content-type'              => "",
	'content-disposition'       => "",
	'content-transfer-encoding' => "",
);

my @patterns = (
	'^([^= ]+) *=[ \"]*([^\"]+)',
	'^(\w[\w\-]*): *([^\x0d\x0a\x09\f]*)[\x0d\x0a\x09\f]+',
	'^[\x0a\x0d]+$',
	'^begin\s*(\d\d\d)\s*(\S+)',
	'^From +[^ ]+ +[a-zA-Z]{3} +[^ ]{3} +\d{1,2} \d\d:\d\d:\d\d +\d{4}',
	'^[ \t]+(?=.*\w+)'
);

my %content_type = (
	"text/html"      => ".html",
	"text/plain"     => ".txt",
	"message/rfc822" => ".rfc822",
	"text/richtext"  => ".richtext",
);

SelfLoader->load_stubs();

sub new {
	my $proto = shift;
	my $class = ref($proto) || $proto;
	my $self  = {
		output_dir         => "/tmp",
		mkdir              => 0,
		decode_subject     => 0,
		check_content_type => 0,
		exclude_types      => [],
		@_,
	};
	bless ($self, $class);
	return($self);
}

sub parse {
	my $self = shift;

	local $/ = "\n";
	my %ctypes = ();
	my $headers = {};
	my $args = {
		output_dir     => $self->{output_dir},
		check_ctype    => $self->{check_content_type} || 0,
		decode_subject => $self->{decode_subject},
	};
	if(scalar(@{$self->{exclude_types}})) {
		@ctypes{@{$self->{exclude_types}}} = (0 .. $#{$self->{exclude_types}});
		$args->{ctypes} = \%ctypes;
	}
	if(!(-d $self->{output_dir}) && $self->{mkdir}) {
		mkdir($self->{output_dir}, $self->{mkdir}) or
			die("MIME::Explode: Failed to create directory \"" . $self->{output_dir} . "\" $!");
	}
	my $last = &_parse(\@_, 1, 0, "0", "", $args, {}, $headers);
	$self->{nmsgs} = ($last->[0]) ? (split(/\./, $last->[0]))[0] + 1 : 0;
	my ($fh_mail, $fh_tmp) = @_;
	if(defined($fh_tmp)) { while(<$fh_mail>) { print $fh_tmp $_; } }
	return($headers);
}

sub nmsgs { $_[0]->{'nmsgs'} }

sub _parse {
	my $fhs = shift;
	my $header = shift;
	my $mbox = shift || 0;
	my $base = shift || "0";
	my $origin = shift || "";
	my $args = shift;
	my $files = shift;

	my ($fh_mail, $fh_tmp) = @{$fhs};
	my ($tree, $key, $tmpbuff, $boundary, $ftmp) = (join("\.", $base, "0"), "", "", "", "");
	my ($check_ctype, $ctlength) = (1, 0);
	my ($ph, $tmp, $exclude, $uucount, $checkhdr) = (0, 0, 0, 0, 0);
	my $fh;
	while(<$fh_mail>) {
		defined($fh_tmp) and print $fh_tmp $_;
		if($header) {
			($ph, $uucount, $exclude, $tmpbuff, $check_ctype, $ctlength, $ftmp) = (1, 0, 0, "", 1, 0, "");
			if(!$mbox && $base eq "0" && /$patterns[4]/o) { $mbox = 1; next; }
			if(exists($_[0]->{$tree}->{$key})) {
				s/\x0d//og;
				if(s/$patterns[5]/ /o) {
					s/\s+$//o;
					if(ref($_[0]->{$tree}->{$key}) eq "ARRAY") {
						$_[0]->{$tree}->{$key}->[$#{$_[0]->{$tree}->{$key}}] .= $_;
						next;
					}
					if(ref($_[0]->{$tree}->{$key}) eq "HASH") { $_[0]->{$tree}->{$key}->{value} .= $_; }
					else { $_[0]->{$tree}->{$key} .= $_; }
					next;
				}
				if(exists($h_hash{$key}) && exists($_[0]->{$tree}->{$key}->{value})) {
					my @params = split(/ *; */o, $_[0]->{$tree}->{$key}->{value});
					$_[0]->{$tree}->{$key}->{value} = shift(@params) || "";
					map { /$patterns[0]/o and $_[0]->{$tree}->{$key}->{lc($1)} = $2; } @params;
				} elsif($key eq "subject" && $args->{decode_subject}) {
					my @parts = &decode_mimewords($_[0]->{$tree}->{subject});
					delete($_[0]->{$tree}->{subject});
					$_[0]->{$tree}->{subject}->{value} = [map {$_->[0]} @parts];
					$_[0]->{$tree}->{subject}->{charset} = [map {$_->[1] || "us-ascii"} @parts];
				}
			}
			if(/$patterns[1]/o) {
				defined($fh) and &file_close($fh);
				($header, $checkhdr) = (1, 1);
				$key = lc($1);
				if($key eq "received" || $key eq "x-received") {
					push(@{$_[0]->{$tree}->{$key}}, $2);
					next;
				}
				unless(exists($_[0]->{$tree}->{$key})) {
					$_[0]->{$tree}->{$key} = (exists($h_hash{$key})) ? {value => $2} : $2;
				}
				next;
			}
			next if(!$checkhdr && (length() <= 2) && /$patterns[2]/o);
			$header = 0;
			if(exists($_[0]->{$tree}->{'content-type'}) && exists($_[0]->{$tree}->{'content-type'}->{value})) {
				$_[0]->{$tree}->{'content-type'}->{value} = lc($_[0]->{$tree}->{'content-type'}->{value});
				if(exists($_[0]->{$tree}->{'content-type'}->{boundary}) && $_[0]->{$tree}->{'content-type'}->{value} =~ /multipart\/\w+/o) {
					my $res = &_parse($fhs, $header, $mbox, $tree, $_[0]->{$tree}->{'content-type'}->{boundary}, $args, $files, $_[0]);
					if($res->[1]) {
						if($mbox) { $tmp = 1; }
						else { return([$tree, $res->[1]]); }
						$_ = $res->[1];
					} else { next; }
				} elsif($_[0]->{$tree}->{'content-type'}->{value} eq "message/rfc822") {
					my $res = &_parse($fhs, 1, $mbox, $tree, $origin, $args, $files, $_[0]);
					if($res->[1]) { 
						if($mbox) { $tmp = 1; }
						else { return([$tree, $res->[1]]); }
						$_ = $res->[1];
					} else { next; }
				}
			}
		}
		$checkhdr = 0;
		$key = "";
		!defined($_) and next;
		if(/$patterns[3]/o) {
			my $file = &check_filename($files, $2);
			my $filepath = ($args->{output_dir}) ? join("/", $args->{output_dir}, $file) : $file;
			my $res = uu_file($fhs, $filepath, $1 || "644", $args->{ctypes});
			$_[0]->{"$tree.$uucount"}->{'content-type'}->{value} = $res->[0];
			$_[0]->{"$tree.$uucount"}->{'content-disposition'}->{filepath} = $filepath unless($res->[1]);
			$uucount++;
			next;
		}
		my $breakmsg = "";
		unless(defined($fh)) {
			$boundary = $origin;
			if(exists($_[0]->{$tree}->{'content-type'}) && exists($_[0]->{$tree}->{'content-type'}->{value})) {
				$exclude = 1 if(($_[0]->{$tree}->{'content-type'}->{value} =~ /^multipart\/\w+$/o) || ($_[0]->{$tree}->{'content-type'}->{value} eq "message/rfc822"));
			} else { $check_ctype = 1; }
			unless($exclude) {
				if(exists($_[0]->{$tree}->{'content-transfer-encoding'}) &&
						exists($_[0]->{$tree}->{'content-transfer-encoding'}->{value})) {
					$_[0]->{$tree}->{'content-transfer-encoding'}->{value} = lc($_[0]->{$tree}->{'content-transfer-encoding'}->{value});
					if($_[0]->{$tree}->{'content-transfer-encoding'}->{value} eq "base64" ||
							($_[0]->{$tree}->{'content-transfer-encoding'}->{value} eq "quoted-printable" && $boundary)) {
						&set_filename($files, $_[0]->{$tree});
						my $filepath = ($args->{output_dir}) ? join("/", $args->{output_dir}, $_[0]->{$tree}->{'content-disposition'}->{filename}) : $_[0]->{$tree}->{'content-disposition'}->{filename};
						my $res = &decode_content($fhs, $_[0]->{$tree}->{'content-transfer-encoding'}->{value}, $filepath, $args->{check_ctype}, $_[0]->{$tree}->{'content-type'}->{value} || "", "--$boundary", $args->{ctypes});
						$_[0]->{$tree}->{'content-type'}->{value} = $res->[1] if($res->[1]);
						$_[0]->{$tree}->{'content-disposition'}->{filepath} = $filepath unless($res->[2]);
						$tmp = 1;
						unless($_ = $res->[0]) {
							$exclude = 1;
							next;
						}
						if($mbox && /$patterns[4]/o && scalar(@{[split(/\./o, $tree)]}) > 2) {
							$breakmsg = $_;
							$_ = "--$boundary--\r\n";
						}
					}
				}
			}
		}
		if($mbox && $tmp && /$patterns[4]/o) {
			if(scalar(@{[split(/\./o, $tree)]}) > 2) {
				$breakmsg = $_;
				$_ = "--$boundary--\r\n";
			} else {
				defined($fh) and &file_close($fh);
				$header = 1;
				my @ps = split(/\./o, $tree);
				$tree = join(".", ++$ps[0], "0");
				next;
			}
		}
		$tmp = ((length() <= 2) && /$patterns[2]/o) ? 1 : 0;
		next if(!defined($fh) && $tmp);
		if($boundary) {
			if(index($_, "--$boundary--") >= 0) {
				defined($fh) and &file_close($fh);
				if($mbox && scalar(@{[split(/\./o, $tree)]}) == 2) {
					($tmp, $exclude) = (1, 1);
					$boundary = "";
					next;
				} else { return([$tree, $breakmsg]); }
			}
			if(index($_, "--$boundary") >= 0) {
				defined($fh) and &file_close($fh);
				($tmp, $header) = (1, 1);
				$boundary = "";
				if($ph) {
					my @ps = split(/\./o, $tree);
					$ps[$#ps]++;
					$tree = join("\.", @ps);
				}
				next;
			}
		}
		($exclude || !$ph) and next;
		if($check_ctype && $args->{check_ctype}) {
			($tmpbuff .= $_) =~ s/^[\n\r\t]+//o;
			if(length($tmpbuff) > BUFFSIZE) {
				if(my $ct = set_content_type($tmpbuff, $_[0]->{$tree}->{'content-type'}->{value} || "")) {
					$_[0]->{$tree}->{'content-type'}->{value} = $ct;
					$tmpbuff = "";
					$check_ctype = 0;
				}
				if(exists($args->{ctypes}) &&
						exists($args->{ctypes}->{$_[0]->{$tree}->{'content-type'}->{value}})) {
					if(defined($fh)) {
						&file_close($fh);
						unlink($_[0]->{$tree}->{'content-disposition'}->{filepath});
						delete($_[0]->{$tree}->{'content-disposition'}->{filepath});
					}
					$exclude = 1;
					next;
				}
			}
		}
		unless(defined($fh)) {
			&set_filename($files, $_[0]->{$tree});
			$_[0]->{$tree}->{'content-disposition'}->{filepath} = ($args->{output_dir}) ?
				join("/", $args->{output_dir}, $_[0]->{$tree}->{'content-disposition'}->{filename}) :
					$_[0]->{$tree}->{'content-disposition'}->{filename};
			defined($fh) and &file_close($fh);
			$fh = &file_open($_[0]->{$tree}->{'content-disposition'}->{filepath});
		}
		if(defined($fh)) {
			if(!$ftmp && (length() <= 2) && /$patterns[2]/o) {
				$ftmp .= $_;
				next;
			}
			if($ftmp) {
				$_ = join("", $ftmp, $_);
				$ftmp = "";
			}
			print $fh ($_[0]->{$tree}->{'content-transfer-encoding'}->{value} eq "quoted-printable") ? rfc822_qprint($_) : $_;
			next unless(exists($_[0]->{$tree}->{'content-length'}));
			if(($ctlength += length()) >= $_[0]->{$tree}->{'content-length'}) {
				defined($fh) and &file_close($fh);
				$exclude = 1;
				next;
			}
		}
	}
	defined($fh) and &file_close($fh);
	return([$tree, ""]);
}

sub file_close {
	close($_[0]);
	undef($_[0]);
}

sub file_open {
	my $path = shift;
	local *FILE;
	if($path =~ /^(.+)$/) { $path = $1; }
	open(FILE, ">$path") or
		die("MIME::Explode: Couldn't open $path for writing: $!\n");
	binmode(FILE);
	return *FILE;
}

sub set_filename {
	my $files = shift;
	my $h = shift;

	my $file = "file";
	if(exists($h->{'content-disposition'}->{filename})) {
		$file = $h->{'content-disposition'}->{filename};
	} elsif(exists($h->{'content-type'}->{name})) {
		$file = $h->{'content-type'}->{name};
	} elsif(exists($h->{'content-type'}->{value})) {
		my $ctype = lc($h->{'content-type'}->{value});
		$file .= $content_type{$ctype} || "";
	}
	$h->{'content-disposition'}->{filename} = &check_filename($files, $file);
	$h->{'content-transfer-encoding'}->{value} = "" unless(exists($h->{'content-transfer-encoding'}->{value}));

	return();
}

bootstrap MIME::Explode $VERSION;

1;

__DATA__

sub check_filename {
	my $files = shift;
	my $rawfile = shift;

	my $file = &decode_mimewords($rawfile);
	$file =~ /[\/\\]?([^\/\\]+)$/o;
	$file = (length($1)) ? $1 : "file";
	if(exists($files->{$file})) {
		my $n = $files->{$file}++;
		$file .= "-$n" unless($file =~ s/(\.[^\.]+)$/\-$n$1/o);
	} else { $files->{$file} = 1; }

	return($file);
}

sub decode_mimewords {
	my $encstr = shift;
	my @tokens;
	$@ = '';
	$encstr =~ s/(\?\=)\r?\n[ \t](\=\?)/$1$2/ogs;
	pos($encstr) = 0;
	my($charset, $encoding, $enc, $dec);
	while (1) {
		last if(pos($encstr) >= length($encstr));
		my $pos = pos($encstr);
		if($encstr =~ /\G=\?([^?]*)\?([bq])\?([^?]+)\?=/ogi) {
			($charset, $encoding, $enc) = ($1, lc($2), $3);
			$dec = ($encoding eq "q") ? rfc822_qprint($enc) : rfc822_base64($enc);
			push(@tokens, [$dec, $charset]);
			next;
		}
		pos($encstr) = $pos;
		if($encstr =~ /\G=\?/g) {
			$@ .= qq|unterminated "=?..?..?=" in "$encstr" (pos $pos)\n|;
			push(@tokens, ['=?']);
			next;
		}
		pos($encstr) = $pos;
		if($encstr =~ /\G([\x00-\xFF]*?\n*)(?=(\Z|=\?))/og) {
			length($1) or die("MIME::Explode: internal logic err: empty token\n");
			push(@tokens, [$1]);
			next;
		}
		die("MIME::Explode: unexpected case:\n($encstr) pos $pos\n");
	}
	return (wantarray ? @tokens : join('',map {$_->[0]} @tokens));
}

__END__

=head1 NAME

MIME::Explode - Perl extension for explode MIME messages

=head1 SYNOPSIS

  use MIME::Explode;

  my $explode = MIME::Explode->new(
    output_dir         => "tmp",
    mkdir              => 0755,
    decode_subject     => 1,
    check_content_type => 1,
    exclude_types      => ["image/gif", "image/jpeg", "image/png"],
  );

  print "Number of messages: ", $explode->nmsgs, "\n";

  open(MAIL, "<file.mbox") or
	die("Couldn't open file.mbox for reading: $!\n");
  open(OUTPUT, ">file.tmp")
	or die("Couldn't open file.tmp for writing: $!\n");
  my $headers = $explode->parse(\*MAIL, \*OUTPUT);
  close(OUTPUT);
  close(MAIL);

  for my $part (sort{ $a cmp $b } keys(%{$headers})) {
    for my $k (keys(%{$headers->{$part}})) {
      if(ref($headers->{$part}->{$k}) eq "ARRAY") {
        for my $i (0 .. $#{$headers->{$part}->{$k}}) {
          print "$part => $k => $i => ", $headers->{$part}->{$k}->[$i], "\n";
        }
      } elsif(ref($headers->{$part}->{$k}) eq "HASH") {
        for my $ks (keys(%{$headers->{$part}->{$k}})) {
          if(ref($headers->{$part}->{$k}->{$ks}) eq "ARRAY") {
            print "$part => $k => $ks => ", join(($ks eq "charset") ? " " : "", @{$headers->{$part}->{$k}->{$ks}}), "\n";
          } else {
            print "$part => $k => $ks => ", $headers->{$part}->{$k}->{$ks}, "\n";
          }
          print "$part => $k => $ks => ", $headers->{$part}->{$k}->{$ks}, "\n";
        }
      } else {
        print "$part => $k => ", $headers->{$part}->{$k}, "\n";
      }
    }
  }

=head1 DESCRIPTION

MIME::Explode is perl module for parsing and decoding single or multipart
MIME messages, and outputting its decoded components to a given directory
ie, this module is designed to allows users to extract the attached files
out of a MIME encoded email messages or mailboxes.

=head1 METHODS

=head2 new([, OPTION ...])

This method create a new MIME::Explode object. The following keys are
available:

=over 5

=item output_dir

directory where the decoded files are placed

=item mkdir => octal_number

if the value is set to octal number then make the output_dir directory
(example: mkdir => 0755).

=item check_content_type => 0 or 1

if the value is set to 1 the content-type of file is checked

=item decode_subject => 0 or 1

if the value is set to 1 then the subject is decoded into a list.

  $header->{'0.0'}->{subject}->{value} = [ARRAYREF];
  $header->{'0.0'}->{subject}->{charset} = [ARRAYREF];
  $subject = join("", @{$header->{'0.0'}->{subject}->{value}});

=item exclude_types => [ARRAYREF]

not save files with specified content types

=back

=head2 parse(FILEHANDLE, FILEHANDLE)

This method parse the stream and splits it into its component entities.
This method return a hash reference with all parts. The FILEHANDLE should
be a reference to a GLOB. The second argument is optional.


=head2 nmsgs()

Returns the number of parsed messages.

=head1 AUTHOR

Henrique Dias <hdias@esb.ucp.pt>

=head1 SEE ALSO

MIME::Tools, perl(1).

=cut

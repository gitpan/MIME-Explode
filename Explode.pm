#
# Scan.pm
# Last Modification: Sat Jul 20 17:36:42 WEST 2002
#
# Copyright (c) 2002 Henrique Dias <hdias@esb.ucp.pt>. All rights reserved.
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

use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);

@ISA = qw(Exporter DynaLoader);
@EXPORT = qw();

$VERSION = '0.01';

use constant FILEISTEXT => 20;
use constant FILEISHTML => 4;
use constant BUFFSIZE   => 64;

my %h_hash = (
	'content-type'              => "",
	'content-disposition'       => "",
	'content-transfer-encoding' => "",
);

my @patterns = (
	'^([^=]+) ?=[ \"]*([^\"]+)',
	'^(\w[\w\-]+): *([^\x0d\x0a\x09\f]*)[\x0d\x0a\x09\f]+',
	'^[\n\r]$',
	'^begin\s*(\d\d\d)\s*(\S+)',
	'^From +[^ ]+ +[a-zA-Z]{3} +[^ ]{3} +\d{1,2} \d\d:\d\d:\d\d +\d{4}',
	'^[ \t]+(?=.*\w+)',
	'^[\n\r]+$',
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
		output_dir  => $self->{output_dir},
		check_ctype => $self->{check_content_type} || 0,
	};
	if(scalar(@{$self->{exclude_types}})) {
		@ctypes{@{$self->{exclude_types}}} = (0 .. $#{$self->{exclude_types}});
		$args->{ctypes} = \%ctypes;
	}
	if(!(-d $self->{output_dir}) && $self->{mkdir}) {
		mkdir($self->{output_dir}, $self->{mkdir}) or
			die("MIME::Explode: Failed to create directory \"" . $self->{output_dir} . "\" $!");
	}
	&_parse(\@_, "0.0", "", $args, $headers);
	my ($fh_mail, $fh_tmp) = @_;
	if(defined($fh_tmp)) { while(<$fh_mail>) { print $fh_tmp $_; } }
	return($headers);
}

sub _parse {
	my $fhs = shift;
	my $base = shift || "0.0";
	my $origin = shift || "";
	my $args = shift;

	my %files = ();
	my ($fh_mail, $fh_tmp) = @{$fhs};
	my ($tree, $key, $tmpbuff, $boundary, $ftmp) = ("", "", "", "", "");
	my ($header, $check_ctype, $ctlength) = (1, 1, 0);
	my ($tmp, $exclude, $uucount, $checkhdr, $mbox) = (0, 0, 0, 0, 0);
	my $fh;

	while(<$fh_mail>) {
		defined($fh_tmp) and print $fh_tmp $_;
		if($header) {
			($uucount, $exclude, $tmpbuff, $check_ctype, $ctlength, $ftmp) = (0, 0, "", 1, 0, "");
			if(!$mbox && !$tree && /$patterns[4]/o) { $mbox = 1; next; }
			if($tree && exists($_[0]->{$tree}->{$key})) {
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
				}
			}
			if(/$patterns[1]/o) {
				defined($fh) and &file_close($fh);
				($header, $checkhdr) = (1, 1);
				$key = lc($1);
				$tree ||= $base;
				if($key eq "received") {
					push(@{$_[0]->{$tree}->{$key}}, $2);
					next;
				}
				unless(exists($_[0]->{$tree}->{$key})) {
					$_[0]->{$tree}->{$key} = (exists($h_hash{$key})) ? {value => $2} : $2;
				}
				next;
			}
			next if(!$checkhdr && (length() <= 2) && /$patterns[6]/o);
			$header = 0;
			if($tree) {
				if(exists($_[0]->{$tree}->{'content-type'}) && exists($_[0]->{$tree}->{'content-type'}->{value})) {
					$_[0]->{$tree}->{'content-type'}->{value} = lc($_[0]->{$tree}->{'content-type'}->{value});
					if($_[0]->{$tree}->{'content-type'}->{value} =~ /multipart\/\w+/o) {
						if(exists($_[0]->{$base}->{'content-type'}) && exists($_[0]->{$base}->{'content-type'}->{boundary}) &&
								($_[0]->{$base}->{'content-type'}->{boundary} ne $_[0]->{$tree}->{'content-type'}->{boundary})) {
							$_[0]->{"$tree.0"}->{'content-type'}->{boundary} = $_[0]->{$tree}->{'content-type'}->{boundary};
							$_[0]->{"$tree.0"}->{'content-type'}->{value} = $_[0]->{$tree}->{'content-type'}->{value};
							&_parse($fhs, "$tree.0", "", $args, $_[0]);
						}
						next;
					}
					if($_[0]->{$tree}->{'content-type'}->{value} eq "message/rfc822") {
						&_parse($fhs, "$tree.0", $_[0]->{$base}->{'content-type'}->{boundary}, $args, $_[0]);
					}
				}
			} else {
				defined($fh) and &file_close($fh);
				$tree = $base;
			}
		}
		$checkhdr = 0;
		$key = "";
		!defined($_) and next;
		if(/$patterns[3]/o) {
			my $file = &check_filename(\%files, $2);
			my $filepath = ($args->{output_dir}) ? join("/", $args->{output_dir}, $file) : $file;
			my $res = uu_file($fhs, $filepath, $1 || "644", $args->{ctypes});
			$_[0]->{"$tree.$uucount"}->{'content-type'}->{value} = $res->[0];
			$_[0]->{"$tree.$uucount"}->{'content-disposition'}->{filepath} = $filepath unless($res->[1]);
			$uucount++;
			next;
		}
		unless(defined($fh)) {
			unless($exclude) {
				$boundary = (exists($_[0]->{$base}->{'content-type'}->{boundary})) ? $_[0]->{$base}->{'content-type'}->{boundary} : "";
			}
			if(exists($_[0]->{$tree}->{'content-type'}) && exists($_[0]->{$tree}->{'content-type'}->{value})) {
				$exclude = 1 if(($_[0]->{$tree}->{'content-type'}->{value} =~ /^multipart\/\w+$/o) || ($_[0]->{$tree}->{'content-type'}->{value} eq "message/rfc822"));
			} else { $check_ctype = 1; }
			unless($exclude) {
				if(exists($_[0]->{$tree}->{'content-transfer-encoding'}) &&
						exists($_[0]->{$tree}->{'content-transfer-encoding'}->{value})) {
					$_[0]->{$tree}->{'content-transfer-encoding'}->{value} = lc($_[0]->{$tree}->{'content-transfer-encoding'}->{value});
					if($_[0]->{$tree}->{'content-transfer-encoding'}->{value} eq "base64") {
						&set_filename(\%files, $_[0]->{$tree});
						my $filepath = ($args->{output_dir}) ? join("/", $args->{output_dir}, $_[0]->{$tree}->{'content-disposition'}->{filename}) : $_[0]->{$tree}->{'content-disposition'}->{filename};
						my $res = &base64_file($fhs, $filepath, $args->{check_ctype}, $_[0]->{$tree}->{'content-type'}->{value}, "--$boundary", $args->{ctypes});
						$_[0]->{$tree}->{'content-type'}->{value} = $res->[1] if($res->[1]);
						$_[0]->{$tree}->{'content-disposition'}->{filepath} = $filepath unless($res->[2]);
						unless($_ = $res->[0]) {
							$exclude = 1;
							next;
						}
					}
				}
			}
		}
		if($mbox && $tmp && /$patterns[4]/o) {
			defined($fh) and &file_close($fh);
			$header = 1;
			my @ps = split(/\./o, $tree);
			$base = ($tree = join(".", ++$ps[0], "0"));
			next;
		}
		$tmp = ((length() <= 2) && /$patterns[2]/o) ? 1 : 0;
		next if(!defined($fh) && $tmp);

		if($origin && !index($_, "--$origin")) {
			&file_close($fh) if(defined($fh));
			return();
		}
		if($boundary) {
			if(index($_, "--$boundary--") >= 0) {
				defined($fh) and &file_close($fh);
				if($mbox) {
					$exclude = 1;
					$boundary = "";
					next;
				} else { return(); }
			}
			if(index($_, "--$boundary") >= 0) {
				defined($fh) and &file_close($fh);
				$header = 1;
				$boundary = "";
				my @ps = split(/\./o, $tree);
				$ps[$#ps]++;
				$tree = join("\.", @ps);
				next;
			}
		}
		$exclude and next;

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
			&set_filename(\%files, $_[0]->{$tree});
			$_[0]->{$tree}->{'content-disposition'}->{filepath} = ($args->{output_dir}) ?
				join("/", $args->{output_dir}, $_[0]->{$tree}->{'content-disposition'}->{filename}) :
					$_[0]->{$tree}->{'content-disposition'}->{filename};
			defined($fh) and &file_close($fh);
			$fh = &file_open($_[0]->{$tree}->{'content-disposition'}->{filepath});
		}
		if(defined($fh)) {
			if(!$ftmp && (length() <= 2) && /$patterns[6]/o) {
				$ftmp .= $_;
				next;
			}
			if($ftmp) {
				$_ = join("", $ftmp, $_);
				$ftmp = "";
			}
			print $fh $_;
			next unless(exists($_[0]->{$tree}->{'content-length'}));
			if(($ctlength += length()) >= $_[0]->{$tree}->{'content-length'}) {
				defined($fh) and &file_close($fh);
				$exclude = 1;
				next;
			}
		}
	}
	defined($fh) and &file_close($fh);
	return();
}

sub file_close {
	close($_[0]);
	undef($_[0]);
}

sub file_open {
	my $path = shift;
	local *FILE;
	open(FILE, ">$path") or
		die("MIME::Explode: Couldn't open file.tmp for writing: $!\n");
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
    check_content_type => 1,
    exclude_types      => ["image/gif", "image/jpeg", "image/png"],
  );

  open(MAIL, "<file.mbox") or
	die("Couldn't open file.mbox for reading: $!\n");
  open(OUTPUT, ">file.tmp")
	or die("Couldn't open file.tmp for writing: $!\n");
  my $headers = $explode->parse(\*MAIL, \*OUTPUT);
  close(OUTPUT);
  close(MAIL);

  for my $msg (sort{ $a cmp $b } keys(%{$headers})) {
    for my $k (keys(%{$headers->{$msg}})) {
      if(ref($headers->{$msg}->{$k}) eq "ARRAY") {
        for my $i (0 .. $#{$headers->{$msg}->{$k}}) {
          print "$msg => $k => $i => ", $headers->{$msg}->{$k}->[$i], "\n";
        }
      } elsif(ref($headers->{$msg}->{$k}) eq "HASH") {
        for my $ks (keys(%{$headers->{$msg}->{$k}})) {
          print "$msg => $k => $ks => ", $headers->{$msg}->{$k}->{$ks}, "\n";
        }
      } else {
        print "$msg => $k => ", $headers->{$msg}->{$k}, "\n";
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

=over 4

=item output_dir

directory where the decoded files are placed

=item mkdir => octal_number

if the value is set to octal number then make the output_dir directory
(example: mkdir => 0755).

=item check_content_type => 0 or 1

if the value is set to 1 the content-type of file is checked

=item exclude_types => [ARRAYREF]

not save files with specified content types

=back

=head2 parse(FILEHANDLE, FILEHANDLE)

This method parse the stream and splits it into its component entities. 
This method return a hash reference with all parts. The FILEHANDLE should 
be a reference to a GLOB. The second argument is optional.

=head1 AUTHOR

Henrique Dias <hdias@esb.ucp.pt>

=head1 SEE ALSO

MIME::Tools, perl(1).

=cut

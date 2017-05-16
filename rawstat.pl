#!/usr/bin/perl
#
# Input: Canon CR2 raw files or already converted PGM files
#

use strict;

our $PROGRAM = 'rawstat';
our $VERSION = '0.2';

our $DCRAW   = "dcraw -c -D -r 1 1 1 1 -4 -t 0";
our $MAGICK_CONVERT = "magick convert";

our $MATCH1  = "-(\\d+_\\d+)_sec";
our $MATCH2  = "_Tv(\\d+-\\d+)_";


use Getopt::Std;
use FileHandle;
use DirHandle;
use Data::Dumper;
use List::Util;
use File::Basename;
use POSIX qw(setlocale LC_NUMERIC);
use locale;

#setlocale LC_NUMERIC, "";


##### main ###################################################################
our ($opt_v, $opt_q, $opt_d, $opt_h, $opt_c, $opt_l, $opt_R, $opt_G, $opt_B,
     $opt_C, $opt_k, $opt_2, $opt_3, $opt_M, $opt_D, $opt_H, $opt_X, $opt_W);
getopts('vqdhclRGBCk23MDHXW');

if($opt_h or $#ARGV < 0) {
    print STDERR
      "\n",
      "$PROGRAM --- Raw image statistics\n",
      "\n",
      "Usage:   $PROGRAM [-vqdhclC23M] PGM|CR2|FITS|TIFF-FILE1 FILE2 ...\n",
      "\n",
      "Options:  -v        verbose\n",
      "          -q        quiet, ie no messages\n",
      "          -d        debug\n",
      "          -h        this help\n",
      "          -c        output CSV\n",
      "          -l        output short list\n",
      "          -R        stat only RED pixel, assuming R G\n",
      "          -G        stat only GREEN pixel         G B\n",
      "          -B        stat only BLUE pixel\n",
      "          -C        crop center 100x100 pixel\n",
      "          -2        set bias for 5D2\n",
      "          -3        set bias for 5D3\n",
      "          -M        match exp time in file name\n",
      "          -D        diff variance mode for matching exp time\n",
      "          -H        output histogram values as CSV\n",
      "          -X        exclude hot pixels (>2500 ADU) from histogram\n",
      "          -W        write R/G/B data to FILE.[RGB].pgm\n",
      "\n";
    exit 1;
}


our $BIAS = 0;
$BIAS = 1024 if($opt_2);	# 5D Mark II
$BIAS = 2048 if($opt_3);	# 5D Mark III
our $MAX  = 16383; 		# for 14 bit AD

$SIG{PIPE} = 'IGNORE';

my $rawstat = new RawStat;
my $args_sep = {};

die "$PROGRAM: you really don't want to use -D without -C ! ;-)\n"
    if($opt_D and !$opt_C);

die "$PROGRAM: you must use at least one of -R/-G/-B with -W NAME\n"
    if($opt_W and !$opt_R and !$opt_G and !$opt_B);

die "$PROGRAM: no use for -D with -W NAME ! ;-)\n"
    if($opt_W and $opt_D);

if($opt_D) {
    ##### diff mode #####
    for my $arg (@ARGV) {
	my @list = glob qq("$arg");
	if(@list) {
	    for my $file (@list) {
		my $key;
		$key = $1 if($file =~ /$MATCH1/);
		$key = $1 if($file =~ /$MATCH2/);
		$key = "none" unless($opt_M);
		if($key) {
#		print "file=", basename($file), " match=$key\n";
		    $args_sep->{$1} = [] unless defined($args_sep->{$1});
		    push @{$args_sep->{$key}}, $file;
		}
		else {
		    die "$PROGRAM: can't match file ", basename($file), "\n";
		}
	    }
	}
	else {
	    die "$PROGRAM: $arg doesn't match files/dirs\n";
	}
    }

    for my $key (sort keys %$args_sep) {
#    print "processing diff var for key=$key\n";
	do_args($args_sep->{$key});
    }
}
else {
    ##### normal mode #####
    for my $arg (@ARGV) {
	my @list = glob qq("$arg");
	if(@list) {
	    for my $file (@list) {
		do_file($file), next if(-f $file);
		do_dir($file) , next if(-d $file);
		die "$PROGRAM: $file is not a file/dir\n";
	    }
	}
	else {
	    die "$PROGRAM: $arg doesn't match files/dirs\n";
	}
    }
}


# final statistics
die "$PROGRAM: no data collected\n" unless($rawstat->stat_valid);

my ($min, $max, $mmean, $mvar, $vvar, $n);
($min)                       = $rawstat->stat_all("min");
(undef,$max)                 = $rawstat->stat_all("max");
(undef,undef,$mmean)         = $rawstat->stat_all("mean");
(undef,undef,$mvar,$vvar,$n) = $rawstat->stat_all("var");

my ($a, $b, $b_mod) = $rawstat->regression();
my $cvar = 1.96 * sqrt(2.0 / $n) * $vvar;

if($n > 1) {
    print "-" x 79 . "\n" if($opt_v);
    printf
	"$PROGRAM: overall min=%.2f max=%.2f mean=%.2f mean var=%.2f conf95=[%.2f ... %.2f]\n",
	$min, $max, $mmean, $mvar, $mvar-$cvar, $mvar+$cvar;

#    print "$PROGRAM: overall min=$min max=$max mean=$mmean mean var=$mvar conf95 var=+/-$cvar\n"
#	. "$PROGRAM: regression a=$a (bias) b=$b (e-/ADU)\n";
#    print "$PROGRAM: with fixed bias=$BIAS b=$b_mod (e-/ADU)\n" if($b_mod);
}

exit 0;



sub do_args {
    my ($args) = @_;

    for my $i (0 .. $#$args-1) {
	for my $j ($i+1 .. $#$args) {
	    my $file1 = $args->[$i];
	    my $file2 = $args->[$j];
	    die "$PROGRAM: $file1 is not a file\n" unless(-f $file1);
	    die "$PROGRAM: $file2 is not a file\n" unless(-f $file2);
	    do_file12($file1, $file2) if($file1 ne $file2);
	}
    }
}



sub do_dir {
    my ($dir) = @_;

    my $dh = DirHandle->new($dir)
	|| die "$PROGRAM: can't open directory $dir: $!";

    print "$PROGRAM: processing dir $dir\n" if($opt_d);

    my @files = sort grep !/^\./, $dh->read;
    my $f;

    for $f (@files) {
	do_file("$dir/$f");
    }
}



sub do_file {
    my ($file) = @_;

    if($file =~ /\.(cr2|pgm|fits?|tiff?)$/i) {
	print "$PROGRAM: processing file $file\n" if($opt_d);
	$rawstat->process_file($file);
    }
    else {
	print "$PROGRAM: skipping file $file\n" if($opt_d);
    }
}


sub do_file12 {
    my ($file1, $file2) = @_;

#    print "file1=", basename($file1), " file2=", basename($file2), "\n";
    $rawstat->process_file12_diff($file1, $file2);
}



##### classes ################################################################

package RawData;
use File::Basename;

sub new {
    my $class = shift;

    my $self = {};
    bless($self, $class);

    # init
    $self->{data}        = {};
    $self->{data}->{R}   = [] if($opt_R);
    $self->{data}->{G}   = [] if($opt_G);
    $self->{data}->{B}   = [] if($opt_B);
    $self->{data}->{ALL} = [] unless($opt_R or $opt_G or $opt_B);
    $self->{temp}        = undef;
    $self->{iso}         = undef;
    $self->{width}       = undef;
    $self->{height}      = undef;
    $self->{file}        = undef;
    
    my $file = shift;
    $self->process_file($file) if($file);

    return $self;
}

sub get_data {
    my $self = shift;

    return $self->{data};
}

sub process_file {
    my $self = shift;

    my ($file) = @_;
    my $basename = basename($file);

    print "RawData: $basename\n" if($opt_v);

    my $sensortemp = 0;
    if($file =~ /_([+\-]\d+)c_/) {
	$sensortemp = $1 + 0;
	print "         sensor temp=$sensortemp\n" if($opt_v);
	$self->{temp} = $sensortemp;
    }

    my $fh;
    if($file =~ /\.pgm$/i) {
	print "RawData: file type PGM\n" if($opt_d);
	$fh = FileHandle->new($file, "r")
	    || die "RawData: can't open $file: $!";
    }
    elsif($file =~ /\.cr2$/i) {
	print "RawData: file type CR2\n" if($opt_d);
	$fh = FileHandle->new("$DCRAW \"$file\"|")
	    || die "RawData: can't open pipe to dcraw for $file: $!";
    }
    elsif($file =~ /\.fits?$/i) {
	print "RawData: file type FITS\n" if($opt_d);
	$fh = FileHandle->new("$MAGICK_CONVERT \"$file\" -depth 16 pgm:-|")
	    || die "RawData: can't open pipe to dcraw for $file: $!";
    }
    elsif($file =~ /\.tiff?$/i) {
	print "RawData: file type TIFF\n" if($opt_d);
	$fh = FileHandle->new("$MAGICK_CONVERT \"$file\" pgm:-|")
	    || die "RawData: can't open pipe to dcraw for $file: $!";
    }
    else {
	die "RawData: file $file not supported." if($opt_v);
    }
    $fh->binmode;

    <$fh> =~ /^P5$/ or die "RawData: $file isn't a PGM RAWBITS file\n"; 
    my $in;
    while($in = <$fh>) {
	last unless($in =~ /^#/);
    }
    my ($width, $height) = split (' ', $in); 
    while($in = <$fh>) {
	last unless($in =~ /^#/);
    }
    my ($max) = split (' ', $in); 

    print "RawData: image width=$width, height=$height, max=$max\n" if($opt_d);
    $self->{width} = $width;
    $self->{height} = $height;
    $self->{file} = $file;
    
    my ($wcenter, $hcenter) = (int($width/4)*2, int($height/4)*2);
    print "RawData: center w=$wcenter, h=$hcenter\n" if($opt_d and $opt_C);

    my ($n, $buffer);
    my $B16 = $max > 255 ? 1 : 0;
    my $size = $width * ($B16 + 1);
    my $val;
    my ($x, $y);
    my @line;
    my ($ymin, $ymax) = $opt_C ? ($hcenter-50, $hcenter+49) : (-1, 999999999);

    for($y=0; $y<$height; $y++) {
	$n = $fh->read($buffer, $size)
	    || die "RawData: can't read $size bytes: $!";
	$n == $size
	    || die "RawData: read short $n/$size bytes";

	next if($y < $ymin || $y > $ymax);

	@line = unpack("n$width", $buffer);
	@line = @line[$wcenter-50 .. $wcenter+49] if($opt_C);

 	if($opt_R) {
	    if(0 == $y % 2) {
		my $idx = 0;
		push @{$self->{data}->{R}}, grep { 0 == $idx++ % 2 } @line; 
	    }
	}
 	if($opt_G) {
	    my $idx = 0;
	    my $mi  = $y % 2 ? 0 : 1;
	    push @{$self->{data}->{G}}, grep { $mi == $idx++ % 2 } @line; 
	}
 	if($opt_B) {
	    if(1 == $y % 2) {
		my $idx = 0;
		push @{$self->{data}->{B}}, grep { 1 == $idx++ % 2 } @line; 
	    }
	}
	push @{$self->{data}->{ALL}}, @line unless($opt_R or $opt_G or $opt_B);
    }

    $fh->close();

    return 1;
}

sub write_pgm {
    my $self = shift;

    my ($key) = @_;

    return unless defined($self->{data}->{$key});
    
    my $name = basename($self->{file}) . ".$key.pgm";
    print "RawData: writing raw data $name\n" if($opt_v);

    my $fh = FileHandle->new($name, "w")
	|| die "RawData: can't write to $name: $!";
    $fh->binmode;

    my $width = $self->{width} / 2;
    my $height = int( $key eq "G" ? $self->{height} : $self->{height} / 2 );

    print $fh "P5\n# raw data channel $key\n$width $height\n65535\n";
    
    my @data = @{$self->{data}->{$key}};
    my $size = @data;
    print "RawData: $size 16bit values\n" if($opt_d);

    print $fh pack("n$size", @data);
    
    $fh->close();
}




package RawStat;
use File::Basename;

sub new {
    my $class = shift;

    my $self = { min  => [],
		 max  => [],
		 mean => [],
		 var  => [],
		 saved_rawdata => {},
    };
    bless($self, $class);

    return $self;
}

sub process_file12_diff {
    my $self = shift;
    my $file1 = shift;
    my $file2 = shift;

    # use saved data if it already exists
    my $rawdata1 = $self->{saved_rawdata}->{$file1};
    unless(defined $rawdata1) {
	$rawdata1 = new RawData($file1);
	$self->{saved_rawdata}->{$file1} = $rawdata1;
    }
    my $data1 = $rawdata1->get_data();

    my $rawdata2 = $self->{saved_rawdata}->{$file2};
    unless(defined $rawdata2) {
	$rawdata2 = new RawData($file2);
	$self->{saved_rawdata}->{$file2} = $rawdata2;
    }
    my $data2 = $rawdata2->get_data();

    for my $k ("ALL", "R", "G", "B") {
	next unless(defined $data1->{$k} and defined $data2->{$k});
	my ($min1, $max1, $mean1) = stat_data($data1->{$k});
	my ($min2, $max2, $mean2) = stat_data($data2->{$k});
	my (undef, undef, $mdiff, $var) = 
	    stat_diff_data($data1->{$k}, $data2->{$k});
	$var /= 2;

	my ($min, $max, $mean) =
	    (($min1 < $min2 ? $min1 : $min2),
	     ($max1 > $max2 ? $max1 : $max2),
	     ($mean1 + $mean2) / 2
	    );

#	print "diff mean=$mdiff\n";

	printf
	    "RawStat: diff2 ($k) min=%.0f max=%.0f mean=%.2f var=%.2f\n",
	    $min, $max, $mean, $var
	    if($opt_v);
	print basename($file1), "/", basename($file2)
	    , ";$k;$min;$max;$mean;$var\n"
	    if($opt_c);
	push @{$self->{min}} , $min;
	push @{$self->{max}} , $max;
	push @{$self->{mean}}, $mean;
	push @{$self->{var}} , $var;
    }
}

sub stat_diff_data {
    my ($a, $b) = @_;

    my $diff = [];
    @$diff = map { $a->[$_] - $b->[$_] } 0..$#$a;

    return stat_data($diff);
}

sub process_file {
    my $self = shift;
    my $file = shift;

    print "RawStat: $file\n" if($opt_d);

    my $rawdata = new RawData($file);
    my $data    = $rawdata->get_data();

    for my $k ("ALL", "R", "G", "B") {
	next unless(defined $data->{$k});
	my ($min, $max, $mean, $var) = stat_data($data->{$k});
	print "RawStat: ($k) min=$min max=$max mean=$mean var=$var\n"
	    if($opt_v);
	print basename($file), ";$k;$min;$max;$mean;$var\n"
	    if($opt_c);
	push @{$self->{min}} , $min;
	push @{$self->{max}} , $max;
	push @{$self->{mean}}, $mean;
	push @{$self->{var}} , $var;

	histo_data($data->{$k}) if($opt_H);
    }

    # dump raw data to single channel pgm file for further analysis
    if($opt_W) {
	for my $k ("R", "G", "B") {
	    $rawdata->write_pgm($k);
	}
    }
}

sub stat_valid {
    my $self = shift;

    return $#{$self->{min}} >= 0;
}

sub stat_all {
    my $self = shift;
    my $key  = shift;

    return defined($self->{$key}) ? stat_data($self->{$key}) : undef;
}

sub stat_data {
    my ($data) = @_;

    my $n    = $#$data + 1;
    my $sum  = 0;
    my $sum2 = 0;
    my $min  = undef;
    my $max  = undef;
    my $x;

    for $x (@$data) {
	$sum  += $x;
	$sum2 += $x * $x;
	$min   = $x if(!defined($min) || $x < $min);
	$max   = $x if(!defined($max) || $x > $max);
    }

    my $mean   = $sum / $n;
    my $var    = $n > 1 ? ($sum2 - $sum*$sum/$n) / ($n - 1) : 0;

    return ($min, $max, $mean, $var, $n);
}    

sub regression {
    my $self = shift;

    my $x = $self->{var};
    my $y = $self->{mean};
    my $n = @$x;
    my ($prod, $sumx, $sumy, $sumx2);

    return (0, 0, 0) unless($n > 1);
    
    for my $i (0..$n-1) {
	$prod  += $x->[$i] * $y->[$i];
	$sumx  += $x->[$i];
	$sumy  += $y->[$i];
	$sumx2 += $x->[$i]**2;
    }
    
    my $b = ($n*$prod - $sumx*$sumy) / ($n*$sumx2 - $sumx**2);
    my $a = ($sumy - $b*$sumx) / $n;

    # modified regression with fixed a=BIAS
    my $b_mod;
    if($BIAS > 0) {
	$b_mod = ($prod - $BIAS*$sumx) / $sumx2;
    }

    return ($a, $b, $b_mod);
}

sub histo_data {
    my ($data) = @_;

    my $x;
    my $histo = {};

    for $x (@$data) {
	next if($opt_X and $x > 2500);
	$histo->{$x}++;
    }
    print "ADU,Occurrence\n";
    for $x (sort { $RawStat::a <=> $RawStat::b } keys %$histo) {
	print $x, ",", $histo->{$x}, "\n";
    }
}



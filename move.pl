#!/usr/bin/perl -w

use Getopt::Std;
use vars qw ($opt_b $opt_h $opt_l);

getopts('bhl');

my $base = shift || '*';

if ($opt_b)
{
    @bins = ('as68/as68','c68/c68','cc/qcc','cpp/qcpp','ld/qld');
    foreach $bin (@bins)
    {
	`cp -b -v $bin /usr/local/bin/`;
    }
}

if ($opt_h or $opt_l)
{
    @misc = <${base}>;
    foreach $misc (@misc)
    {
	if ($opt_h)
	{
	    $hnam = $misc;

	    $hnam =~ s/include_/include\//i;
	    $hnam =~ s/sys_/sys\//i;
	    system "cp -v $misc /usr/local/qdos/$hnam";
	}

	if ($opt_l)
	{
	    if($misc =~ /lib_(.*)/i)
	    {
		$lnam = $1;
		system "cp -v $misc /usr/local/qdos/lib/$lnam";
	    }
	}
    }
}



# Perl
#
# Select Validation modes
#
# perl run_modes.pl (die|warn|ignore)
#
# Wed Jun 25 13:32:12 2014

$| = 1;

use strict;
use warnings;

use Scalar::Validation qw (:all);
use Vt;

sub position {
    Vt::position(@_);
}

my $validation_mode = shift || 'die';

{
    print "# Switch to validation mode $validation_mode\n";

    local ($Scalar::Validation::fail_action, $Scalar::Validation::off)
	= prepare_validation_mode($validation_mode => 1);
    
    my $var = '0';
    
    print "'".ref('0')."'\n";
    
    position (5, 3);
    position (4.001, 2.001);
    
    
    position ('a4.1', 2);
    position ('a5', 2);
}

print "\n# now leaving the block and returning to initial validation mode: 'die'.\n";

position ('a4.1', 2); # dies

print "#### did not die!!!\n";

# Perl
#
# Example of Scalar::Validation, named access by npar
#
# Wed Jun 25 13:32:12 2014

use strict;
use warnings;

use Scalar::Validation qw (:all);

sub named {
    # --- define and scan parameters ------------------------------
    my %parameters = convert_to_named_params \@_;

    my $p_int   = npar -p_int   => Int   => \%parameters;
    my $p_float = npar -p_float => Float => \%parameters;

    parameters_end \%parameters;

    # --- run sub -------------------------------------------------
    print  "named (-p_int => $p_int , -p_float => $p_float)\n";
}

print "\n# --- calls with valid args ---\n\n";

named (-p_int => 1, -p_float => 1);
named (-p_int => 3, -p_float => 3.14159);

print "\n# --- calls with invalid args enclosed by eval { } ---\n\n";

eval { named         }; print $@;
eval { named ()      }; print $@;
eval { named -p_int => 1                    }; print $@;
eval { named -p_int => 1.2, -p_float => 5   }; print $@;
eval { named -p_int => 1,   -p_float => 'a' }; print $@;

eval { named -p_int => 1,   -p_float => 1, -p_3 => 2 }; print $@;

print "\n# --- Ready. ---\n";

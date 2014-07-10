# Perl
#
# Example of Scalar::Validation, indexed access by par ... shift
#
# Sat Jul  5 17:29:25 2014

$| = 1;

use strict;
use warnings;

use Scalar::Validation qw (:all);

sub indexed {
    # --- define and scan parameters ------------------------------
    my $p_int   = par p_int   => Int   => shift;
    my $p_float = par p_float => Float => shift;

    parameters_end \@_;

    # --- run sub -------------------------------------------------
    print  "indexed (p_int = $p_int , p_float = $p_float)\n";
}

print "\n# --- calls with valid args ---\n\n";

indexed (1,1);
indexed (3, 3.14159);

print "\n# --- calls with invalid args enclosed by eval { } ---\n\n";

eval { indexed         }; print $@;
eval { indexed ()      }; print $@;
eval { indexed 1       }; print $@;
eval { indexed 1.2, 5  }; print $@;
eval { indexed 1,  'a' }; print $@;

eval { indexed 1, 2, 'par_3' }; print $@;

print "\n# --- Ready. ---\n";

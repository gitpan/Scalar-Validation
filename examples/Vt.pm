
$VERSION = "0.100";

package Vt;

use Scalar::Validation qw (:all);

sub position {
	my $x = validate (x => greater_than 4 => Float => shift);
	my $y = validate (y => greater_than 2 => Float => shift);

	print "int position ($x, $y)\n";
}

1;


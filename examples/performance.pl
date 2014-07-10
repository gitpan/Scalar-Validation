# Perl
#
# Performance Tests of Validation
#
# Sun Jul  6 10:19:58 2014

$| = 1;

use strict;
use warnings;

use Scalar::Validation qw (:all);

use Time::HiRes qw(time);

declare_rule (
    Any => -where   => sub { 1; },
    -message => sub { "No problem!!"}
    );

# my $max_loops = 1000000;
my $max_loops = 100000;
# my $max_loops = 10000;
# my $max_loops = 1000;
my $run = 0;

# --- content, called by both loops ---

# my $sub_content;
my $sub_content = sub { my $v = shift; return ++$v; };
# my $sub_content = sub { my @arr = map { $_ } (0..shift); };

# --- first get time needed without validation ---

my $max_empty_loops_per_second = 0;
while ($run < 3) {
    my $loops = 1000;

    my $rules_ref = [Any => Any => 0];
    
    while ($loops <= $max_loops ) {
	my $start_time = time;
	
	foreach my $i (1..$loops) {
	    my $variable = -1 + $i;
	    $sub_content->($variable) if $sub_content;
	}
	my $duration = time - $start_time;
	my $loops_per_second = $loops / $duration;
	$max_empty_loops_per_second = $loops_per_second if $max_empty_loops_per_second < $loops_per_second;
	
	print "## empty $loops, time $duration s, validations/second: $loops_per_second\n";
	
	$loops *= 10;
    }
    $run++;
}

# --- now get time needed with validation ---
# ($Scalar::Validation::fail_action, $Scalar::Validation::off)   = prepare_validation_mode(off => 1);

my $max_loops_per_second = 0;

$run = 0;
while ($run < 3) {
    my $loops = 1000;

    my $rules_ref = [Any => Any => 0];
    
    while ($loops <= $max_loops ) {
	my $start_time = time;
	
	foreach my $i (1..$loops) {
	    my $variable = validate(Performance => Any => $i);
	    # my $variable = is_valid(Performance => Any => $i);
	    # my $variable = validate(Performance => [Any => 'Any'] => $i);
	    # my $variable = validate(Performance => -And => [Any => Any => 0] => $i);
	    # my $variable = validate(Performance => $rules_ref => $i);
	    # my $variable = validate(Performance => Int => $i);
	    # my $variable = validate(Performance => -Optional => Int => undef);
	    # my $variable = validate(Performance => -Optional => Int => '');  # => dies
	    # my $variable = validate(Performance => -Optional => Int => $i);
	    # my $variable = validate(Performance => -Optional => Float => $i);
	    # };

	    $sub_content->($variable-1) if $sub_content;
	}
	
	my $duration = time - $start_time;
	my $loops_per_second = $loops / $duration;
	$max_loops_per_second = $loops_per_second if $max_loops_per_second < $loops_per_second;
	
	print "## validations $loops, time $duration s, validations/second: $loops_per_second\n";
	
	$loops *= 10;
    }
    $run++;
}

my $factor = $max_loops_per_second / $max_empty_loops_per_second * 100.0;
print "\n## max loops per second = $max_loops_per_second\n";
print "## factor               = $factor %\n";

#------------------------------------------------------------------------------------
## max 6092033.29024387   => my $variable = -1 + $loops;
## max 2131252.03252033   => off => my $variable = validate(Performance => Any => -1);
## max 2058049.06771344   => off => my $variable = validate(Performance => $rules_ref => -1); (outside: $rules_ref = [Any => Any => 0];)
## max  450407.635542039  => my $variable = validate(Performance => Any => -1);
## max  330825.687060321  => validate(Performance => [Optional => Int => 0] => undef);
## max  289062.991040662  => validate(Performance => Int => -1); no check for filled before
## max  169371.022451946  => validate(Performance => Int => -1); two step check: Filled => Int
## max  131836.030998395  => validate(Performance => [Any => 'Any']           => -1);
## max  124714.669711458  => validate(Performance => [Any => Any => 0]        => -1);
## max   80054.6790706161 => validate(Performance => [Optional => Int => 0]   => -1);
## max   40836.6525718703 => validate(Performance => [Optional => Float => 0] => -1.1);


#------------------------------------------------------------------------------------
# validate(Any => 10); ==> maximum speed, -where => sub { 1; }

## validations 100, time 0.000226020812988281 s, validations/second: 442437.130801688
## validations 1000, time 0.00213789939880371 s, validations/second: 467748.856919817
## validations 100, time 0.000211000442504883 s, validations/second: 473932.655367232
## validations 100000, time 0.209654808044434 s, validations/second: 476974.513166454
## validations 10000, time 0.0208311080932617 s, validations/second: 480051.275008012
## validations 1000000, time 2.0767650604248  s, validations/second: 481518.116351326
## validations 1000, time 0.00206518173217773 s, validations/second: 484218.88709305
## validations 100, time 0.00020599365234375  s, validations/second: 485451.851851852
## validations 1000, time 0.00205683708190918 s, validations/second: 486183.377767474
## validations 100000, time 0.204194068908691 s, validations/second: 489730.189199161
## validations 1000000, time 2.0394229888916  s, validations/second: 490334.768925737
## validations 1000000, time 2.03901100158691 s, validations/second: 490433.842299882
## validations 10000, time 0.0203900337219238 s, validations/second: 490435.677369566
## validations 10000, time 0.0203869342803955 s, validations/second: 490510.238688325
## validations 100000, time 0.203597068786621 s, validations/second: 491166.206841634

#------------------------------------------------------------------------------------
# validate(Positive => 10); ==> -where   => sub { $_ >= 0 }

## validations 100, time 0.000252008438110352 s, validations/second: 396812.10974456
## validations 100, time 0.000231027603149414 s, validations/second: 432848.71001032
## validations 100, time 0.000230073928833008 s, validations/second: 434642.901554404
## validations 1000, time 0.00223183631896973 s, validations/second: 448061.531887619
## validations 1000, time 0.00219416618347168 s, validations/second: 455753.993263066
## validations 1000, time 0.00218701362609863 s, validations/second: 457244.521966641
## validations 10000, time 0.0218451023101807 s, validations/second: 457768.512960437
## validations 10000, time 0.0216689109802246 s, validations/second: 461490.658627291
## validations 100000, time 0.216356039047241 s, validations/second: 462201.103516066
## validations 100000, time 0.215886831283569 s, validations/second: 463205.649948371
## validations 100000, time 0.214712142944336 s, validations/second: 465739.844187224
## validations 10000, time 0.0213799476623535 s, validations/second: 467727.992506189
## validations 1000000, time 2.10470604896545 s, validations/second: 475125.730973947
## validations 1000000, time 2.1029531955719  s, validations/second: 475521.757738431
## validations 1000000, time 2.08221507072449 s, validations/second: 480257.786075892

#------------------------------------------------------------------------------------
# validate(Int => 10); -where   => sub { /^[\+\-]?\d+$/ },

## validations 100, time 0.000334024429321289 s, validations/second: 299379.300499643
## validations 100, time 0.000307083129882812 s, validations/second: 325644.720496894
## validations 1000, time 0.00298118591308594 s, validations/second: 335436.980166347
## validations 10000, time 0.0296890735626221 s, validations/second: 336824.252158201
## validations 10000, time 0.0296549797058105 s, validations/second: 337211.493624479
## validations 100, time 0.000293970108032227 s, validations/second: 340170.640713706
## validations 1000000, time 2.93383383750916 s, validations/second: 340850.932733466
## validations 100000, time 0.293323993682861 s, validations/second: 340919.945703858
## validations 1000, time 0.00293207168579102 s, validations/second: 341055.781427874
## validations 100000, time 0.291172027587891 s, validations/second: 343439.58390651
## validations 1000000, time 2.89830589294434 s, validations/second: 345029.143553967
## validations 1000000, time 2.88187408447266 s, validations/second: 346996.423399597
## validations 1000, time 0.00286316871643066 s, validations/second: 349263.385793988
## validations 100000, time 0.285795211791992 s, validations/second: 349900.893625825
## validations 10000, time 0.028548002243042  s, validations/second: 350287.208010757

#------------------------------------------------------------------------------------
# validate(PositiveInt => 10); -where   => sub { /^[\+\-]?\d+$/ }
#                              -where   => sub { $_ >= 0 },

## validations 100, time 0.00055694580078125  s, validations/second: 179550.684931507
## validations 100, time 0.000543832778930664 s, validations/second: 183880.052608505
## validations 100, time 0.0005340576171875   s, validations/second: 187245.714285714
## validations 10000, time 0.051192045211792  s, validations/second: 195342.849824186
## validations 1000, time 0.00507187843322754 s, validations/second: 197165.608987919
## validations 1000, time 0.00504803657531738 s, validations/second: 198096.821423511
## validations 1000, time 0.00504493713378906 s, validations/second: 198218.525519849
## validations 100000, time 0.503677129745483 s, validations/second: 198539.886157888
## validations 1000000, time 4.94846796989441 s, validations/second: 202082.746838783
## validations 10000, time 0.0493988990783691 s, validations/second: 202433.661206406
## validations 10000, time 0.0486710071563721 s, validations/second: 205461.127358052
## validations 100000, time 0.479023933410645 s, validations/second: 208757.836561529
## validations 1000000, time 4.7882399559021  s, validations/second: 208845.005515518
## validations 100000, time 0.478343963623047 s, validations/second: 209054.587503489
## validations 1000000, time 4.78092312812805 s, validations/second: 209164.626412127

#------------------------------------------------------------------------------------
# validate(PositiveInt => -1); # died because not matching rule, \
#                              # ==> max speed: -where   => sub { 0; }

## validations  10, time 0.000869989395141602 s, validations/second:  11494.3929843793
## validations  1000, time 0.0734570026397705 s, validations/second:  13613.4059934891
## validations  100, time 0.00732994079589844 s, validations/second:  13642.6749934947
## validations  10, time 0.000731945037841797 s, validations/second:  13662.2280130293
## validations  10, time 0.000728845596313477 s, validations/second:  13720.3271180896
## validations  10000, time 0.726739883422852 s, validations/second:  13760.0814653261
## validations  100000, time 7.15035200119019 s, validations/second:  13985.3254753549
## validations  100, time 0.00714397430419922 s, validations/second:  13997.8107061807
## validations  100000, time 7.14260506629944 s, validations/second:  14000.4940874898
## validations  10000, time 0.714101076126099 s, validations/second:  14003.6198436342
## validations  100000, time 7.13942289352417 s, validations/second:  14006.7343665418
## validations  1000, time 0.0713930130004883 s, validations/second:  14006.9729231509
## validations  10000, time 0.713597059249878 s, validations/second:  14013.5106645645
## validations  1000, time 0.0713260173797607 s, validations/second:  14020.1294946233
## validations  100, time 0.00713181495666504 s, validations/second:  14021.6761942968

#------------------------------------------------------------------------------------
# validate(Int => 'a'); # died because not matching rule

## validations  10, time 0.000885009765625    s, validations/second:  11299.3103448276
## validations  100, time 0.00750899314880371 s, validations/second:  13317.3646610573
## validations  10, time 0.000746965408325195 s, validations/second:  13387.5007979572
## validations  1000, time 0.0746810436248779 s, validations/second:  13390.2788641116
## validations  10000, time 0.73863697052002  s, validations/second:  13538.4504149037
## validations  100, time 0.00726795196533203 s, validations/second:  13759.0342474741
## validations  100, time 0.00726509094238281 s, validations/second:  13764.4526122342
## validations  10000, time 0.726456880569458 s, validations/second:  13765.4419243179
## validations  100000, time 7.26404190063477 s, validations/second:  13766.4404153921
## validations  1000, time 0.0726380348205566 s, validations/second:  13766.8922689109
## validations  100000, time 7.26015019416809 s, validations/second:  13773.8197317636
## validations  10, time 0.000725984573364258 s, validations/second:  13774.3973727422
## validations  1000, time 0.0725948810577393 s, validations/second:  13775.075947912
## validations  10000, time 0.7255539894104   s, validations/second:  13782.5718636406
## validations  100000, time 7.25426506996155 s, validations/second:  13784.9939360611

#------------------------------------------------------------------------------------
# validate(PositiveInt => -1); # died because not matching 2. rule

## validations  10, time 0.000936985015869141 s, validations/second:  10672.5292620865
## validations  100, time 0.00787496566772461 s, validations/second:  12698.46805934
## validations  1000, time 0.0785439014434814 s, validations/second:  12731.7332297222
## validations  10, time 0.000782966613769531 s, validations/second:  12771.9366626066
## validations  1000, time 0.0771088600158691 s, validations/second:  12968.6783048563
## validations  100, time 0.00770902633666992 s, validations/second:  12971.8067668708
## validations  100000, time 7.70031380653381 s, validations/second:  12986.4837346172
## validations  10000, time 0.769781112670898 s, validations/second:  12990.705845332
## validations  100, time 0.00769591331481934 s, validations/second:  12993.90935283
## validations  100000, time 7.69377017021179 s, validations/second:  12997.5288821563
## validations  10000, time 0.76933217048645  s, validations/second:  12998.2865446495
## validations  1000, time 0.0769309997558594 s, validations/second:  12998.6611791541
## validations  10000, time 0.769108057022095 s, validations/second:  13002.0741672099
## validations  10, time 0.000767946243286133 s, validations/second:  13021.7447997516
## validations  100000, time 7.67715811729431 s, validations/second:  13025.6533045386


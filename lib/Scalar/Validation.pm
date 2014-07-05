# perl
#
# Class Scalar::Validation
#
# Simple rule based validation package for scalar values
#
# Ralf Peine, Sat Jul  5 17:47:11 2014
#
# More documentation at the end of file
#------------------------------------------------------------------------------

$VERSION = "0.600";

package Scalar::Validation;

use base qw (Exporter);

use strict;
use warnings;

our @EXPORT = qw();
our @EXPORT_OK = qw (validate is_valid validate_and_correct npar named_parameter par parameter 
                     rule_known declare_rule enum Enum enum_explained Enum_explained
                     greater_than less_than equal_to
                     convert_to_named_params parameters_end p_end
                     validation_messages get_and_reset_validation_messages prepare_validation_mode);

our %EXPORT_TAGS = (
        all => [qw(validate is_valid validate_and_correct npar named_parameter par parameter
                   rule_known declare_rule enum Enum enum_explained Enum_explained
                   greater_than less_than equal_to
                   convert_to_named_params parameters_end p_end
                   validation_messages get_and_reset_validation_messages prepare_validation_mode)],
);               

use Carp;
use Data::Dumper;

# ------------------------------------------------------------------------------
#
# Initiliazation
#
# ------------------------------------------------------------------------------

_init_run_API();

# ------------------------------------------------------------------------------
#
# default actions, not changable
#
# ------------------------------------------------------------------------------

my $croak_sub = sub { croak "Error: ",@_; };
my $get_caller_info_default = sub {
    my ($module, $file_name, $line, $sub_name);
    $sub_name     = 'Scalar::Validation';
    my $call_level = 1;
    
    while ($sub_name =~ /^Scalar::Validation/) {
        ($module, $file_name, $line, $sub_name) = caller($call_level++);
        # $module   = '' unless $module;
        $sub_name = '' unless $sub_name;
        # print "$module\n";
        # print "$sub_name()\n\n";
    }
    
    $sub_name = "MAIN" unless $sub_name;
    return $sub_name;
};

# ------------------------------------------------------------------------------
#
# variables my be overwritten by user
#
# ------------------------------------------------------------------------------

our $message_store   = undef; # local ($Scalar::Validation::message_store) = []; # to start storing messages
our $off             = 0;     # no checks if $off == 1

# ------------------------------------------------------------------------------
#
# actions, changable
#
# ------------------------------------------------------------------------------

our $fail_action     = $croak_sub;
our $get_caller_info = $get_caller_info_default;

# ------------------------------------------------------------------------------
#
#  private vars of Validation "Instance"
#
# ------------------------------------------------------------------------------

my $special_rules;
my $rule_store;
my $get_content_subs;

# ------------------------------------------------------------------------------
#
# normal rules, can be added, replaced, removed
#
# ------------------------------------------------------------------------------

$rule_store = {
    Bool =>          { -where   => sub { ref ($_) ? 0: 1 },
                       -message => sub { "value $_ is not a bool value" },
    },
    Bool01 =>        { -where   => sub { defined $_ && ($_ eq '0' || $_ eq '1') },
                       -message => sub { "value $_ is not a bool value ( 0 or 1 )" },
    },
    Int =>           { -as      => 'Filled',
                       -where   => sub { /^[\+\-]?\d+$/ },
                       -message => sub { "value $_ is not an integer" },
    },
    PositiveInt =>   { -as      => 'Int',
                       -where   => sub { $_ >= 0 },
                       -message => sub { "value $_ is not a positive integer" },
    },
    NegativeInt =>   { -as      => 'Int',
                       -where   => sub { $_ < 0 },
                       -message => sub { "value $_ is not a negative integer" },
    },
    Float =>         { -as      => 'Filled',
                       -where   => sub { /^[\+\-]?\d+(\.\d+)?([Ee][\+-]?\d+)?$/ },
                       -message => sub { "value $_ is not a float" },          
    },
    Even =>          { -as      => 'Int',
                       -where   => sub { $_ % 2 ? 0: 1; },
                       -message => sub { "value $_ is not an integer or not even"},
    },
    PositiveFloat => { -as      => 'Float',
                       -where   => sub { $_ > 0 },
                       -message => sub { "value $_ is not a positive float" },         
    },
    CodeRef     =>   { -where   => sub { $_ and ref($_) eq 'CODE' },
                       -message => sub { "value $_ is not a code reference" },
    },
    HashRef     =>   { -where   => sub { $_ and ref($_) eq 'HASH' },
                       -message => sub { "value $_ is not a hash reference" },
    },
    ArrayRef    =>   { -where   => sub { $_ and ref($_) eq 'ARRAY' },
                       -message => sub { "value $_ is not a array reference" },
    },
    Scalar      =>   { -where   => sub { ref($_) eq '' },
                       -message => sub { "value $_ is not a scalar" },
    },
    String      =>   { -where   => sub { defined $_ and ref($_) eq '' },
                       -message => sub { "value $_ is not a string" },
    },
    Defined     =>   { -where   => sub { defined $_ },
                       -message => sub { "value is not defined" },
    },
    Filled      =>   { -where   => sub { defined $_ and $_ ne '' },
                       -message => sub { "value is not set" },
    },
    Empty       =>   { -where   => sub { !defined $_ or $_ eq '' },
                       -message => sub { "value $_ has to be empty" },
    },
    Optional    =>   { -where   => sub { 1; },
                       -message => sub { "value is optional" },
    },
};

# ------------------------------------------------------------------------------
#
# gets content of ref container
#
# ------------------------------------------------------------------------------

$get_content_subs = {
        HASH  => sub { my @keys = keys %$_; return scalar @keys ? \@keys: undef; },
        ARRAY => sub { return scalar @$_ ? $_: undef; },
};

# ------------------------------------------------------------------------------
#
# special rules (combined and other), not changable
#
# ------------------------------------------------------------------------------

$special_rules = {
    -Optional => {
        -value_position => '-9',
        -code           => sub {
            # my    $subject_info = shift || '';
            # my    $rule_info    = shift;
            local $_            = $_[2];

            # skip one param if special rule follows
            my $special_rule = $special_rules->{$_[1]};
            if ($special_rule) {
                $_ = $_[$special_rule->{-value_position}];
            }
            ;
        
            return $_ if !defined $_; # value not set
            return validate(@_);
        }
    },
    -And => {
        -value_position => 3,
        -code           => sub {
            my    $subject_info = shift || '';
            my    $rule_list    = shift;
            local $_            = shift;
            my    $message_ref  = shift;
            
            my $rule_exists  = 0;
            my $orig_value   = $_;
            
            foreach my $rule (@$rule_list) {
                if (!defined $rule || $rule eq '') {
                    $fail_action->("rule for validation not set");
                    next        # in case of fail action doesn't die
                }
                
                if ($rule) {
                    validate ($subject_info, $rule, $_, $message_ref);
                    $rule_exists = 1;
                }
            }
            
            $fail_action->("No rule found in list to be validated") unless $rule_exists;
            
            return $orig_value;
        }
    },
    -Or => {
        -value_position => 3,
        -code           => sub {
            my    $subject_info = shift || '';
            my    $rule_list    = shift;
            local $_            = shift;
            my    $message_ref  = shift;

            my $rule_exists  = 0;
            my $orig_value   = $_;
        
            foreach my $rule (@$rule_list) {
                if (!defined $rule || $rule eq '') {
                    $fail_action->("rule for validation not set");
                    next        # in case of fail action doesn't die
                }

                next unless $rule;

                my $rule_ref = $rule_store->{$rule};
                if ($rule_ref) {
                    my $test_message_ref = $message_ref || $rule_ref->{-message};

                    my $parent_is_valid = defined $rule_ref->{-as}
                        ? _check_parent_rules($rule_ref->{-as}, $_)
                            : 1;
                    return $orig_value if $parent_is_valid && $rule_ref->{-where}->();

                    $rule_exists = 1;
                }
            }
        
            $fail_action->("No rule found in list to be validated") unless $rule_exists;

            my $result = _do_fail($subject_info, $message_ref || sub { "No rule matched of [".join(', ', @$rule_list)."]";});
            return $result if defined $result;

            return $orig_value;
        }
    },
    -Enum => {
        -value_position => 3,
        -code           => sub {
            my    $subject_info = shift || '';
            my    $enum_ref     = shift;
            local $_            = shift;
            my    $message_ref  = shift;
        
            my $orig_value      = $_;

            my $arg_type = ref ($enum_ref);
            if ($arg_type eq 'ARRAY') {
                $enum_ref = { map {$_=> 1} @$enum_ref };
            }
            elsif ($arg_type ne 'HASH') {
                _do_fail($subject_info, sub {"-Enum needs HASH_ref as second parameter";});
            }

            unless (defined $_ && $enum_ref->{$_}) {
                my $result = _do_fail($subject_info, $message_ref ||
                             sub { "value $_ unknown, allowed values are: [ "
                                       .join (", ", sort (keys(%$enum_ref)))." ]"; }
                         );
                return $result if defined $result;

            }
            return $orig_value;
        }
    },
    -Range => {
        -value_position => 4,
        -code           => sub {
            my    $subject_info = shift || '';
            my    $range_ref    = shift;
            my    $rule         = shift;
            local $_            = shift;
            my    $message_ref  = shift;
        
            my $wrong_call_message_sub_ref
                = sub { "-Range needs ARRAY_ref containing two values [min max] as second parameter" };

            my $orig_value = $_;
        
            unless (ref($range_ref) eq 'ARRAY') {
                _do_fail($subject_info, $wrong_call_message_sub_ref);
                return $orig_value;
            }

            unless (scalar @$range_ref == 2) {
                _do_fail($subject_info, $wrong_call_message_sub_ref);
                return $orig_value;
            }

            my ($min, $max) = @$range_ref;
            if ($min > $max) {
                _do_fail($subject_info, sub { "(min) $min > $max (max) in range definition"; });
                return $orig_value;
            }

            # type check by is_valid to return here if fails
            my @messages;
            my $is_valid;
            {
                local ($message_store) = [];
                $is_valid = is_valid ($subject_info, $rule, $_, $message_ref);
                @messages = @{validation_messages()};
            }
            
            unless ($is_valid) {
                my $message = join ("\n", @messages);
                push (@$message_store, $message) if $message_store;
                my $result = $fail_action->($message);
                return $result if defined $result;

                return $orig_value;
            }
        
            unless ($min <= $_  &&  $_<= $max) {
                my $result = _do_fail($subject_info, sub {"value $_ is out of range [$min,$max]"});
                return $result if defined $result;
                return $orig_value;
            }

            return $orig_value;
        }
    },
        -RefEmpty => {
                -value_position => 3,
                -code => sub {

                        my    $subject_info = shift || '';
            local $_            = shift;
            my    $message_ref  = shift;

                        my $content_ref = _ref_empty_check($subject_info, $_, $message_ref);

                        return undef unless defined $content_ref;

                        my $count_results = scalar @$content_ref;
                        return 0 unless $count_results;

                        _do_fail($subject_info, sub { "Should be empty, but contains $count_results entries: [ ".
                                                                                          join (", ", @$content_ref)." ];" });
                        
                        return $count_results;
                }
    },
};

# ------------------------------------------------------------------------------
#
# internal Methods
#
# ------------------------------------------------------------------------------

sub _handle_enum_explained {
    my $transform_key_ref = shift;
    my $transformed_text  = shift;
    my $rule_name         = shift;
    my @enum_args;
    my @enums_list;
    my %enums;
    
    foreach my $arg (@_) {
        if ($arg eq 1 or $arg eq 0) {
            # arg is complete
            my $last_idx = $#enum_args;
            $fail_action->("not enough configuration values for enum '$enum_args[0]'")
                if $last_idx < 1;
            my $explanation = $enum_args[$last_idx];
            map { my $key = $transform_key_ref ? $transform_key_ref->($_): $_;
                  $enums{$key} = $explanation;
                  push (@enums_list, $key);
              } @enum_args[0..--$last_idx];
            @enum_args = ();
        }
        else {
            push (@enum_args, $arg);
        }
    }

    my $validation_sub_ref = $transform_key_ref
        ? sub { defined $_ && defined $enums{$transform_key_ref->($_)} }
            : sub { defined $_ && defined $enums{$_} };
        
    return ($rule_name,
            -where   => $validation_sub_ref,
            -enum    => \%enums,
            -message => sub { "$rule_name: value $_ unknown, allowed values$transformed_text ".
                                  "are: [ ".join (", ", @enums_list)." ]" }
        );
}

sub _check_parent_rules {
    my    $rule_name    = shift;
    local $_            = shift;

    my $orig_value      = $_;

    my $rule_ref = $rule_store->{$rule_name};

    unless ($rule_ref) {
        $fail_action->("unknown rule '$rule_name' for validation");
        return 0; # in case of fail action doesn't die
    }
    
    if (defined $rule_ref->{-as}) {
        return 0 unless _check_parent_rules($rule_ref->{-as}, $_);
    }
    
    return $rule_ref->{-where}->();
}

sub _ref_empty_check {
        my    $subject_info = shift || '';
        local $_            = shift;
        my    $message_ref  = shift;

        my $ref_type = ref($_);

        unless ($ref_type) {
                _do_fail($subject_info, sub { "Not a reference: $_" });
                return undef;
        } 
        
        my $get_contents_ref = $get_content_subs->{$ref_type};

        unless ($get_contents_ref) {
                _do_fail($subject_info, sub { "could not check, if $ref_type is empty" });
                return undef;
        }
        
        return $get_contents_ref->();
}

sub _do_fail {
    my $subject_info = shift;
    my $message_ref  = shift;
        
    $_ = defined ($_) ? "'$_'" : '<undef>';

    my $message = $get_caller_info->()."($subject_info): ".$message_ref->();
    push (@$message_store, $message) if $message_store;
    return $fail_action->($message);
}

# ------------------------------------------------------------------------------
#
# API Methods
#
# ------------------------------------------------------------------------------

sub _init_run_API {
    *npar      = *named_parameter;

    *parameter = *_do_validate;
    *par       = *parameter;

    *validate  = *_do_validate;

    *p_end     = *parameters_end ;

}

sub convert_to_named_params {
        my $array_ref = validate (args => ArrayRef => shift);

        validate (arg_count => Even => scalar @$array_ref =>
                                  sub { "Even number of args needed to build a hash, but arg-count = $_" });
        return @$array_ref;
}

sub parameters_end {
    my $container_ref = par (container_ref => -Or => [HashRef => 'ArrayRef'] => shift);
    my $message_text  = par (message_text  => Scalar => shift) || "extra parameters found";

    my $container_type = ref ($container_ref);
    if ($container_type eq 'ARRAY') {
	validate (parameters => sub { scalar @$container_ref == 0 } => $container_ref => sub { "$message_text: [ '".join ("', '", @$container_ref)."' ]"; });
	return scalar @$container_ref;
    }
    elsif ($container_type eq 'HASH') {
	my @arg_names = keys %$container_ref;
	validate (parameters => sub { scalar @arg_names == 0 } => $container_ref => sub { "$message_text: [ '".join ("', '", @arg_names)."' ]"; });
	return scalar @arg_names;
    }

    _do_fail("parameters_end()", sub { "unknown reference type $container_ref" });
    return -1;
}

# ------------------------------------------------------------------------------
#
# Messages and Validation Mode
#
# ------------------------------------------------------------------------------

sub validation_messages {
    my $mode = shift || '';

    return $message_store if !$message_store || !$mode || $mode ne '-clear';

    my @messages = @$message_store;
    @$message_store = ();
    return \@messages;
}

sub prepare_validation_mode {
    my $mode = lc(shift);

    my $new_fail_action = $fail_action;
    my $new_off         = $off;

    unless (is_valid(mode => -Enum => [ qw (die warn ignore off) ] => $mode)) {
	croak "unknown mode for Scalar::Validation selected: '$mode'";
    }

    # print "#### Select validation mode: $mode\n";
        
    if ($mode eq 'die') {
        $new_fail_action   = $croak_sub;
        $new_off = 0;
    }
    elsif ($mode eq 'warn') {
        $new_fail_action   = sub { carp "Warning: ", @_; return undef; };
        $new_off = 0;
    }
    elsif ($mode eq 'ignore') {
        $new_fail_action   = sub { return undef; };
        $new_off = 0;
    }
    elsif ($mode eq 'off') {
        $new_fail_action = sub { return undef; };
        $new_off = 1;
    } else {
	$fail_action->("prepare_validation_mode(): unknown validation mode $mode used");
    }

    return $new_fail_action, $new_off;
}

# ------------------------------------------------------------------------------
#
# Rules
#
# ------------------------------------------------------------------------------

sub declare_rule {
    my $rule    = shift || '';
    my %options = @_;

    # --- rule ---
    validate (rule => Filled => $rule    => sub { "not valid: rule not set" });
    if       (rule_known($rule)) { $fail_action->("rule '$rule': already defined"); }

    # --- where ---
    my $where_cond_ref = $options{-where};
    validate (-where => Filled => $where_cond_ref =>  sub {"rule '$rule': where condition is missing"});
    validate (-where => CodeRef => $where_cond_ref => sub { "rule '$rule': where condition is not a code reference: '$where_cond_ref'" });

    # --- message ---
    $options{-message} = sub { "Value $_ is not valid for rule '$rule'" } unless $options{-message};

    # --- ok, store now ---
    $rule_store->{$rule} = \%options;

    return $rule;
}

sub Enum {
    my $rule_name  = shift;
    my %enums      = map { $_ => 1 } @_;
    my @enums_list = @_;

    return ($rule_name,
            -where   => sub { defined $_ && defined $enums{$_} },
            -enum    => \%enums,
            -message => sub { "$rule_name: value $_ unknown, allowed values are: [ ".join (", ", @enums_list)." ]" }
        );
}

sub enum {
    my $rule_name  = shift;
    my %enums      = map { lc($_) => 1 } @_;
    my @enums_list = map { lc($_)      } @_;

    return ($rule_name,
            -where   => sub { defined $_ && defined $enums{lc($_)} },
            -enum    => \%enums,
            -message => sub { "$rule_name: value $_ unknown, allowed values (transformed to lower case) are: [ ".join (", ", @enums_list)." ]" }
        );
}

sub Enum_explained {
    _handle_enum_explained(undef, "", @_);
}

sub enum_explained {
    _handle_enum_explained(sub { lc($_[0])}, " (transformed to lower case)", @_);
}

sub greater_than {
        my $limit      = shift;
    my $type       = shift;
        return ({ -as    => $type,
                          -where   => sub { $_ > $limit },
                          -message => sub { "$_ > $limit failed. Value is not of type $type or not greater than limit."},
                  },
                        @_);
}

sub equal_to {
        my $compare      = shift;
    my $type         = shift;
        if ($type eq 'String') {
                return ({ -as    => $type,
                                  -where   => sub { $_ eq $compare },
                                  -message => sub { "$_ eq $compare failed. Value is not of type $type or different."},
                          },
                                @_);
        }
        
        return ({ -as    => $type,
                          -where   => sub { $_ == $compare },
                          -message => sub { "$_ == $compare failed. Value is not of type $type or different."},
                  },
                        @_);
}

sub less_than {
        my $limit      = shift;
    my $type       = shift;
        return ({ -as    => $type,
                          -where => sub { $_ < $limit },
                          -message => sub { "$_ < $limit failed. Value is not of type $type or not less than limit."},
                  },
                        @_);
}

sub rule_known {
    my $rule = shift;

    validate (rule => Filled => $rule => sub { "rule not set" });

    $rule = '' unless defined $rule;
    return $rule_store->{$rule} ? $rule : '';
}

# ------------------------------------------------------------------------------
#
# Validation
#
# ------------------------------------------------------------------------------

# --- helpful for tests ------------------------------------------------

sub is_valid {
    my $valid = 1; 
    local ($fail_action) = sub { $valid = 0 };
    
    validate(@_);

    return $valid;
}

# --- return value           if valid   ---------------
# --- return corrected value if invalid   ---------------
sub validate_and_correct {
    my ($validation_options_ref,        # options for validate
        $options_ref
    ) = @_;

    my $correction_action = $options_ref->{-correction}; # action that does corrections in value
    unless ( defined $validation_options_ref->[-1]) {
        $validation_options_ref->[-1] = $options_ref->{-default} if exists $options_ref->{-default};
    }

    if ($correction_action) {
        local ($fail_action) = sub {
            s/^'//o;
            s/'$//o;
            $correction_action->($_);
        };
        return validate(@$validation_options_ref);
    }
    return validate(@$validation_options_ref);
}

# --- don't name key twice, deletes validated values out of hash -------------------------
#   named_parameter
sub named_parameter {
        my $first_arg = shift;
        my $hash_ref;
        
        my $msg_ref;
        my $key;
        my $option_args_ref;

        my $args_ref = \@_;

        unless (is_valid(key  => Scalar  => $first_arg)) {
                $args_ref        = validate (validation_args => ArrayRef => $first_arg);
                $key             = shift @$args_ref;
                $option_args_ref = shift;
        }
        else {
                $key = $first_arg;
        }

        $key = validate (key  => Scalar  => $key);

        $hash_ref = pop @$args_ref;
        
        unless (is_valid(option_ref => HashRef => $hash_ref)) {
                $msg_ref  = validate (message_ref => CodeRef => $hash_ref);
                $hash_ref = validate (option_ref  => HashRef => pop @$args_ref);
        }

        my $value = delete $hash_ref->{$key};

        unless ($value) {
                if ($option_args_ref) {
                        $value = $option_args_ref->{-default};
                        print "used default $key => '$value'\n";
                        # print $option_args_ref->{-description}."\n";
                }
        }

        return validate ($key, @$args_ref, $value, $msg_ref);
}

# --- return value        if valid   ---------------
# --- call   $fail_action if invalid ---------------
sub _do_validate {
    if ($off) {
        my $value_pos = 2;
        $value_pos = $special_rules->{$_[1]}->{-value_position} if $special_rules->{$_[1]};
        return $_[$value_pos] if $value_pos >= 0;
    }

    my    $subject_info = shift || '';
    my    $rule_info    = shift;

    unless ($rule_info) {
        $fail_action->("rule for validation not set");
        return $_; # in case of fail action doesn't die
    }

    my $rule_ref   = $rule_store->{$rule_info};

    unless ($rule_ref) {
        my $special_rule = $special_rules->{$rule_info}->{-code};

                return $special_rule->($subject_info, @_) if $special_rule;

                my $ref_type = ref ($rule_info);

                unless ($ref_type) {
                        $fail_action->("unknown rule '$rule_info' for validation");
            return shift; # in case of fail action doesn't die
                }
                elsif ($ref_type eq 'HASH') { # given rule
                        $rule_ref = $rule_info;
                        # TODO: validate rule ...
                }
                elsif ($ref_type eq 'CODE') { # where condition for rule
                        $rule_ref = {
                                -where   => $rule_info,
                                -message => sub { "$_ does not match free defined rule" },
                        };
                }
                else {
                        $fail_action->("cannot handle ref type '$ref_type' of rule '$rule_info' for validation");
                        return shift; # in case of fail action doesn't die
                }
    }

    local $_            = shift;
    my    $message_ref  = shift;

    my $orig_value       = $_;
    my $test_message_ref = $message_ref || $rule_ref->{-message};

    my $parent_is_valid = defined $rule_ref->{-as}
        ? _check_parent_rules($rule_ref->{-as}, $_)
            : 1;

    unless ($parent_is_valid && $rule_ref->{-where}->()) {
        $_ = defined ($_) ? "'$_'" : '<undef>';
        my $message = $get_caller_info->()."($subject_info): ".$test_message_ref->();
        push (@$message_store, $message) if $message_store;
        my $result = $fail_action->($message);
        return $result if defined $result;
    }

    return $orig_value;
}

1;
__END__

=head1 NAME

Scalar::Validation

Simple rule based validation package for scalar values

=head1 VERSION

This documentation refers to version 0.500 of Scalar::Validation

=head1 SYNOPSIS

  use Scalar::Validation qw(:all);

  my $int_1    = validate int_1   => Int   => 123;
  my $float_1  = validate float_1 => Float => 3.1415927;

  my $para_1   = par parameter_1 => -Range => [1,5] => Int => shift;

  my $int_2    = validate (int_2    => -And => [Scalar => 'Int'],  123);
  my $int_3    = validate (int_3    => -Or  => [Int => 'CodeRef'], 123);
  my $code_ref = validate (code_ref => -Or  => [Int => 'CodeRef'], sub { 123; });

  my $enum_abc = validate (parameter => -Enum => {a => 1, b => 1, c => 1}, 'c');

  my $int_4    = validate (int_4   => -Optional => Int    =>                             undef);
  my $int_5    = validate (int_5   => -Optional => -And   => [Scalar => Int => 0] =>     undef);
  my $int_6    = validate (int_6   => -Optional => -Or    => [Int => CodeRef => 0] =>    undef);
  my $enum_2   = validate (enum_2  => -Optional => -Enum  => {a => 1, b => 1, c => 1} => undef);
  my $range_1  = validate (range_1 => -Optional => -Range => [1,5] => Int =>             undef);

Just checks, never dies:

  is_valid(valid_1 => Int => 123);   # is valid,     returns 1;
  is_valid(valid_2 => Int => 1.23);  # is not valid, returns 0;
  is_valid(valid_3 => Int => 'a');   # is not valid, returns 0;
  is_valid(valid_4 => Int => undef); # is not valid, returns 0;

Free defined rules or wheres only (also for validate(...))

  # be careful, doesn't check that $_ is an integer!
  is_valid (free_where_greater_zero => sub { $_ && $_ > 0} => 2);  # is valid, returns 1

  is_valid (free_rule_greater_zero => { -as      => Int =>
                                                                                -where   => sub { $_ && $_ > 0},
                                                                                -message => sub { "$_ is not > 0" },
                                                                          }
            => 2); # is valid, returns 1

  my $my_rule = { -as => Int => -where => sub { $_ && $_ > 0} => -message => sub { "$_ is not > 0" };

  is_valid (free_rule_greater_zero => $my_rule => 2);              # is valid, returns 1


=head1 DESCRIPTION

This class implements a fast and flexible validation for scalars.
It is implemented functional to get speed and some problems using global rules for all ;).

=head2 Dies by error message

  validate (parameter => -And => [Scalar => 'Int'],  {} );
  validate (parameter => -And => [Scalar => 'Int'],  [] );
  validate (parameter => -And => [Scalar => 'Int'],  sub { 'abc'; });

=head2 Just check without die

  print is_valid(parameter => -And => [Scalar => 'Int'],  123) ." => 123 is int\n";
  print is_valid(parameter => -And => [Scalar => 'Int'],  {} ) ." => {} is no scalar\n";
  print is_valid(parameter => -And => [Scalar => 'Int'],  [] ) ." => [] is no scalar\n";
  print is_valid(parameter => -And => [Scalar => 'Int'],  sub { 'abc'; }) ." => sub { 'abc'; } is no scalar\n";

=head3 Get validation messages

Message store has to be localized. The only safe way to deal with
recursive calls and die! So use a block like this to store messages

  my @messages;
  {
      local ($Scalar::Validation::message_store) = [];
  
      my $result = is_valid(parameter => -And => [Scalar => 'Int'],  {} );
              
      @messages = @{validation_messages()} unless $result;
  }

=head2 As parameter check for indexed arguments

It can be also used a parameter check for unnamed and named parameters, see

  sub create_some_polynom {
      my $max_potenz = par maximum_potenz => -Range => [1,5] => Int => shift;

      parameters_end \@_;

      # --- run sub -------------------------------------------------

      my $polynom = '';
      map { $polynom .= " + ".int (100*rand())."*x^".($max_potenz-$_); } (0..$max_potenz);

      return $polynom;
  };

  sub create_some_polynom_named {
      my %pars = convert_to_named_params \@_;

      my $max_potenz = npar -maximum_potenz => -Range => [1,5] => Int => \%pars;

      parameters_end \%pars;

      # --- run sub -------------------------------------------------

      my $polynom = '';
      map { $polynom .= " + ".int (100*rand())."*x^".($max_potenz-$_); } (0..$max_potenz);

      return $polynom;
  };


  print create_some_polynom(1)."\n";
  print create_some_polynom(2)."\n";
  print create_some_polynom(3)."\n";
  print create_some_polynom(4)."\n";
  print create_some_polynom(5)."\n";

  print create_some_polynom_named(-maximum_potenz => 4);

Dies by error message

  print create_some_polynom(5.5)."\n";
  print create_some_polynom(6)."\n";
  print create_some_polynom(6, 1)."\n";

=head2 Rules

You can and should create your own rules, i.e.

  declare_rule (
      Positive =>  -as      => Int =>           # Parent rule is optional
                   -where   => sub { $_ >= 0 },
                   -message => sub { "value '$_' is not a positive integer" },
  );

  rule_known(Unknown  => 1); # returns 0 (false)
  rule_known(Positive => 1); # returns 1 (true)

=head2 Create Own Validation Module

You should not use Scalar::Validation direct in your code.

Better is creating an own module My::Validation, that adds the rules
you need and only exports the subs the developers in your project
should use:

  use My::Validation;

  my $v_my_type = validate v_int => my_type => new MyType(); 

=head2 More Examples

Have a look into Validation.t to see what else is possible

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2014 by Ralf Peine, Germany.  All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.6.0 or,
at your option, any later version of Perl 5 you may have available.

=head1 DISCLAIMER OF WARRANTY

This library is distributed in the hope that it will be useful,
but without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.

=cut

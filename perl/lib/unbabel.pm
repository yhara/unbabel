package Unbabel;

use strict;
use warnings;

## called when this module is use'd
#sub import {
#  print @_; 
#}

sub new {
  my ($class, $source) = @_;
  return bless {
  }, $class;
}

# signature

sub signature_of {
  my ($self, $script, $comment) = @_;
  die "error: need a script" unless defined($script);
  $script =~ ($comment . "\\s*(\\S+)\\s*::\\s*((?:\\S+\\s*->\\s*)+)(\\S+)\\s*"); 
  my ($funcname, $argtypes, $rettype) = ($1, $2, $3);
  die "error: signature not found: " . $script unless defined($1);
  my @splitted = split(/\s*->\s*/, $argtypes);
  return [$funcname, \@splitted, $rettype];
  #return {"funcname" => funcname, "argtypes" => argtypes, "rettype" => 3};
}

# langdef

sub load_langdef {
  my ($self, $path) = @_;
  my $langdef = {};

  my $in_stub = 0;
  my $stub = "";
  my $lines = "";
  open my $fh, '<', $path or die $!;
  while (defined(my $line = <$fh>)) {
    if ($in_stub) {
      $stub .= $line;
    }
    elsif ($line eq "stub:\n") {
      $in_stub = 1;
    }
    else {
      chomp($line);
      my ($key, $value) = split(/:/, $line);
      $langdef->{$key} = $value;
    }
  }
  $langdef->{"stub"} = $stub;

  return $langdef;
}

# stub

sub make_stub {
  my ($self, $src, $langdef, $args) = @_;
  my $sig = $self->signature_of($src, $langdef->{"comment"});
  my $funcname = $sig->[0];
  my $argments = $self->format_args($args, $sig);
  my $stub = $langdef->{"stub"};
  $stub =~ s/<FUNCTION_DEFINITION>/$src/g;
  $stub =~ s/<FUNCTION_NAME>/$funcname/g;
  $stub =~ s/<ARGUMENTS>/@$args/g;

  return $stub;
}

sub format_args {
  return 1;
  my ($self, $args, $sig) = @_;
  my $formatted = map { $self->format_arg($_, $sig, 1) } @$args;
  return join(", ", $formatted);
}

sub format_arg {
  my ($self, $arg, $type, $langdef, $outer) = @_;
  if ($type eq "Int") {
    return "" . $arg;
  }
  elsif ($type eq "String") {
    $arg =~ s/\\/\\\\/g;
    $arg =~ s/\n/\\n/g;
    $arg =~ s/\t/\\t/g;
    return '"' . $arg . '"';
  }
  elsif ($type =~ /^\[(.*)\]$/) {
    my ($pre, $between, $post) = @{$self->parens($langdef->{"argtype"})};
    my $elem_type = $1;
    my @formatted = map { $self->format_arg($_, $elem_type, $langdef, 0) } @$arg;
    return $pre . join($between, @formatted) . $post;
  }
  else {
    die "error: bad type " . $type . " for " . $arg;
  }
}


sub parens {
  my ($self, $argtype, $outer) = @_;
  my ($pre, $between, $post);
  if ($argtype eq "square") {
    ($pre, $between, $post) = ("[", ", ", "]");
  }
  elsif ($argtype eq "paren") {
    ($pre, $between, $post) = ("(", ", ", ")");
  }
  elsif ($argtype eq "sexp") {
    ($pre, $between, $post) = ("(", " ",  ")");
  }
  else {
    die "error: unknown argtype: " . $argtype;
  }
  if ($argtype eq "sexp" && $outer) {
    $pre = "'" . $pre;
  }
  return [$pre, $between, $post];
}


#sub format_arg {
#  my ($self, $arg, $sig, $outer) = @_;
#  if ((ref($arg)) eq "ARRAY") { # list
#    return "notimplementedyet";
#  }
#  elsif (($arg ^ $arg) eq "") { # string
#    die "stringstring";
#    $arg =~ s/\\/\\\\/g;
#    $arg =~ s/\n/\\n/g;
#    $arg =~ s/\t/\\t/g;
#    return '"' . $arg . '"';
#  }
#  elsif (($arg ^ $arg) == 0) { # maybe number?
#    return "" . $arg;
#  }
#  die "error: unknown type of argument: " . $arg;
#}
#
#sub foo {
#  my ($self, $str) = @_;
#  $str =~ s/a/A/g;
#  return $str;
#}

1;
#
#package Babel::Ruby;
#
#sub new {
#  my ($class, $source) = @_;
#  return bless {
#    "source" => $source
#  }, $class;
#}
#
#sub run {
#  my ($self, @args) = @_;
#  my $args = \@args # reference
#
#  return 55;
#}
#
#1;

use strict;
use warnings;
use Test::More qw(no_plan);

use Unbabel qw(Ruby);

# preparation

my $FIB = <<EOD;
# fib :: Int -> Int
def fib(n)
  case n
  when 0 then 0
  when 1 then 1
  else fib(n-2) + fib(n-1)
  end
end
EOD

my $babel = Unbabel->new();

# signature

my $sig = $babel->signature_of($FIB, "#");
is( $sig->[0], "fib");
is( $sig->[1]->[0], "Int");
is( $sig->[2], "Int");

my $sig2 = $babel->signature_of(";; foo :: [[Int]] -> String -> [[String]]", ";");
is( $sig2->[0], "foo");
is( $sig2->[1]->[0], "[[Int]]");
is( $sig2->[1]->[1], "String");
is( $sig2->[2], "[[String]]");

# langdef

my $langdef = $babel->load_langdef("../templates/ruby.unb");
is($langdef->{"name"}, "Ruby");
is($langdef->{"comment"}, "#");
is($langdef->{"command"}, "ruby");
is($langdef->{"argtype"}, "square");
is($langdef->{"stub"}, <<EOD);
<FUNCTION_DEFINITION>

class Babel
  def self.to_sexp(x)
    case x
    when Numeric
      x.to_s
    when String
      x.inspect
    when Array
      '(' + x.map{|item| Babel.to_sexp(item)}.join(' ') + ')'
    else
      raise
    end
  end
end
print Babel.to_sexp(<FUNCTION_NAME>(<ARGUMENTS>))
EOD

# stub

is($babel->make_stub($FIB, $langdef, [10]), <<EOD);
$FIB

class Babel
  def self.to_sexp(x)
    case x
    when Numeric
      x.to_s
    when String
      x.inspect
    when Array
      '(' + x.map{|item| Babel.to_sexp(item)}.join(' ') + ')'
    else
      raise
    end
  end
end
print Babel.to_sexp(fib(10))
EOD

is($babel->format_arg(10, "Int", $langdef, 1), "10");
is($babel->format_arg("10", "String", $langdef, 1), "\"10\"");
is($babel->format_arg([10, 20], "[Int]", $langdef, 1), "[10, 20]");

#my $fib = Babel::Ruby->new($FIB);
#is( $fib->run(10, 20, 30), 55);


$LOAD_PATH << File.expand_path("../lib", File.dirname(__FILE__))
require 'unbabel'

describe "Unbabel21" do 

  it "should call Perl function" do
    fib = Unbabel::Perl.new((<<-EOD).unindent)
      use strict;
      use warnings;

      # fib :: Int -> Int
      sub fib {
        my $x = shift;

        if($x == 0){
          return 0;
        }
        elsif($x == 1){
          return 1;
        }
        else{
          return fib($x-2) + fib($x-1);
        }
      }
    EOD
    fib[10].should == 55
  end

end

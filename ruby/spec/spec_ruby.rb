$LOAD_PATH << File.expand_path("../lib", File.dirname(__FILE__))
require 'unbabel'

describe "Unbabel21" do 

  it "should call Ruby function :-)" do
    fib = Unbabel::Ruby.new((<<-EOD).unindent)
      # fib :: Int -> Int
      def fib(n)
        case n
        when 0 then 0
        when 1 then 1
        else fib(n-2) + fib(n-1)
        end
      end
    EOD
    fib[10].should == 55
  end

end

$LOAD_PATH << File.expand_path("../lib", File.dirname(__FILE__))
require 'unbabel'

describe "Unbabel21" do 

  it "should call Ruby 1.9 function ;-)" do
    fib = Unbabel::Ruby19.new((<<-EOD).unindent)
      # fib :: Int -> Int
      def fib(n)
        case n
        when 0 ; ->(n){ 0 }
        when 1 ; ->(n){ 1 }
        else ->(n){ fib(n-2) + fib(n-1) }
        end.call(n)
      end
    EOD
    fib[10].should == 55
  end

end

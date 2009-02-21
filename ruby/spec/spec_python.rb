$LOAD_PATH << File.expand_path("../lib", File.dirname(__FILE__))
require 'unbabel'

describe "Unbabel21" do 

  it "should call Python function" do
    fib = Unbabel::Python.new((<<-EOD).unindent)
      # fib :: Int -> Int
      def fib(x):
        if x == 0:
          return 0
        elif x == 1:
          return 1
        else:
          return fib(x-2) + fib(x-1)
    EOD
    fib[10].should == 55
  end

end

$LOAD_PATH << File.expand_path("../lib", File.dirname(__FILE__))
require 'unbabel'

describe "Unbabel" do 

  it "should call Java function" do
    fib = Unbabel::Java.new((<<-EOD).unindent)
      // fib :: Int -> Int
      public static int fib (int x) {
        switch(x){
          case 0: return 0; 
          case 1: return 1;
          default: 
            return fib(x-2) + fib(x-1);
        }
      }
    EOD
    fib[10].should == 55
  end

end

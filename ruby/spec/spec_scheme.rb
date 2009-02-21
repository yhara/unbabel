$LOAD_PATH << File.expand_path("../lib", File.dirname(__FILE__))
require 'unbabel'

describe "Unbabel21" do 

  it "should call Scheme function" do
    fib = Unbabel::Scheme.new((<<-EOD).unindent)
      ;; fib :: Int -> Int
      (define (fib x)
        (case x
          ((0) 0)
          ((1) 1)
          (else (+ (fib (- x 2)) (fib (- x 1))))))
    EOD
    fib[10].should == 55
  end

end

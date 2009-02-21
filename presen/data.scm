(define *presen*
  '#(("<br><div style='float:right; font-size:small;'>2008.08 - LL Future</div><strong style='font-weight:bold;'>Unbabel</strong><br>- LLにLLを埋め込んでみた"
;      "<strong>タイトルが変わりました( Babel21 → Unbabel )</strong>"
      "ネットワーク応用通信研究所<br>
      yhara (原 悠)<br>
powered by <a href='http://www.biwascheme.org/'>BiwaScheme</a>
"
;       "<strong>(右上の「→」ボタンを押してください)</strong>"
       ) 

;; introduction ----------------------------------------------------

     (("自己紹介" . "I am a programmer working at Matsue") 
       "yhara (原 悠)"
;       "BiwaScheme (JavaScriptで書かれたSchemeインタプリタ)"
       "松江にある会社でプログラマーとして働いています"
       "島根は鳥取の左です"
       )

     (("宣伝" . "I'm writing a book named 'Esoteric Language'")
      "最近LLの本を書いています"
      "「Esoteric Language(仮)」"
      "Brainf*ck, Whitespaceといった<font color='red'>超Lightweight</font> Language"
      )
     (("宣伝" . "I'm writing a book named 'Esoteric Language'")
      "最近LLの本を書いています"
      "「Esoteric Language(仮)」"
      "Brainf*ck, Whitespaceといった<font color='red'>超Lightweight</font> Language"
      "Whitespaceで書かれたHello, worldが！"
      )
     (("　" . "Don't ask me how long does it take :-(")
      "禁句:「いつ出るんですか？」"
      )

;     (("代表作" . "I made biwascheme")
;      "foo"
;      "bar")


;; future ----------------------------------------------------

     (("言語の未来" . "Many languages will be alive, I think")
      "100年後まで生き残る言語はどれかと考える"
      "2078年、言語大統一理論？"
      "どれか一つしか生き残らないってことはなさそう"
      )
     (("なぜか？" . "Right language in the right place")
      "言語には向き・不向きがある"
      "適材適所"
      "なので、未来＝いろんな言語を使い分ける時代？"
      )
     (("なぜか？" . "Right language in the right place")
      "言語には向き・不向きがある"
      "適材適所"
      "なので、未来＝いろんな言語を使い分ける時代？"
      "<strong>ていうか既にそうなってるよね</strong>"
      )
     (("例: インラインアセンブラ" . "Inline assembler (asm embed in a C code)")
      "Cのコードにアセンブラを埋め込む<pre>#include &lt;stdio.h&gt;

int main() {
    char ch1, ch2;
    <span class='asm'>__asm {
        mov ch1, 0x41
        mov ch2, 0x42
    }</span>

    printf(\"ch1=%c, ch2=%c¥n\", ch1, ch2);

    return 0;
}</pre>")
     (("例: PerlのInline::C" . "Inline::C (C code embed in a Perl script)")
      "PerlにCのコードを埋め込む<pre>use Inline C;
for (my $x in ('Ingy', 42)) {
  greet($x);
}
__END__    
__C__ <span class='c-lang'>
void greet(char* name) {
  printf(\"Hello %s!\n\", name);
} </span></pre>"
      )
     (("次は？" . "What's next?")
      "asm in C"
      "C in LL  　　　　　 ←いまここ"
      )
     (("次は？" . "What's next?")
      "asm in C"
      "C in LL  　　　　　 ←いまここ"
      "<strong>LLにLLを埋め込む　←未来</strong>")

;; rubiskell ----------------------------------------------------

     (("[作ってみた] Rubiskell" . " I made `Rubiskell': embed Haskell code in Ruby script ")
      "RubyスクリプトにHaskellを埋め込む<pre><span class='ruby'>require 'rubiskell'
fib = Haskell.new(&lt;&lt;EOD)<span class='haskell'>
  fib :: Int -> Int
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-2) + fib (n-1)</span>
EOD
puts \"fib 5 is #{fib.call(5)}.\"</span></pre>")
     (("[作ってみた] Rubiskell" . " I made `Rubiskell': embed Haskell code in Ruby script ")
      "RubyスクリプトにHaskellを埋め込む<pre>require 'rubiskell'
<span class='ruby'>fib</span> = Haskell.new(&lt;&lt;EOD)<span class='haskell'>
  fib :: Int -> Int
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-2) + fib (n-1)</span>
EOD
puts \"fib 5 is #{fib.call(5)}.\"</pre>")
     (("[作ってみた] Rubiskell" . " I made `Rubiskell': embed Haskell code in Ruby script ")
      "RubyスクリプトにHaskellを埋め込む<pre>require 'rubiskell'
<span class='ruby'>fib</span> = Haskell.new(&lt;&lt;EOD)<span class='haskell'>
  fib :: Int -> Int
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-2) + fib (n-1)</span>
EOD
puts \"fib 5 is #{<span class='ruby'>fib.call(5)</span>}.\"</pre>")

     (("仕組み" . " How it works")
      "スクリプトの断片に<pre><span class='haskell'>  fib :: Int -> Int
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-2) + fib (n-1)</span></pre>")
     (("仕組み" . " Add 'main' function to the snippet...")
      "mainを付け加えて<pre><span class='haskell'>  fib :: Int -> Int
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-2) + fib (n-1) 
  main = do args <- getArgs
            print $ apply fib $ _read $ head args
    where
      _read :: String -> Int
      _read = read
      apply f (a) = f a</span>
</pre>")
     (("仕組み" . "and just execute it")
      "runghcで実行する<pre>$ runghc tmp.hs
55</pre>"
      "fibを呼ぶたびにHaskellのインタプリタが走る"
      )
     (("仕組み" . "and just execute it")
      "runghcで実行する<pre>$ runghc tmp.hs
55</pre>"
      "fibを呼ぶたびにHaskellのインタプリタが走る"
      "<strong>富豪的プログラミング</strong>"
      )

     (("発想" . "Write UI in Ruby, write algorithm in Haskell and combine them")
      "Haskellはアルゴリズムの記述とかに強い"
      "でもIO処理は苦手"
      "RubyはIO処理が得意"
      "組み合わせたら最強じゃね？")

;     (("よくある要望" . " Unbabel brings you pattern matching in Ruby! ")
;      "Rubyにもパターンマッチがほしい！"
;      "→ <del>Haskell使えば？</del>"
;      "→ それRubiskellでできるよ"
;      )

;; unbabel ----------------------------------------------------

;     (("一般化できないか" . " Generalization")
;      "Ruby in Python (Rubython?)"
;      "Python in Perl (Pyerl?)"
;      "Perl in Ruby (Peruby?)"
;     )
;     (("一般化できないか" . " Generalization")
;      "Ruby in Python (Rubython?)"
;      "Python in Perl (Pyerl?)"
;      "Perl in Ruby (Peruby?)"
;      "名前を付けるのが面倒<strong>→Unbabel</strong>"
;     )

     (("[一般化してみた] Unbabel" . "Unbabel = generalization to these languages")
      "呼び出す側: Ruby, Python, Scheme, Haskell, <del>Perl</del><small>←間に合いませんでした（＞＜）</small>"
      "呼び出される側: Ruby, Python, Perl, Scheme, Java")

;     (("仕組み" . "*.unb: language definition file")
     (("仕組み: 言語ごとに定義ファイル" . "*.unb: language definition file")
       "<pre>name:Ruby
comment:#
command:ruby
argtype:square
stub:
<strong>&lt;FUNCTION_DEFINITION></strong>
class Babel
  def self.to_sexp(x)
    case x
    when Numeric then x.to_s
    when String  then x.inspect
    when Array   then '(' + x.map{|item| Babel.to_sexp(item)}.join(' ') + ')'
    else raise ArgumentError
    end
  end
end
print Babel.to_sexp(<strong>&lt;FUNCTION_NAME></strong>(<strong>&lt;ARGUMENTS</strong>>))
</pre>"
      )

;; examples ----------------------------------------------------

;     (("例: PythonにPerlを埋め込む" . "Example: Call perl function from a Python script") 
;"<pre>FIB = \"\"\"
;  # fib :: Int -> Int
;  sub fib {
;    my $x = shift;
;
;    if($x == 0){ return 0; }
;    elsif($x == 1){ return 1; }
;    else{ return fib($x-2) + fib($x-1); }
;  }
;\"\"\"
;unbabel.init(unbabel, '../templates/')
;fib = unbabel.perl(FIB)</pre>")

     (("例: PythonにPerlを埋め込む" . "Example: Call perl function from a Python script") 
"<pre><span class='python'>FIB = \"\"\" <span class='perl'>
  # fib :: Int -> Int
  sub fib {
    my $x = shift;

    if($x == 0){ return 0; }
    elsif($x == 1){ return 1; }
    else{ return fib($x-2) + fib($x-1); }
  } </span>
\"\"\"
unbabel.init(unbabel, '../templates/')
fib = unbabel.perl(FIB)</span></pre>")

     (("例: RubyにSchemeを埋め込む" . "Example: Scheme code in a Ruby script") 
      "<pre><span class='ruby'>plus = Unbabel::Scheme(<<-EOD) <span class='scheme'>
  ;; plus :: Int -> Int -> Int
  (define (plus x y)
    (+ x y))</span>
EOD
plus[2, 3]</span></pre>"
      )
     (("例: SchemeにRubyを埋め込む" . "Example: Ruby code in a Scheme script") 
      "<pre><span class='scheme'>(define plus (babel-ruby \"</span><span class='ruby'>
  # plus :: Int -> Int -> Int
  def plus(x, y)
    x + y
  end\"</span><span class='scheme'>))
(plus 2 3)</span></pre>"
      )

;     (("Ruby in Scheme in Ruby" . "You can call Ruby function via Scheme") 
;      "<pre>plus = Unbabel::Scheme(<<-EOD) 
;  ;; plus :: Int -> Int -> Int
;  (define (plus x y)
;    ((unbabel-ruby \" 
;      # plus :: Int -> Int -> Int
;      def plus(x, y)
;        x + y
;      end\"
;    ) x y))
;EOD
;plus[2, 3]"
;      )
;     (("Ruby in Scheme in Ruby" . "You can call Ruby function via Scheme") 
;      "<pre><span class='ruby'>plus = Unbabel::Scheme(<<-EOD) </span>
;  ;; plus :: Int -> Int -> Int
;  (define (plus x y)
;    ((unbabel-ruby \" 
;      # plus :: Int -> Int -> Int
;      def plus(x, y)
;        x + y
;      end\"
;    ) x y))<span class='ruby'>
;EOD
;plus[2, 3]</span>"
;      )
;     (("Ruby in Scheme in Ruby" . "You can call Ruby function via Scheme") 
;      "<pre><span class='ruby'>plus = Unbabel::Scheme(<<-EOD) <span class='scheme'>
;  ;; plus :: Int -> Int -> Int
;  (define (plus x y)
;    ((unbabel-ruby \" </span></span>
;      # plus :: Int -> Int -> Int
;      def plus(x, y)
;        x + y
;      end<span class='scheme'>\"
;    ) x y))</span><span class='ruby'>
;EOD
;plus[2, 3]</span>"
;      )
     (("例: Ruby in Scheme in Ruby" . "You can call Ruby function via Scheme") 
      "Rubyに埋め込まれたSchemeのコードに<pre><span class='ruby'>plus = Unbabel::Scheme(<<-EOD) <span class='scheme'>
  ;; plus :: Int -> Int -> Int
  (define (plus x y)
    ((unbabel-ruby \"
      # plus :: Int -> Int -> Int
      def plus(x, y)
        x + y
      end\"
    ) x y))</span>
EOD
plus[2, 3]</span>"
      )
     (("例: Ruby in Scheme in Ruby" . "You can call Ruby function via Scheme") 
      "Rubyのコードを埋め込む<pre><span class='ruby'>plus = Unbabel::Scheme(<<-EOD) <span class='scheme'>
  ;; plus :: Int -> Int -> Int
  (define (plus x y)
    ((unbabel-ruby \" <span class='ruby'>
      # plus :: Int -> Int -> Int
      def plus(x, y)
        x + y
      end</span>\"
    ) x y))</span>
EOD
plus[2, 3]</span>"
      )

;     (("Unbabel::Ruby" . "You can call Ruby function *directly*!")
;      "RubyスクリプトにRubyの関数を埋め込むことができます
;<pre>plus = Unbabel::Ruby.new(<<-EOD) <span class='ruby'>
;  # plus :: Int -> Int -> Int
;  def plus(x, y)
;    x + y
;  end</span>
;EOD
;plus[1, 2]  #=> 3")

     (("Ruby1.8にRuby1.9を埋め込む" . "You can use Ruby 1.9 in secret to your boss ;-)") 
       "<pre>fib = Babel::Ruby19.new(<<-EOD)<span class='ruby'>
  # fib :: Int -> Int
  def fib(n)
    case n
    when 0 ; ->(n){ 0 }
    when 1 ; ->(n){ 1 }
    else ->(n){ fib(n-2) + fib(n-1) }
    end.call(n)
  end</span>
EOD
fib[10].should == 55</pre>")
     (("Ruby1.8にRuby1.9を埋め込む" . "You can use Ruby 1.9 in secret to your boss ;-)") 
       "<pre>fib = Babel::Ruby19.new(<<-EOD)
  # fib :: Int -> Int
  def fib(n)
    case n
    when 0 ; <span class='ruby'>->(n){ 0 }</span>
    when 1 ; <span class='ruby'>->(n){ 1 }</span>
    else <span class='ruby'>->(n){ fib(n-2) + fib(n-1) }</span>
    end.call(n)
  end</span>
EOD
fib[10].should == 55</pre>")

     (("応用" . "You can jump the version gaps")
      "こっそり最新版の言語を使うことができます"
      "Java 4 に Java 5を？"
      "PHP 4 に PHP 6を？"
      "Perl 5 に Perl 6を？")

;     (("逆向きの応用" . "You can jump the version gaps")
;      "Ruby 1.8 in Ruby 1.9"
;      "これで後方互換性はばっちりですね"
;      )

;     ("今後の展望"
;      "JavaScript対応"
;      "Brainf*ck対応")
;     ("Ruby in JavaScript")

;; brainf*ck ----------------------------------------------------

     (("今後の展望" . "Brainf*ck support")
      "Brainf*ckを埋め込みたいです<pre><span class='ruby'>foo = Unbabel::Brainf_ck(&lt;&lt;EOD)</span>
  [>++++++++++++++-<]++++++>-+++<[++++
  ++++++++>[-+++++<]+++[-++++] <span class='ruby'>
EOD
foo[\"abc\"]</span></pre>")
     (("今後の展望" . "Brainf*ck support")
      "Brainf*ckの弱点: 入出力が貧弱なこと"
      "Rubyなどと組み合わせる"
      "ロジック部分だけBrainf*ckで組むことができる"
      )
     (("今後の展望" . "Brainf*ck support")
      "Brainf*ckの弱点: 入出力が貧弱なこと"
      "Rubyなどと組み合わせる"
      "ロジック部分だけBrainf*ckで組むことができる"
      "<strong>Brainf*ckで業務アプリ？</strong>"
      )

;     (("謝辞" . "acknowledgement")
;      "インラインアセンブラのソースコードは<a href='http://codezine.jp/article/detail/393?p=1'>CodeZine</a>を参考にしました"
;      "Inline::Cのソースコードは<a href='http://search.cpan.org/~ingy/Inline-0.44/C/C.pod'>Inline::Cのページ</a>を参考にしました"
;      "「島根は鳥取の左です」は島根大学の野田先生の真似です")

     ("オレオレ言語"
      "あなたが言語製作者なら"
      "まずUnbabelプロトコルを実装すると"
      "CPANやrubygemsの全てのライブラリがあなたの言語から使えるようになります(^o^)/")

     (("ご清聴ありがとうございました" . " Thank you!")
      "好きな言語を好きなように組み合わせよう！"
;      "「Esoteric Language」応援よろしく！"
      "続きはWebで: <br>　<input value='Unbabel' style='font-size: 25px' size='14'><input type='button' value='検索' style='font-size: 25px'>"
       )
     ))


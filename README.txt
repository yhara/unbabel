= Unbabel - Embed snippet code to other language

Unbabel is a project that allows you to embed snipped code
to other language.

It's like Perl's Inline::C but many languages are supported
(Ruby, Scheme, Haskell, Python, etc) and you can call 
the functions written in these languages each other.

See SPEC.en for further information.

== Example: call Python function from Ruby script

  fib = Babel::Python.new((<<-EOD).unindent)
    # fib :: Int -> Int
    def fib(x):
      if x == 0:
        return 0
      elif x == 1:
        return 1
      else:
        return fib(x-2) + fib(x-1)
  EOD
  p fib[10]  #=> 55

= Unbabel - LLにLLを埋め込んでみた

UnbabelはLLのコードを別の言語に埋め込むというプロジェクトです。
まぁPerlのInline::Cみたいなもんですが、いろんな言語をサポート
していて(Ruby, Scheme, Haskell, Python などなど) 互いに関数を
呼び出しあうことができます。

詳細は SPEC.ja を参照してください。

== 連絡先

 yhara(at)kmc.gr.jp

let of_string pat =
  Reparser.expr Relexer.token (Lexing.from_string pat)

let pmatch pat string =
  Regular.re_accepts (of_string pat) string

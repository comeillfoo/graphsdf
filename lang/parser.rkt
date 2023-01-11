#lang brag

gsdf-program : statement*

statement : assignment

assignment : IDENTIFIER "=" expr

expr : [unary-op] ident-or-const
  | ident-or-const binary-op ident-or-const

ident-or-const : IDENTIFIER | CONSTANT

unary-op : "-"
  | "sqrt"

binary-op : "+"
  | "-"
  | "*"
  | "/"

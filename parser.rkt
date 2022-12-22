#lang brag

sdf-program : statement-or-module-definition*

statement-or-module-definition : statement | module-definition

module-definition : "module" IDENTIFIER "(" ports ")" ";"
  statement+
  "endmodule"

ports : output-port | input-port "," ports

input-port : "input" IDENTIFIER

output-port : "output" IDENTIFIER

statement : assignment | module-invocation

module-invocation : IDENTIFIER "(" ident-list ")"

ident-list : IDENTIFIER | IDENTIFIER "," ident-list

assignment : IDENTIFIER "=" expr

expr : [unary-op] ident-or-const
  | ident-or-const binary-op ident-or-const

ident-or-const : IDENTIFIER | CONSTANT

unary-op : "-"

binary-op : "+"
  | "-"
  | "*"
  | "/"

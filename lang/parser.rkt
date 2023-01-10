#lang brag

gsdf-program : statement*

;;; statement-or-module-definition : statement | module-definition

;;; module-definition : "module" IDENTIFIER "(" input-output-port ("," input-output-port)* ")" ";" assignment+ "endmodule"

;;; input-output-port : ( "output" | "input" ) IDENTIFIER

statement : assignment ;;; | module-invocation

;;; module-invocation : IDENTIFIER "(" IDENTIFIER ("," IDENTIFIER)* ")"

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

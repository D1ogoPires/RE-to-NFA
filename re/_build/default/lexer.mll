{ open Parser }

rule token = parse
| ['a'-'z'] as c { CHAR c }
| '_' { EMPTY }
| '*' { STAR }
| '|' { CHOICE }
| '(' { LPAR }
| ')' { RPAR }
| eof { EOF }

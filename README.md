# matf

## Supported Grammar

```
<expr> ::=
| <literal> <expr'>
| \frac{<expr>}{<expr>}
| <expr'> <pow>
| \sum <sub> <pow> <expr>
| \sum <pow> <expr>
| \sum <expr>
| \prod <sub> <pow> <expr>
| \prod <pow> <expr>
| \prod <expr>
| <literal>

<expr'> ::=
| <op> <expr'>
| <literal>

<op> ::=
| +
| -
| \cdot
| /

<pow> ::= ^{<expr>}
<sub> ::= _{<literal>}

<literal> ::=
| <literal'> <sub>
| <literal'>

<literal'> ::=
| [a-zA-Z]+
| [0-9](.[0-9]+)?
```

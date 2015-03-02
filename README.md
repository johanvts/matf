# matf

Supported Grammar:

E :=
| E OP E
| \frac{E}{E}
| E POW
| \sum SUB{0,1} POW E
| \prod SUB{0,1} POW E
| LITERAL

OP :=
| +
| -
| \cdot
| /

POW := ^{E}
SUB := _{LITERAL}

LITERAL :=
| LITERAL SUB
| [a-zA-Z]+
| [0-9][.[0-9]+]{0,1}

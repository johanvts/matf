# matf

## Supported Grammar

```
E :=
| E OP E
| \\frac\{E\}\{E\}
| E POW
| \\sum SUB? POW E
| \\prod SUB? POW E
| LITERAL

OP :=
| +
| -
| \\cdot
| /

POW := ^\{E\}
SUB := _\{LITERAL\}

LITERAL :=
| LITERAL SUB
| [a-zA-Z]+
| [0-9](.[0-9]+)?
```

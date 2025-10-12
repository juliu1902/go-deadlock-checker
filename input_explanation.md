# Big Test

## ST, normalisation and context  calculation, equivalency

### Paar 1

```
var c1 chan int
var c2 chan int
c1 <- 1
c2 <- 2
close c1
close c2
---
var c1 chan int
var c2 chan int
c2 <- 2
c1 <- 1
close c1
close c2
```

```
ST A:
c1!;c2!;c1#;c2#

ST B:
c2!;c1!;c1#;c2#
```

```
Context: 
"c1" -> (TChan CInt, AChan "id0")
"c2" -> (TChan CInt, AChan "id0")
```

```
A ~ B : FALSE
DUAL : FALSE
```

### Paar 2

```
var c chan int
if b then { c <- 1 } else { c <- 2 }
c <- 3
close c
---
var c chan int
if b then { 
    c <- 1
    c <- 3 
} else { 
    c <- 2
    c <- 3 
}
close c
```

```
ST A:
c! if b else c!;c!;c#

ST B:
{c!;c!} if b else {c!;c!};c#
```

```
Context: 
"c" -> (TChan CInt, AChan "id0")

Normalized:
ST A: c! if b else c!;c!;c# 
cond-eta
c!;c!;c#

ST B: {c!;c!} if b else {c!;c!};c#
cond-eta
c!;c!;c#
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 3

```
var c chan int
c <- 0
if b then { c <- 1 } else { c <- 2 }
close c
---
var c chan int
if b then { 
    c <- 0
    c <- 1 
} else { 
    c <- 0 
    c <- 2 
}
close c
```

```
ST A:
c!;c! if b else c!;c#

ST B:
{c!;c!} if b else {c!;c!};c#
```

```
Context: 
"c" -> (TChan CInt, AChan "id0")

Normalize:
ST A: c!;c! if b else c!;c#
cond-eta
c!;c!;c#

ST B: {c!;c!} if b else {c!;c!};c#
cond-eta
c!;c!;c#
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 4

```
var c1 chan int
var c2 chan int
var c chan int
c := c1
c <- 1
c := c2
c <- 2
close c1
close c2
---
var c1 chan int
var c2 chan int
x = <- c1
y = <- c2
close c1
close c2
```

```
ST A:
c1!;c2!;c1#;c2#

ST B:
c1?;c2?;c1#;c2#
```

```
Context: 
ST A:
"c" -> (TChan CInt, AChan "id0")
"c1" -> (TChan CInt, AChan "id1")
"c2" -> (TChan CInt, AChan "id2")
NACH c := c1
"c1" -> (TChan CInt, AChan "id1")
"c2" -> (TChan CInt, AChan "id2")
"c" -> (TChan CInt, ATerm (EVar "c1"))
NACH c := c2
"c1" -> (TChan CInt, AChan "id1")
"c2" -> (TChan CInt, AChan "id2")
"c" -> (TChan CInt, ATerm (EVar "c2"))

ST B:
"c1" -> (TChan CInt, AChan "id0")
"c2" -> (TChan CInt, AChan "id1")

Normalized:
ST A: c1!;c2;c1#;c2#
ST B: c1?;c2?;c1#;c2#
```

```
A ~ B : FALSE
DUAL :TRUE
```

### Paar 5

```
var c chan int
c <- 1
close c
c <- 2
---
var c chan int
c <- 1
c <- 2
close c
---
```

```
ST A:
c!;c#;c!

ST B:
c!;c!;c#
```

```
A ~ B : FALSE
DUAL : FALSE
```

### Paar 6


```
var c chan int
if b then { 
    c <- 1
    c <- 2 
} else { c <- 2 }
close c
---
var c chan int
if b then { 
    c <- 1
    c <- 2 
} else { 
    c <- 1
    c <- 2 }
close c
```

```
ST A:
{c!;c!} if b else c!;c#

ST B:
{c!;c!} if b else {c!;c!};c#
```

```
Normalized:
ST A: {c!;c!;c#} if b else {c!;c#}
ST B: c!;c!;c#
```

```
A ~ B : FALSE
DUAL : FALSE
```

### Paar 7

```
var c chan int
x = <- c
if (x > 0) then { c <- 1 } else { c <- 2 }
close c
---
var c chan int
x = <- c
if (x <= 0) then { c <- 2 } else { c <- 1 }
close c
---
```

```
ST A:
c?;c! if x>0 else c!;c#

ST B:
c?;c! if x<=0 else c!;c#
```

```
Normalized:
ST A: c?;c!;c#
ST B: c?;c!;c#
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 8

```
var c chan int
if b then { if b then c <- 1 else c <- 1 } else { c <- 2 }
close c
---
var c chan int
if b then { c <- 1 } else { c <- 2 }
close c
---
```

```
ST A:
{c! if b else c!} if b else c!;c#

ST B:
c! if b else c!;c#
```

```
Normalized:
ST A: {c! if b else c!} if b else c!;c#
cond-dist
{c! if b else c!;c#} if b else {c!;c#}
cond-dist
{{c!;c#} if b else {c!;c#}} if b else {c!;c#}
cond-eta
{c!;c#} if b else {c!;c#}
cond-eta
c!;c#

ST B: c!;c#
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 9

```
var c chan int
var y bool
y := (x > 0)
if y then { c <- 1 } else { c <- 2 }
close c
---
var c chan int
if (x > 0) then { c <- 1 } else { c <- 2 }
close c
---
// Erwartung: A ~ B = True (Substitution bei ST-Erzeugung greift)
```

```
ST A:
c! if x>0 else c!;c#

ST B:
c! if x>0 else c!;c#

Normalized: c!;c#
```

```
Context: 
ST A:
"c" -> (TChan CInt, AChan "id0")
"y" -> (TChan CInt, ATerm (EBinOp Gt (EVar "x") (EInt 0)))

ST B:
"c" -> (TChan CInt, AChan "id0")
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 10

```
var c1 chan int
var c2 chan int
if b then { 
    c1 <- 1
    c2 <- 1 
} else { 
    c1 <- 2
    c2 <- 2 
}
close c1
close c2
---
var c1 chan int
var c2 chan int
x = <- c1
y = <- c2
close c1
close c2
---
// Erwartung: dual(A) ~ B = True
```

```
ST A: {c1!;c2!} if b else {c1!;c2!};c1#;c2#

ST B: c1?;c2?;c1#;c2#
```

```
Context: 
ST A:
"c1" -> (TChan CInt, AChan "id0")
"c2" -> (TChan CInt, AChan "id1")

ST B:
"c1" -> (TChan CInt, AChan "id0")
"c2" -> (TChan CInt, AChan "id1")

Normalized:
ST A: c1!;c2!;c1#;c2#
ST B: c1?;c2?;c1#;c2#
```

```
A ~ B : FALSE
DUAL : TRUE
```

### Paar 11

```
var c1 chan int
var c2 chan int
c1 <- 1
c2 <- 2
c1 <- 3
close c1
close c2
---
var c1 chan int
var c2 chan int
c2 <- 2
c1 <- 1
c1 <- 3
close c1
close c2
---
// Erwartung: A ~ B = False
```

```
ST A: c1!;c2!;c1!;c1#;c2#

ST B: c2!;c1!;c1;c1#;c2#

```

```
Normalized:
ST A: c1!;c2!;c1!;c1#;c2#
ST B: c2!;c1!;c1;c1#;c2#
```

```
A ~ B : FALSE
DUAL : FALSE 
```

### Paar 12

```
var c chan int
if b then { c <- 1 } else { c <- 1 }
c <- 2
close c
---
var c chan int
c <- 1
c <- 2
close c
---
// Erwartung: A ~ B = True (cond-eta + assoc)
```

```
ST A:
c! if b else c!;c!;c#

ST B:
c!;c!;c#
```

```
Normalized:
ST A: c!;c!;c#
ST B: c!;c!;c#
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 13

```
var c chan int
if b then { 
    c <- 1
    if b then { c <- 3 } else { c <- 3 } 
} else { 
    c <- 2
    c <- 3 
}
close c
---
var c chan int
if b then { c <- 1 } else { c <- 2 }
c <- 3
close c
---
```

```
ST A:
{c!;c! if b else c!} if b else {c!;c!};c#

ST B:
c! if b else c!;c!;c#
```

```
Normalized:
ST A: {c!;c! if b else c!} if b else {c!;c!};c#
cond-eta
{c!;c!} if b else {c!;c!};c#
cond-eta
c!;c!;c#
ST B: 
c!;c!;c#
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 14

```
var c1 chan int
var c chan int
c := c1
c <- 1
c <- 2
close c1
---
var c1 chan int
c1 <- 1
c1 <- 2
close c1
---
// Erwartung: A ~ B = True
```

```
ST A:
c1!;c1!;c#

ST B:
c1!;c1!;c#
```

```
Context: 
ST A:
"c" -> (TChan CInt, AChan "id0")
"c1" -> (TChan CInt, AChan "id1")
am Ende:
"c" -> (TChan CInt, ATerm EVar "c1")
"c1" -> (TChan CInt, AChan "id1")
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 15

```
var c1 chan int
var c2 chan int
if b then { 
    c1 <- 1
    close c1
    c2 <- 2
    close c2 
} else { 
    c1 <- 1
    c2 <- 2
    skip
    close c2
    close c1 
}
---
var c1 chan int
var c2 chan int
c1 <- 1
c2 <- 2
close c1
close c2
---
```

```
ST A:
{c1!;c1#;c2!;c2#} if b else {c1!;c2!;skip;c2#;c1#}

ST B:
c1!;c2!;c2#;c1#
```

```
Normalized:
ST A: {c1!;c1#;c2!;c2#} if b else {c1!;c2!;skip;c2#;c1#}

ST B: c1!;c2!;c2#;c1#
```

```
A ~ B : FALSE
DUAL : FALSE
```

Richtig?

### Paar 16

```
var c chan int
x = <- c
if (x > 0) then { c <- 1 } else { c <- 2 }
if (x > 0) then { c <- 3 } else { c <- 4 }
close c
---
var c chan int
x = <- c
if (x > 0) then { 
    c <- 1
    c <- 3 
} else { 
    c <- 2
    c <- 4 
}
close c
---
```

```
ST A:
c?;c! if x>0 else c!;c! if x>0 else c!;c#

ST B:
c?; {c!;c!} if x>0 else {c!;c!};c#
```

```
Normalized:
ST A: c?;c!;c!;c#
ST B: c?;c!;c!;cä
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 17

```
var c chan int
for (i=0;i<0;i++) { c <- i }
close c
---
var c chan int
close c
---
```

```
ST A:
c#

ST B:
c#
```

```
ST B: c1?;c2?;c1#;c2#
```

```
A ~ B : TRUE
DUAL : TRUE
```

### Paar 18

```
var c chan int
if b then { 
    c <- 1
    c <- 2 
} else { 
    c <- 3
    c <- 4
    c <- 5 
}
close c
---
var c chan int
if b then { 
    c <- 1
    c <- 2 
} else { 
    c <- 3
    c <- 4 
}
close c
---
```

```
ST A:
{c!;c!} if b else {c!;c!;c!};c#

ST B:
{c!;c!} if b else {c!;c!};c#
```

```
Normalized:
ST A: {c!;c!;c#} if b else {c!;c!;c!;c#}
ST B: c!;c!;c#
```

```
A ~ B : FALSE
DUAL : FALSE
```

### Paar 19

```
var c chan int
x = <- c
if (x == 0) then { 
    c <- 1 
} else { 
    c <- 1
    c <- 2 
}
close c
---
var c chan int
x = <- c
if (x == 0) then { 
    c <- 1
    c <- 2 
} else { 
    c <- 1 
}
close c
---
```

```
ST A:
c?;c! if (x==0) else {c!;c!};c#

ST B:
c?;{c!;c!} if (x==0) else c!;c#
```

```
Normalized:
ST A: c?;{c!;c#} if (x==0) else {c!;c!;c#}
ST B: c?;{c!;c!;c#} if (x==0) else {c!;c#}
```

```
A ~ B : FALSE
DUAL : FALSE
```

### Paar 20

```
var c chan int
if (b && (1 - b)) then { c <- 1 } else { c <- 2 }
close c
---
var c chan int
c <- 2
close c
---
```

```
ST A:
c! if b&&1-b else c!;c#

ST B:
c!;c#
```

```
Normalized:
ST A: c!;c#
ST B: c!:c#
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 21

```
var c1 chan int
var c2 chan int
c1 <- 1
close c2
c2 <- 2
close c1
---
var c1 chan int
var c2 chan int
c1 <- 1
c2 <- 2
close c1
close c2
---
```

```
ST A:
c1!;c2#;c2!;c1#

ST B:
c1!;c2!;c1#;c2#
```

```
A ~ B : FALSE
DUAL : FALSE
```

### Paar 22

```
var c chan int
x = <- c
y = <- c
if b then { c <- x } else { skip }
close c
---
var c chan int
var x int
c <- 0
c <- 0
if b then { x := 1 } else { skip }
close c
```

```
ST A:
c?;c?;c! if b else skip;c#

ST B:
c!;c!;c#
```

```
Normalized: 
ST A: c?;c?;{c!;c#} if b else c#
ST B: c!;c!;c#
```

```
A ~ B : FALSE
DUAL : FALSE
```

### Paar 23

```
var c chan int
var x int
var y int
if b then { x := 1 } else { y := 1 }
c <- x
close c
---
var c chan int
var x int
var y int
var z int
if b then { x := y } else { x := z }
c <- x
close c
```

```
ST A:
c!;c#

ST B:
c!;c#
```

```
Context A in the end: 
"c" -> (TChan CInt, AChan "id0")
"x" -> (TInt, AIf b (ATerm 1) (AUnknown))
"y" -> (TInt, AIf b AUnknown (ATerm 1))

Context B in the end:
"c" -> (TChan CInt, AChan "id0")
"x" -> (TInt, AIf b (ATerm y) (ATerm z)
"y" -> (TInt, AUnknown)
"z" -> (TInt, AUnknown)
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 24

```
var c1 chan int
var c2 chan int
var c chan int
if b then { c := c1 } else { c := c2 }
c <- 1
close c1
close c2
---
var c1 chan int
var c2 chan int
if b then { c1 <- 1 } else { c2 <- 1 }
close c1
close c2
```

```
ST A:
c1! if b else c2!;c1#;c2#

ST B:
c1! if b else c2!;c1#;c2#
```

```
Context A in the end: 
"c" -> (TChan CInt, AIf b (ATerm c1) (ATerm c2))
"c1" -> (TChan CInt, AChan ChannelID "id0")
"c2" -> (TChan CInt, AChan ChannelID "id1")

Context B in the end:
"c1" -> (TChan CInt, AChan ChannelID "id0")
"c2" -> (TChan CInt, AChan ChannelID "id1")
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 25

```
var c1 chan int
var c2 chan int
var c chan int
if b then { c = c1 } else { c = c2 }
close c
---
var c1 chan int
var c2 chan int
var c chan int
if b then { c = c1 } else { c = c2 }
if b then { close c1 } else { close c2 }
```

```
ST A:
if b then c1# else c2#

ST B:
if b then c1# else c2#
```

```
A ~ B : TRUE
DUAL : TRUE
```

### Paar 26

```
var c1 chan int
var c2 chan int
var x int
if x>0 then { c1 <- x } else { c2 <- -x }
close c1
close c2
---
var x int
var c1 chan int
var c2 chan int
c1 ::= make (chan int)
c2 ::= make (chan int)
if x>0 then { c1 <- x } else { c2 <- -x }
close c1
close c2
```

```
ST A:
c1! if x>0 else c2!;c1#;c2#

ST B:
new c1.new c2.c1! if x>0 else c2!;c1#;c2#
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 27

```
var x chan int
var y chan int
var c1 chan int
var c2 chan int
x := <- c1
y := <- c2
if x>0 then { 
    c1 <- x
    c2 <- x 
} else if x<0 then { 
    c1 <- 3*x
    c2 <- 3*x 
} else { skip }
close c1
close c2
---
var x chan int
var y chan int
var d1 chan int
var d2 chan int
x := <- d1
y := <- d2
if x>0 then { 
    d1 <- x
    d2 <- x 
} else if x<0 then { 
    d1 <- 3*x
    d2 <- 3*x } 
else { skip }
close d1
close d2
```

```
ST A:
c1?;c2?;{c1!;c2!} if x>0 else {{c1!;c2!} if x<0 else skip};c1#;c2#

ST B:
d1?;d2?;{d1!;d2!} if x>0 else {{d1!;d2!} if x<0 else skip};d1#;d2#
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 28

```
var c chan int
c2 ::= make (chan int)
c := c2 
c <- 1
close c
---
c2 ::= make (chan int)
c2 <- 1
close c2
```

```
ST A:
c2!;c2#

ST B:
c2!;c2#
```

```
A ~ B : TRUE
DUAL : FALSE
```

### Paar 29

```
var c1 chan int
var c2 chan int
var c chan int
c := c1
c1 := c2
c <- 2
close c
---
var c1 chan int
var c2 chan int
c2 <- 2
close c2
```

```
ST A:
c2!;c2#

ST B:
c2!;c2#
```

```
A ~ B : TRUE
DUAL : FALSE
```

# TODO

```
FRAGEN:
Type Matching testen, also:
var x int
var c chan bool
x := c
c := x
müsste Fehler aufwerfen? Assignments nur unter gleichen Typen

c!;c#;c! wird geparsed und auf äquivalenz geprüft aber sollte es einen Fehler aufwerfen?

Anmerkung: zwei Statements sind äquivalent, auch wenn ihre conds in ihren if-statements unterschiedlich sind
```


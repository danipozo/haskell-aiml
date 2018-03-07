# haskell-aiml

Un pequeño DSL para aliviar el dolor de escribir AIML, en Haskell.

Ejemplo:

```haskell
cat [
        pat  ["Hola, soy *"],
        temp ["Hola, ", star [index_ 1], " que tal"]
    ]

```

Salida:

```xml
<category >
  <pattern >
        Hola, soy *
  <pattern/>
  <template >
        Hola, <star index="1" /> que tal
  <template/>
<category/>

```

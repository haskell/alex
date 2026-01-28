# wyvern - sample lexers

Below are three sample alex files illustrating how to use various alex wrappers and the differences between them:

* `basic`
* `posn`
* `monad`

All examples are taken from [wyvern-diagrams](https://hackage.haskell.org/package/wyvern-diagrams).

All lexers return a list of tokens (plus some additional information like token position in example 2 and 3) so if the provided input is correct, the tokens can be used directly in parsers.

## input

### correct

[source](./correct-input.txt)

```
"action"
"question"
{
    "action"
}
{
    "action"
}
"action"
```

### incorrect

[source](./incorrect-input.txt)

```
"action"
"question"
{
    "action"|
}
{
    "action"
}
"action"
```

## `basic` - lexer v1

[source](./LexerV1.x)

### features

* easy to implement
* extracts tokens as advertised
* very useful for your first lexer
* no information about token/error positions
* errors out on incorrect input

### usage

```haskell
-- ...
let tokens = alexScanTokens fileContent
-- ...
```

### output

* tokens extracted correctly:

  ```
  [TokenAction "# \"action\"",TokenAction "# \"question\"",TokenOCB,TokenAction "# \"action\"",TokenCCB,TokenOCB,TokenAction "# \"action\"",TokenCCB,TokenAction "# \"action\""]
  ```

* errors:

  ```
  wyvern-diagrams: lexical error
  CallStack (from HasCallStack):
    error, called at src/lib/Lexer.hs:493:32 in wyvern-diagrams-0.3.1.3-inplace:Lexer
  ```

## `posn` - lexer v2

[source](./LexerV2.x)

### features

* only slightly more difficult to implement compared to the basic wrapper 
* extracts tokens as advertised but also reveals more information about:
  * token positions
  * error positions
* errors out on incorrect input

### usage

```haskell
-- ...
let tokens = alexScanTokens fileContent
-- ...
```

### output

* tokens extracted correctly:

  ```
  [TokenAction (AlexPn 0 1 1) "# \"action\"",TokenAction (AlexPn 9 2 1) "# \"question\"",TokenOCB (AlexPn 20 3 1),TokenAction (AlexPn 26 4 5) "# \"action\"",TokenCCB (AlexPn 35 5 1),TokenOCB (AlexPn 37 6 1),TokenAction (AlexPn 43 7 5) "# \"action\"",TokenCCB (AlexPn 52 8 1),TokenAction (AlexPn 54 9 1) "# \"action\""]
  ```

* errors:

  ```
  wyvern-diagrams: lexical error at line 4, column 13
  CallStack (from HasCallStack):
    error, called at src/lib/LexerV2.hs:573:61 in wyvern-diagrams-0.3.1.3-inplace:LexerV2
  ```

## `monad` - lexer v3

[source](./LexerV3.x)

### features

* only slightly more difficult to implement compared to the posn wrapper but has a different interface (`alexMonadScan`, `runAlex`)
* just like the posn wrapper, it extracts tokens as advertised but also reveals more information about:
  * token positions
  * error positions
* does not error out on incorrect input but instead returns the result as `Either`:
  * string error
  * list of tokens

### usage

```haskell
-- ...
let lexingResult = runAlex fileContent lexAll
-- ...
```

### output

* tokens extracted correctly:

  ```
  Right [TokenAction (AlexPn 0 1 1) "# \"action\"",TokenAction (AlexPn 9 2 1) "# \"question\"",TokenOCB (AlexPn 20 3 1),TokenAction (AlexPn 26 4 5) "# \"action\"",TokenCCB (AlexPn 35 5 1),TokenOCB (AlexPn 37 6 1),TokenAction (AlexPn 43 7 5) "# \"action\"",TokenCCB (AlexPn 52 8 1),TokenAction (AlexPn 54 9 1) "# \"action\""]
  ```

* errors:

  ```
  Left "lexical error at line 4, column 13"
  ```

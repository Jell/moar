# moar
[![Build Status](https://travis-ci.org/Jell/moar.svg?branch=master)](https://travis-ci.org/Jell/moar)

> MOOOOAR!!1!
>
> -- <cite>The Internet</cite>

A monad library made for Clojure. No macros needed.

## Usage

Quick example:

```lisp
(ns moar.example
    (:require [moar.core :refer :all]
              [moar.protocols :refer :all]
              [moar.monads.sequence :as sequence]
              [moar.monads.maybe :as maybe :refer [just nothing]]
              [moar.monads.maybe-t :refer [maybe-t]]))

(wrap maybe/monad :tobias)
;;=> #<Just@74de792d: :tobias>

(bind (just 2) #(wrap maybe/monad (inc %)))
;;=> #<Just@1e8c0586: 3>

@(just :x)
;;=> :x

(>>= (wrap maybe/monad 2)
     (fn [x] (just (inc x)))
     (fn [x] nothing)
     (fn [x] (just (inc x))))
;;=> #<Nothing moar.monads.maybe.Nothing@72ca7ea3>

(fmap inc (just 2))
;;=> #<Just@590d3235: 3>

(= (just 2) (just 2))
;;=> true

(= (just 2) (just 3))
;;=> false

(let [return (partial wrap maybe/monad)]
  (>>= (return 1)
       (lift-f maybe/monad inc)
       (lift-f maybe/monad inc)))
;;=> #<Just@241c11b4: 3>

;; Also: Monad transformers!
(let [monad (maybe-t sequence/monad)
      return (partial wrap monad)]
  (>>= (return 1)
       (lift-f monad inc)
       (lift-f monad
               (lift-f sequence/monad inc))))
;;=> #<Transformer@2ec291ff: (#<Just@33053b97: 3>)>

;; Lifting monadic values
(let [monad (maybe-t sequence/monad)
      return (partial wrap monad)]
  (>>= (lift monad (list 1))
       (lift-f monad inc)))
;;=> #<Transformer@10bbf34: (#<Just@66bd3ffd: 2>)>

(let [monad (maybe-t sequence/monad)
      return (partial wrap monad)]
  ((lift-m monad +) (return 1) (return 2) (return 3)))
;;=> #<Transformer@4583ce1e: (#<Just@2e21d86c: 6>)>

(let [monad (maybe-t sequence/monad)
      return (partial wrap monad)]
  ((lift-m monad list) (return 1) (return 2) (return 3)))
;;=> #<Transformer@44478429: (#<Just@10f7c67e: 1> #<Just@2bba512: 2> #<Just@2449f0b9: 3>)>
```

## Design Goals

- No magic
- Easily extensible
- Intelligent error messages
- Support all major monads
- Support for major monad transformers
- Adapt implementation to the dynamic nature of Clojure

## Naming conventions

- `val`: values (a)
- `fun`: functions (a -> b)
- `m-val`: monadic values (MonadInstance m => m a)
- `m-fun`: monadic functions (a -> m a)
- `m-impl`: implementation of a monad (Monad, MonadPlus)
- `t-impl`: implementation of a monad transformer

## Notes

- a monadic value like `(just 4)` implements the `MonadInstance`
  protocol, which points to a monad implementation (`maybe/monad` in
  that case) that implements the `Monad` (and potentially the
  `MonadPlus`) protocol.

- `return` in haskell becomes `wrap monad` in `moar`. Since we can't
  dispatch on the return type at compile time in Clojure, we do an
  explicit dispatch on the monad implementation at runtime.

  We chose `wrap` instead of `return` to avoid confusion to newcomers.

- `bind`, `>>=`, `fmap` and `>>` behave like their haskell
  counterpart, except that they dispatch the monad implementation at
  runtime based on their monadic argument.

## Latest version

[![Clojars Project](http://clojars.org/moar/latest-version.svg)](http://clojars.org/moar)

## Future work

- Cleanup monad transformers (a bit rough right now)
- Better pretty printing
- `lift` (not yet drafted)
- More monad and monad transformers implementations

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

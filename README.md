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
              [moar.monads.maybe :as maybe :refer [just nothing]]))

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
```

## Design Goals

- No magic
- Easily extensible
- Support all major monads
- Support for major monad transformers
- Adapt implementation to the dynamic nature of Clojure

## Latest version

[![Clojars Project](http://clojars.org/moar/latest-version.svg)](http://clojars.org/moar)

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

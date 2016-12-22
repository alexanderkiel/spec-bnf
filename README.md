# Spec BNF

This library can generate BNF's from Clojure spec.

## Install

To install, just add the following to your project dependencies:

```clojure
[org.clojars.akiel/spec-bnf "0.1-SNAPSHOT"]
```

## Usage

```clojure
(use 'spec-bnf.core)

(println (emit-grammar (apply grammar (spec-kws "<your-ns>"))))
```

## License

Copyright © 2016 Alexander Kiel

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

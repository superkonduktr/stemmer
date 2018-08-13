# Stemmer

[![Build Status](https://travis-ci.org/superkonduktr/stemmer.svg?branch=master)](https://travis-ci.org/superkonduktr/stemmer)

An OCaml implementation of the renowned [Porter stemming algorithm](http://snowball.tartarus.org/algorithms/porter/stemmer.html).

### Usage

```ocaml
utop[1]> Stemmer.stem "perfidiousness";;
- : string = "perfidi"
```

### Testing

The [test](https://github.com/superkonduktr/stemmer/blob/master/test/stemmer_test.ml) is run against the provided [sample vocabulary](https://tartarus.org/martin/PorterStemmer/voc.txt)
with the [corresponding output](https://tartarus.org/martin/PorterStemmer/output.txt).

```
$ make test
```

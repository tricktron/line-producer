# Line-Producer Investigations
This repository is the code playground for the accompanying article *Investigations About Multi-Parameter Type Classes for the Possible Use in General Type Safe Data Processing in Frege*.

### Starting position
The starting point builds the [LineProducer.fr](/frege/src/main/LineProducer.fr) file written by Dierk König. His `LineProducer` type class allows only monomorphic `String` line producer-consumer pair. The goal is to generalise this limitation to arbitrary line producer-consumer types.

### Approach

The basis of the article is the free monad transformer data type `Coroutine` introduced by Blazevic [in his paper](http://www.berniepope.id.au/assets/files/haskell-mpi.monad.reader.pdf#page=29).

I experimented with this `Coroutine` data type first in Haskell. See the files in the [/src](/src) directory.

### Solution
In a second step, I then transformed my Haskell code to Frege. See the files in the [/frege/src/main](/frege/src/main) directory.

Finally, I reproduced the initial functionality in the [LineProducer.fr](/frege/src/main/LineProducer.fr) with the added ability to support polymorphic line-producer consumer pairs in [CoroutineLine.fr](/frege/src/main/CoroutineLine.fr). All the examples in the article are taken from this file.

### Mini Steps with Quickcheck

In the file [CoroutineLineTest.fr](/frege/src/test/CoroutineLineTest.fr), I played around with Quickcheck to show a way on how to transform the examples in the long `main` function from [CoroutineLine.fr](/frege/src/main/CoroutineLine.fr). This is far from complete but it already inlcudes an interesting property `p_producerIsInvariantInLineCountAndLineContent`.

### How to Run the Code

### Haskell

#### Manual
1. Install `ghc`. This is tested with version `8.8.4`.
2. Install `cabal`. This is tested with version `3.2.0.0`.
3. Optional: Make a sandbox for cabal with `cabal sandbox`
4. Clone this repo.
5. Run `cabal install` in the root directory.
6. `cabal repl`: Now you can play around with it in the repl. E.g. `LineProducer.main` should print the numbers 0-9 on to your screen.

#### The cool `nix` way
1. Install `nix` package manager with `curl -L https://nixos.org/nix/install | sh`
2. Clone this repo.
3. run `nix-shell` in the root directory. This single command guarantees to give you the exact same reproducible environment as I have. Same ghc version, same cabal. Nothing to worry about. My machine becomes your machine. But it might take some time to fetch all dependencies. Maybe grab a coffee. If `ghc` needs to be built it may even take 30 min+. Definitely grab a coffee.
4. `cabal repl`: Now you can play around with it in the repl. E.g. `LineProducer.main` should print the numbers 0-9 on to your screen.


### Frege

#### The only way
1. Clone the repo.
2. Follow [Using Frege in Intellij IDEA](https://github.com/Frege/frege/wiki/Using-Frege-in-Intellij-IDEA).
3. `frege run CoroutineLine.fr` to see the examples from the article.
4. `frege test` to see the quickCheck tests pass. Including the interesting `p_producerIsInvariantInLineCountAndLineContent` property.








# PRNG

## Context
Most real runs of this app make millions of calls to the rng which makes it an integral part of the program, with a large effect on performance. There are two common interfaces for using a prng:
    - pure: generally has a function type similar to `rand a :: forall a. Seed -> Tuple Seed a`
    - impure: generally has a function type similar to `rand a :: forall a. Effect a`

## Decision
Use PureScript's `Effect.Random.random` which wraps JavaScript's `Math.random()` only within the `PartyCarlo.MonteCarlo` module. This makes the entry function into parallel sampling return `Aff (SortedArray Int)` which can be easily lifted into the production monad with `liftAff`.
    - Pros:
        - This rng is roughly 2 times faster than `Random.PseudoRandom.randomR` in single threaded execution.
        - Parallel implementation is encapsulated to the `PartyCarlo.MonteCarlo` module which makes test monads simpler than if `PartyCarlo.MonteCarlo` functions used the `Control.Parallel` constraint.
    - Cons:
        - JavaScript's `Math.random()` has different implementations for each JS version and runtime so results could vary widely based on the browser. Since cryptographic properties are not necessary I suspect this will have a negligible impact for most users.
        - All tests for functions that use the rng are necessarily non-deterministic which is not true for rngs like `Random.PseudoRandom.randomR` which require a seed.

## Alternatives
- Use PureScript's `Effect.Random.random` but use mtl-style constraints (e.g. - `rand :: Random m => m Probability`, or `rand :: MonadEffect m => m Probability`).
    - Pros:
        - Instances for test monads can be made deterministic
    - Cons:
        - Adding parallel execution with the `Control.Parallel` constraint makes all production and test monads significantly more complex.
- Use `Random.PseudoRandom.randomR`.
    - Pros:
        - All property and unit tests for random code become pure and deterministic.
        - Tests can run in the `State` monad instead of `Effect`. Because PS quickcheck lacks an equivelant to Haskell's `monadicIO`, we can remove all instances of `unsafePerformEffect`.
    - Cons:
        - It makes the app run 2 times as slow as the `Effect.Random.random` implementation.
        - `randomR` isn't splittable so sampling isn't easily parallizable.
        - The library has not been actively mantained since October 2019.

- Wrapping `Crypto.getRandomValues()` from JS
    - Note: I never implemented this so I don't know what the performance implications of this library are.
    - Pros:
        - More consistent implementations across JS versions and browsers
    - Cons:
        - Similar to JavaScript's `Math.random()` it's also impure so it would need to be wrapped in the `Effect` monad
        - I would need to generate all random numbers before running any experiments which is a bigger code change.

## Status
The app has been implemented with the decided prng and parallel sampling.

## Consequences
- Some tests are not non-deterministic and may be removed in the future.
- The app runs 20x faster than the previous implementation.

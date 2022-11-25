# party-carlo
It's easy to assign the likelihood you think one person will attend a party, but it's harder to reason about a collection of probabilities to represent a group. This client-side web application will use your individual probabilites with Monte Carlo methods to compose them into a distribution that can be used to reason about the group as a whole with confidence intervals.

## Local Dev
```
spago bundle-app
```
then open `./index.html` in a browser

## Future Improvements
- Better prng. Most real runs of this app make _millions_ of calls to the rng which makes it an integral part of the program. Since it's not a good idea to write my own, I only see three reasonable options in the PS and JS ecosystems:
    - Effect.Random.random :: Effect Random (current implementation)
        - Pros: It's significantly faster than the other two
        - Cons: It uses Math.random() which is implemented radically differently across JS versions and browsers. It's impure so some tests become non-deterministic.
    - Random.PseudoRandom.randomR (a previous implementation)
        - Pros: It's pure so tests can run in the State monad instead of Effect which is awesome expecially because PS quickcheck lacks an equivelant to Haskell's `monadicIO`.
        - Cons: It makes the app run at half speed which is a travesty. It's also not actively maintained anymore.
    - Wrapping Crypto.getRandomValues() from JS (never implemented)
        - Note: I have no idea what the performance implications of this library are.
        - Pros: More consistent implementations across JS versions and browsers
        - Cons: Impure so it would need to be wrapped in the Effect monad, would need to generate all random numbers before running any experiments which is a bigger code change.
- Async and multithreading for Monte Carlo experiments. Right now everything is single threaded through the Effect monad.
- Better cross-device screen sizing. Right now phones smaller than mine have to scroll around to see the full app.

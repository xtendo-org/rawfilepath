# rawfilepath

A Haskell library for the mid-level system functions for the `RawFilePath` data type.

## Background

Traditional `String` is notorious:

- Up to 40 bytes (five words) required for one character (the List constructor, the pointer to the Char constructor, the Char constructor, the actual Char value, and the pointer to the next List constructor)
- Heap fragmentation causing malloc/free overhead
- A lot of pointer chasing for reading, devastating the cache hit rate
- A lot of pointer chasing plus a lot of heap object allocation for manipulation (appending, slicing, etc.)
- Completely unnecessary but mandatory conversions and memory allocation when the data is sent to or received from the outside world

Transition to `Text` and `ByteString` began, but even after a dazzling community effort, `FilePath`, a key data type for programming anything useful, remained to be a type synonym of `String`.

To put a cherry on top of creaking, fuming, dragging, and littering pointers all over the heap space, `String` had another fantastic nature to serve as a file path data type: Encoding blindness. All functions that return `FilePath` would actually take a series of bytes returned by a syscall and somehow magically "decode" it into a `String` which is surprising because no encoding information was given. Of course there is no magic and it's an abject fail. `FilePath` just wouldn't work.

In June 2015, three bright Haskell programmers came up with an elegant solution called the [Abstract FilePath Proposal] and met an immediate thunderous applause. Inspired by this enthusiasm, they further pursued the career of professional Haskell programming and focused on more interesting things. 16 months later, a programmer under the pseudonym XT got so fed up with the situation and released a package called `rawfilepath` out of a purely selfish intent.

## So what is this?

`RawFilePath` is a data type provided by the `unix` package. It has no performance issues because it is `ByteString` which is packed. It has no encoding issues because it is `ByteString` which is a sequence of bytes instead of characters. However, the functions in `unix` are low-level, and the higher-level packages such as `process` and `directory` are strictly tied to `FilePath`.

So I decided to start writing the `RawFilePath` version of those functions.

## Advantages

- High performance
- No round-trip encoding issue
- Minimal dependencies (`bytestring`, `unix`, and `base`)
- Lightweight library (under 400 total lines of code)
- Available now

## Documentation

[API documentation of rawfilepath on Stackage](https://www.stackage.org/package/rawfilepath).

## To do

`rawfilepath` is in an early stage, although major backwards-incompatible changes are unlikely to happen. We can probably port more system functions that are present in `process` or `directory`

Patches will be highly appreciated.

[Abstract FilePath Proposal]: https://ghc.haskell.org/trac/ghc/wiki/Proposal/AbstractFilePath

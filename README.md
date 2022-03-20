# rawfilepath

Version: 1.0.1

The `unix` package provides `RawFilePath` which is a type synonym of `ByteString`. Unlike `FilePath` (which is `String`), it has no performance issues because it is `ByteString`. It has no encoding issues because it is `ByteString` which is a sequence of bytes instead of characters.

That's all good. With `RawFilePath`, we can properly separate the "sequence of bytes" and the "sequence of Unicode characters." The control is yours. Properly encode or decode them with UTF-8 or UTF-16 or any codec of your choice.

However,

- The functions in `unix` are low-level.
- The higher-level packages such as `process` and `directory` are strictly tied to `FilePath`.

This library provides the higher-level interface with `RawFilePath`.

## Advantages

`rawfilepath` is easy to use.

```haskell
{-# language OverloadedStrings #-}

import RawFilePath
import System.IO
import qualified Data.ByteString as B


main :: IO ()
main = do
  p <- startProcess $ proc "sed" ["-e", "s/\\>/!/g"]
    `setStdin` CreatePipe
    `setStdout` CreatePipe
  B.hPut (processStdin p) "Lorem ipsum dolor sit amet"
  hClose (processStdin p)
  result <- B.hGetContents (processStdout p)
  print result
  -- "Lorem! ipsum! dolor! sit! amet!"
```

- High performance
- No round-trip encoding issue
- Minimal dependencies (three packages: `bytestring`, `unix`, and `base`)
- Lightweight library (under 400 total lines of code)
- Type safety (inspired by typed-process)
- **Available now**

## Rationale

### Performance

Traditional `String` is notorious:

- 24 bytes (three words) required for one character (the List constructor, the actual Char value, and the pointer to the next List constructor). 24x memory consumption.
- Heap fragmentation causing malloc/free overhead
- A lot of pointer chasing for reading: Devastates the cache hit rate
- A lot of pointer chasing plus a lot of heap object allocation for manipulation (appending, slicing, etc.)
- Completely unnecessary but mandatory conversions and memory allocation when the data is sent to or received from the outside world

This already makes us unhappy enough to avoid `String`. `FilePath` is a type synonym of `String`. Use `RawFilePath` instead. It's faster and occupies less memory.

### Encoding

`FilePath` is a type synonym of `String`. This is a bigger problem than what `String` already has, because it's not just a performance issue anymore; it's a correctness issue as there is no **encoding information**.

A syscall would give you (or expect from you) a series of bytes, but `String` is a series of characters. But how do you know the system's encoding? NTFS is UTF-16, and FAT32 uses the OEM character set. On Linux, there is no filesystem-level encoding. Would Haskell somehow magically figure out the system's encoding information and encode/decode accordingly? Well, there is no magic. `FilePath` has completely no guarantee of correct behavior at all, especially when there are non-ASCII letters.

### AFPP

In June 2015, three bright Haskell programmers came up with an elegant solution called the [Abstract FilePath Proposal] and met an immediate thunderous applause. Inspired by this enthusiasm, they further pursued the career of professional Haskell programming and focused on more interesting things. (sigh)

This library provides a stable and high-performance API that is available now.

## Documentation

[API documentation of rawfilepath on Stackage](https://www.stackage.org/package/rawfilepath).

## To do

`rawfilepath` is stable. We don't expect any backward-incompatible changes. But we do want to port more system functions that are present in `process` or `directory`. We'll need to be a bit careful about their API for stability, though.

Patches will be highly appreciated.

[Abstract FilePath Proposal]: https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/abstract-file-path

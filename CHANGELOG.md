# Change Log

## 0.2.3 - 2017-05-07

### Added

- A little bit more documentation

### Changed

- `stack.yaml` is no longer included. It has been added to `.gitignore`.

## 0.2.2 - 2017-04-23

### Fixed

- The missing "processFlags.h" in the "c-sources" section of the cabal file. This made the builds fail.

## 0.2.1 - 2017-04-23

### Added

- Convenience functions that were mistakenly removed: `getDirectoryFilesRecursive` and `tryRemoveFile`.

## 0.2.0 - 2017-04-20

### Added

- Higher-level directory functions
- Utility functions for process (callProcess, readProcessWithExitCode)

### Changed

- Module hierarchy now follows [Taylor Fausak's 2016 advice](http://taylor.fausak.me/2016/12/05/haskell-package-checklist/). `RawFilePath` will be the top-level module which re-exports everything.

## 0.1.1 - 2017-03-21

### Added

- A new module, `System.Process.RawFilePath`. This new interface is pretty nice. It has type-safe `Handle`s instead of `Maybe Handle`s (inspired by the `typed-process` package). It also performs the process invocation entirely inside the FFI, which solves [#1].

[#1]: https://github.com/xtendo-org/rawfilepath/issues/1

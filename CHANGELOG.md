# Change Log

## 1.1.1 - 2024-07-21

- Fixed the deprecated `memcpy` (now uses `Foreign.Marshal.Utils.copyBytes` instead)
- Added the "bailout" series of functions for un-typing processes.

## 1.1.0 - 2023-08-29

### Fixed

- Fixed `Data.ByteString.RawFilePath` to build with unix-2.8 (and therefore GHC 9.6)

## 1.0.1 - 2022-03-20

### Fixed

- Fixed the module import statements to compile with GHC 9.2.2.

## 1.0.0 - 2021-10-16

### Fixed

- Fixed the namespace conflict issue in the new GHC.IO.Handle.FD interface. Now the library works fine with ghc-9.0.1.

### Changed

- After four years of use, it is clear that the API is stable. We are now 1.0.0.

### Fixed

- Fixed test

## 0.2.4 - 2017-05-11

### Fixed

- Fixed test

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

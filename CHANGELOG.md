# Change Log

## 0.1.1 - 2017-03-21

### Added

- A new module, `System.Process.RawFilePath`. This new interface is pretty nice. It has type-safe `Handle`s instead of `Maybe Handle`s (inspired by the `typed-process` package). It also performs the process invocation entirely inside the FFI, which solves [#1].

[#1] https://github.com/xtendo-org/rawfilepath/issues/1

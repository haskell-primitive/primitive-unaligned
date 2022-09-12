# Revision history for primitive-unaligned

## 0.1.1.2 -- 2022-09-12

* Support GHC 9.4.1

## 0.1.1.1 -- 2020-01-09

* Only derive `PrimUnaligned` instances for `cc`, `gid`, `nlink`, and `uid`
  on platforms where these types exist. Prior to this change, Windows users
  were unable to build this library.

## 0.1.1.0 -- 2019-06-25

* First release

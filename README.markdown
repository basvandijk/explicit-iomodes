The module `System.IO.ExplicitIOModes` exports a `Handle` to a file
which is parameterized with the IOMode the handle is in. All
operations on handles explicitly specify the needed IOMode. This way
it is impossible to read from a write-only handle or write to a
read-only handle for example.

See the [explicit-iomodes-bytestring] and [explicit-iomodes-text]
packages for `ByteString` / `Text` operations.

[explicit-iomodes-bytestring]: http://hackage.haskell.org/package/explicit-iomodes-bytestring
[explicit-iomodes-text]: http://hackage.haskell.org/package/explicit-iomodes-text

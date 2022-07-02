---
title: Haskell strings field guide
date: 20181015T00:00Z
---

Here's how to convert between byte, string, and character types in Haskell:

|Function                      |Type                    |
|------------------------------|------------------------|
|encodeUtf8                    |Text -> ByteString      |
|unpack                        |Text -> String          |
|decodeLatin1                  |ByteString -> Text      |
|decodeUtf8With lenientDecode  |ByteString -> Text      |
|BC.unpack                     |ByteString -> String    |
|UTF8.toString                 |ByteString -> String    |
|B.unpack                      |ByteString -> [Word8]   |
|UTF8.fromString               |String -> ByteString    |
|pack                          |String -> Text          |
|w2c                           |Word8 -> Char           |
|B.pack                        |[Word8] -> ByteString   |
|c2w ❌ partial                |Char -> Word8           |
|BC.pack ❌ partial            |String -> ByteString    |
|BSC.string ❌ stops streaming |String -> BSC.ByteString|

Note that Char is finite:

```haskell
toEnum 10000000 :: Char
*** Exception: Prelude.chr: bad argument: 10000000
```

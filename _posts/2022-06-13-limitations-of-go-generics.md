---
layout: post
title: Limitations of Go generics
categories:
- blog
---

Go does not allow type parameters (generics) in method declarations, and that limits the APIs you can make.

Here's an example when you might encounter that problem and how to work around it:

Say you're building a library to process streams of values, and you start with this type:

```go
// Stream receives In values from upstream and sends Out values downstream.
type Stream[In, Out] ...
```

Ideally, we could write the following generic `.connect()` method:

```go
//                           vvvvvvv ❌ not allowed by Go!
func (s Stream[A, B]) connect[C any](other Stream[B, C]) Stream[A, C]
```

But [Go does not allow type parameters in method declarations](https://go.googlesource.com/proposal/+/refs/heads/master/design/43651-type-parameters.md#no-parameterized-methods). That means the `.connect()` method can't introduce any new types:

```go
//                                                      vvvvvv ✅ OK
func (s Stream[A, B]) connect(other Stream[B, B]) Stream[A, B] { ... }
```

That restricts the kinds of processing we can do by chaining `.connect()` methods together:

```go
lines
	.connect(omitEmpty) // ✅ (string -> string) same type
	.connect(getLength) // ❌ (string -> int   ) new type
	.connect(print)
```

We can get around this limitation by converting `.connect()` into a top-level `connect()` function that can change the stream type 🎉:

```go
func connect[A, B, C any](s1 Stream[A, B], s2 Stream[B, C]) Stream[A, C]
```

Unfortunately, readability suffers a little:

```go
connect(connect(connect(lines, omitEmpty), getLength), print)
```

Or written with right-associativity:

```go
connect(lines, connect(omitEmpty, connect(getLength, print)))
```

Or on separate lines:

```go
s1 := lines
s2 := connect(s1, omitEmpty)
s3 := connect(s2, getLength)
s4 := connect(s3, print)
```

We need separate variables for each step in the stream, but other than that it's not much different from method chaining above!

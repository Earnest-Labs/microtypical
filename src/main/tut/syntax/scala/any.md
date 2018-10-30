---
layout: docs
title: Syntax Extensions for Any
section: syntax
---

This includes both the traditional `thrush` operator and a general `tap` operator.

##Contents

It is often useful to apply functions left to right rather than in the usual order.
A syntax extension for generic over all types is provided:
```tut:silent
import com.earnest.microtypical.syntax.scala.any._
```

This allows us to do things like the following:
```tut
val content = 5 |> (_ * 2) |> (_ - 1)
```

This can be clearer in some cases.

Similarly, a `tap`-like operator is offered with allows abstracting effectful operations cleanly.
```
var requestValue = 5

requestValue.withEffect(_ -= 1).withEffect(_ *= 3)
```


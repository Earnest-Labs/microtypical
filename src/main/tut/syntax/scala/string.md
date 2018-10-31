---
layout: docs
title: Syntax Extensions for Strings
section: syntax
---

Mostly, this provides an unsafe conversion to the `NonEmptyString` type, which is useful when working with constants, especially in tests.

```tut:silent
import scala.util.Try
import com.earnest.microtypical.syntax.scala.string._
```

We can convert fixed strings to `NonEmptyStrings` conveniently:

```tut
"This is not empty".nesUnsafe
```

However, if the string is empty...

```tut
Try("".nesUnsafe)
```

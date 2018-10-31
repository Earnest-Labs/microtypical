---
layout: docs
title: Syntax Extensions for Longs
section: syntax
---

This allows one to convert downwards to `Int` in a safe manner:
```tut:silent
import com.earnest.microtypical.syntax.scala.long._
```

Invoked as:
```tut
5L.maybeInt

Long.MaxValue.maybeInt
```

---
layout: docs
title: Syntax Extensions for Scala Try
section: syntax
---

Importing:

```tut:silent
import scala.util.Try
import com.earnest.microtypical.syntax.scala.try_._
```

Will provide you with the ability to easily transform your `Try` into a Twitter `Try` if necessary:

```tut
Try("all good").toTwitter
```

As well as folding over the cases of the `Try`:

```tut
Try("This works").fold(identity, identity)
Try({throw new Exception("Oh no")}).fold(identity, identity)
```

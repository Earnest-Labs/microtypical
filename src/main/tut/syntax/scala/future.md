---
layout: docs
title: Syntax Extensions for Scala Future
section: syntax
---

This primarily includes a convenience method to convert a `Scala` `Future` into a `Twitter` `Future`.

Importing as:
```tut:silent
import scala.concurrent.{ExecutionContext, Future}
import com.earnest.microtypical.syntax.scala.future._

implicit val ec = ExecutionContext.global
```

Allows one to do:
```tut
Future(5).toTwitter
```

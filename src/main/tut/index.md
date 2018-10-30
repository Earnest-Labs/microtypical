---
layout: home
title:  "microtypical"
section: "microtypical"
technologies:
 - first: ["Scala", "Programming language!"]
 - second: ["Cats", "Functional programming in Scala."]
 - third: ["Shapeless", "Dependent types and type level programming."]
---

# Earnest Standard Utils For Scala

Hi, welcome to Earnest's Standard Utils for Scala.
```tut
val hello = "Hello World"
```

This includes lots of cool stuff. For example,
```tut:silent
import com.earnest.microtypical.syntax.scala.string._
```
will let you do something like,
```tut
"this is nonempty".nesUnsafe
```

This library is divided loosely into two parts.
The [first](./data/) comprises validated data types and tooling to help you write your own validated data types.
The [second](./syntax/) comprises various syntax extensions and goodies that you may find useful, like the example above.

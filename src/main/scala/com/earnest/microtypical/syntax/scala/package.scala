package com.earnest.microtypical.syntax

package object scala {
  object all extends ToAnyOps
    with ToIterableOps
    with ToLongOps
    with ToScalaFutureOps
    with ToScalaTryOps
    with ToStringOps
}

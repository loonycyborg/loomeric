Loomeric: alternative numeric prelude for Haskell
=================================================

The aim of this project is to improve upon the infamously
supoptimal numeric classes from Haskell 98 report yet maintain
reasonable compatibility with existing sourcecode.

Design goals
============

Typeclasses should be designed so they reflect underlying mathematical
structures as much as feasible so misusing them would a raise compile error
(aka make invalid states unrepresentable). For example, with standard prelude
Word type supports standard subtraction making it prone to underflow if you
subtract to below 0 which causes it to wrap around to astronomic values. In this
library Word is merely a Semiring thus it doesn't have (-) operator forcing
checked subtraction only for it.

Features
========

- Finer grained typeclasses for algebraic structures. Most basic operations
are unlocked by appropriate typeclass, for example AdditiveSemigroup unlocks (+)
and only its subclass AdditiveGroup unlocks (-). This makes it possible to use
those operators for structures that are pretty far removed from standard numbers
and can't support a lawful Num instance from standard prelude.
- Checked operations -? and /? that return a Maybe for algebraic structures that
aren't closed under subtraction or division.


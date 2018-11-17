Add Likelyhood Annotations
==============

.. proposal-number:: _.
.. trac-ticket:: _.
.. implemented:: _.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

Here you should write a short abstract motivating and briefly summarizing the proposed change.


Motivation
------------

Programmers often know a good deal more about program behaviour than we can currently
communicate to the compiler, leading to missed optimization opportunities.

Consider types like

 - ``data List a = Nil | Cons a (List a)``

 - ``data Either a b = Left a | Right b``

We usually want to optimize for the common case.
Lists are usually not empty.
Left is often used to represent an exceptional case. Making Right the common case.

A programmer has this information but no way to communicate this to the compiler.

Communicating this kind of information can inform optimization passes to produce
faster code. In particular potential gains include:
 * Avoid inlining into unlikely case alternatives.
 * Produce better code layout.
 * Better register allocation.

Proposed Change Specification
-----------------------------

We propose a new Pramgma: {-# LIKELY <NUM> #-}

This will be useable in two cases with different behaviour:

 - Simple case expressions.

 - Data type definitions of Sum Types.

Uses where the requirements are not satisfied will result in warnings similar to
the ones from bad UNPACK pragmas. Likelyhood values must be >= 0.

Case expressions:

Simple case expressions are case expressions which:
 - Don't contain nested patterns.
 - Only match on ADTs or GADT.

Given a simple case expression with n alternatives [A1 .. An],
with likelyhoods [L1 .. Ln], Ls = sum [L1 .. Ln] GHC will optimize code under the assumption that
the chance for each alternative to be taken is L1/Ls.

In other words alternatives with likelyhood zero are assumed to be almost never taken. (But still correct IF taken!)
For alternatives with Li > 0 the likelyhood values give the relative frequency of the alternative.

We give an likelyhood by <Pattern> -> <Pragma> <rhs>. See example below.

If a case has no annotations assumptions about likelyhoods are up to the implementation.
If a case has alternatives with and without likelyhood information the compiler
will give a warning and the unannotated alternatives are given an implementation dependent likelyhood.
If a case doesn't match all possible constructors the unmachted constructors are assumed to have likelyhood zero.

For reference consider this example:

::

 head xs = case xs of
    [] -> {-# LIKELY 0 #-} error "Empty list"
    (x:_) -> {-# LIKELY 1 #-} x

Here we assume the error case is never taken. Further we assume that the second alternative is always taken.

Data types behaviour derive from the case behavior.

The syntax for data definition by example is as follows:

::
 data Foo
   = {-# LIKELY <NUM> #-} Bar
   | {-# LIKELY <NUM> #-} Baz

If likelyhood information for data types is given it must be given for all constructors.

When pattern matching on an expression of such a type using a simple case expression
the default likelyhoods given by the information in the definition.

When pattern matching on such an expression using other means the likelyhood information
might be considered by the compiler but no guarantees are given.


Effect and Interactions
-----------------------

This makes it possible to have GHC optimize better for hot code paths.

Currently high performance code tends to vary things like constructor order manually for maximal performance.
This will provide a more reliable alternative which will remain stable between versions.


Costs and Drawbacks
-------------------
This comes with an increase in compiler complexity as one would expect.

I don't expect negative impacts on existing code or users not making use of this feature.


Alternatives
------------
None I know of.

Unresolved Questions
--------------------

I think the handling of partial or missing information here is reasonable.

However people often disagree what is reasonable so give feedback if you disagree with these.

Implementation Plan
-------------------
I would implement this.

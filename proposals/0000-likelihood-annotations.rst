Add Likelihood Annotations
==============

.. proposal-number:: _.
.. trac-ticket:: _.
.. implemented:: _.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/182>`_.
.. sectnum::
.. contents::

A lot of performance can be gained by optimizing for hot code paths.
However GHC currently does not allow users to actually give hints about which code
paths are hot code paths.

This proposal changes this.


Motivation
------------

Programmers often know a good deal more about program behaviour than we can currently
communicate to the compiler. This leads to missed optimization opportunities.

Consider types like ``data Either a b = Left a | Right b``

Left is often used to represent an exceptional case. Making Right the common case.

Based on this information and given code like ``either f g x`` we might want
to inline g but not f for example.

The same logic applies to many other cases. For example Lists are rarely empty.

A programmer has this information but no way to communicate this to the compiler.

Communicating this kind of information can allow optimization passes to produce
faster code. In particular potential gains include:
 * Avoid inlining into unlikely case alternatives.
 * Produce better code layout.
 * Better register allocation.

Proposed Change Specification
-----------------------------

We propose a new Pramgma: ``{-# LIKELY <NUM> #-}``

This will be useable in two kinds of places with different semantics:
 - Simple case expressions.
 - Data type definitions of Sum Types.

Uses where the requirements are not satisfied will result in warnings similar to
the ones from bad UNPACK pragmas. Likelihood values must be >= 0.

Case expressions:

Simple case expressions are case expressions which:
 - Don't contain nested patterns.
 - Don't use guards.
 - Only match on ADTs or GADT.

Given a simple case expression with n alternatives [A1 .. An],
with likelihoods [L1 .. Ln], Ls = sum [L1 .. Ln] GHC will optimize code under the assumption that
the chance for each alternative to be taken is L1/Ls.

In other words alternatives with likelihood zero are assumed to be almost never taken. (But still correct IF taken!)
For alternatives with Li > 0 the likelihood gives the relative frequency of alternatives.

We give an likelihood by <Pattern> -> <Pragma> <rhs>. See example below.

If a case has no annotations assumptions about likelihoods are up to the implementation.
If a case has alternatives with and without likelihood information the compiler
will give a warning and the unannotated alternatives are given an implementation dependent likelihood.
If a case doesn't match all possible constructors the unmachted constructors are assumed to have likelihood zero.

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

If likelihood information for data types is given it must be given for all constructors.

When pattern matching on an expression of such a type using a simple case expression
the default likelihoods given by the information in the definition.

When pattern matching on such an expression using other means the likelihood information
might be considered by the compiler but no guarantees are given.

Pattern matches using nested arguments, function definitions by pattern matching
and guards are excluded for now for two reasons. It is not always obvious how to assign weights from the
overall pattern to the individual Constructors. And it needlessly increases implementation complexity.

Effect and Interactions
-----------------------

This makes it possible to have GHC optimize better for hot code paths.

Currently high performance code tends to vary things like constructor order manually for maximal performance.
This will provide a more reliable alternative which will remain stable between versions.

To give some examples:

::
 f x = case x of
  Just v  -> {-# LIKELY 1 #-} e1
  Nothing -> {-# LIKELY 0 #-} e2

We can avoid inlining e2 knowing it is rarely called, reducing code size and
 making f itself a better inlineing candidate.

For more low level optimization we always want control flow for the hot path to be
linear. This means given the code below:

::
 f x = case x of
         C1 -> {-# LIKELY 1 #-} e1
         C2 -> {-# LIKELY 0 #-} e2

We want assemby (simplified to just the control flow) to look like this:

::
 f:
  <if x == C2> goto e2:
 e1:
  <e1_code>
 e2:
  <e2_code>

Currently if we get this layout depends implicitly on the order of constructors and the GHC
version. With the pragma GHC will try to generate this layout when beneficial instead.


Costs and Drawbacks
-------------------
This comes with an increase in compiler complexity as one would expect.

I don't expect negative impacts on existing code
or users not making use of this feature.


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

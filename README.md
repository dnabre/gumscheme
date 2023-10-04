# GUM Scheme

This is a fork of [UNM Scheme](https://www.cs.unm.edu/~williams/unmscheme.html) that is licensed under the [LGPL](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html). The goal is to
provide the same graphics and image processing extensions, while adding ease-of-use features and expanding the existing features in stability, speed, and scope.

Most of this will be just by pulling in libraries instead of using basic homespun code. On top of functionality goals, the aim is to keep the implementation small and understandable. So that anyone with a moderate understanding of C programming can see how different language features are implemented.

To avoid any confusion with the original project and/or associations with the university it was developed at, this fork has been dubbed 'GUM Scheme'. This is a mushing together of GNU/GPL and UNM that results in a word.

## Implementation

GUM Scheme is an R5RS Scheme. It provides a REPL that compiles
expressions to bytecode, which is interpreted on a virtual machine. The specifics of both are based on the thesis, [Three implementation models for Scheme](https://citeseerx.ist.psu.edu/pdf/bc896e5336120b0f4ad00feb500cd7ce70134836) by R. Kent Dybvig^1.

These included features/implementation details are likely to be changed in short term.

* Garbage collection is a simple copy-collector.
* Linear Algebra operations are implemented as homespun C-functions.
* It provides the ability to load/save images only in the ASCII version of PGM and PPM
* Plumbing Graphics (see [Plumbing Graphics Reference](https://www.cs.unm.edu/~williams/cs257/graphics.html))
* Several object types don't have implementations for GC, and just stick around forever.
* [Singular Value Decomposition](https://en.wikipedia.org/wiki/Singular_value_decomposition) function that needs to be replaced.
* `longjmp` is used throughout for error handling and implementing `call/cc`.

## Immediate Goals
* Changes to make the code build and work properly on its main three platforms (Linux, Windows, and Mac).
* Updating the build system from crude hand-written Makefiles to autoconf or CMake.
* Using standard/common libraries to provide functionality more robust versions of existing functionality
    * Linear Algebra
    * Bignums
    * Fourier Transforms
    * Image Reading and Writing

We want to keep acknowledges and historical citations, but code, file names, and the like that refer to original version should be cleaned up.  Breaking the code intosensible pieces so that contributors don't have search around a 11+KLOCs C file. The general reorganizing/renaming may be primarily a matter of personal preference, but a side effect will be to make the code easier to work with.

Each of the implementation bullets above are items which could benefit
from changing, updating, or outright removing. The Plumbing Graphics
system for example, may have pedagogical uses, but isn't of great use
generally speaking.

## History of UNM Scheme

[UNM Scheme](https://www.cs.unm.edu/~williams/unmscheme.html) was created by Lance R. Williams at the University of New
Mexico. It was used in a number of courses, in particular CS 422/522:
Digital Image Processing.

It is an implementation of R5RS Scheme with additional functionality
mainly for Linear Algebra, Image Manipulation and Processing, and Image Display.

A student was hired in Spring 2010 to work on this program with three
main goals:

* Fix the immediate issue of Image Viewing not working on Mac OS X due to recent updates to that platform.
* Provide an excuse to provide financial support very considerately for the student that semester.
* Improve, fix, and add to the program generally.

The most notable long-term change made was the transition from a broad but non-standard Open Source license to a standardized one, namely the GNU Lesser Public License v 2.1 ([LGPL](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html)). This relicensing was done with the explicit and direct permission of the sole author and copyright holder. UNM Scheme, version 2.6.3, was officially released under the LGPL on 2010-02-03. This repo starts directly from that release.

As sometimes happens with software development all the changes made from that endeavor were reverted and the original author continued with official UNM Scheme development on the codebase prior to any of those changes being made. Its most recent release was version 2.7 on 2011-08-31.

A small collection of images is normally part of the UNM Scheme distribution. Being of unknown providence, they are being excluded.

There is also one piece of code, a C implementation of [Singular Value Decomposition](https://en.wikipedia.org/wiki/Singular_value_decomposition), which is listed as "Copyright 1996 by Carl Edward Rasmussen" and credited to being "adapted with permission" from a Pascal implementation from J. C. Nash's, *Compact numerical methods for computers* (Hilger 1990). Being an isolated function, this is far easier to reimplement in the future from scratch than deal with any concern about its copyright.

# References

1. R. K. Dybvig. 1987. Three implementation models for scheme. Ph.D.
   Dissertation. University of North Carolina at Chapel Hill, USA. Order
   Number: UMI Order No. GAX87-22287.

********

### This project is NOT associated with the University of New Mexico.

This project is an independent Open Source Project. Its maintainers,developers, and contributors have no association with the original authors of the software or their associated organizations.



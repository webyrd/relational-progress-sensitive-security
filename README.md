relational-progress-sensitive-security
======================================

Relational implementation of type system and semantics from "Precise Enforcement of Progress-Sensitive Security" by Moore, Askarov, and Chong, CCS'12:

http://people.seas.harvard.edu/~aslan/ccs12.pdf

To run the tests, load the file ```pse.scm``` in the ```miniKanren-version``` directory.  All code is written in miniKanren (http://www.miniKanren.net), and tested under Petite Chez Scheme 8.4.

TODO:

* implement parser/unparser
* double-check output to make sure tests are correct
* add more test programs
* implement semantics
* implement multi-level lattices

QUESTIONS/ISSUES:

* Would this code benefit from the use of LVars (lattice variables)?  I think it might.
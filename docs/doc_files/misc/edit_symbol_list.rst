Editing the symbol list
-----------------------

Clicking the button “Load symbols from equations” enables the **symbol
table for editing** while the **correct syntax of the equations** is
being checked with the internal function parser. In the case of an error
corresponding warnings are given via Windows Message-box dialogs, which
are acknowledged, and one has then look for reason behind that error
message.

The symbol table consists of four text columns "**Symbol**", "**Type**",
"**Unit**" and "**Meaning**".

The symbols which were extracted automatically are characterised in the
column “Type” as dependent (a) or independent (u) quantities, where the
independent ones are listed first. **Only the independent symbols
(quantities) are the ones the measurement uncertainties of which are
considered by the program. This is the important point which prevents at
the very beginning that covariances could otherwise be inferred between
dependent symbols because they would share common (independent)
symbols.**

Within the symbol list, such input quantities for which a mean and its
uncertainty are to be derived from a data set, can be marked in the type
column with “m“ instead of “a“ (dependent) or “u“ (independent).
Furthermore, a quantity, to be derived by an equation, which shall be
treated as a parameter without uncertainty, can be defined by sitting
its type to “p“.

The columns “Unit” and “Meaning” are to be completed by the user.
However, it is recommended to this later, at least after the equations
have got their final state, because subsequent changes in the equations
may lead to a different ordering of the symbols. This may also be
postponed to a later session.

Due to subsequent changes within equations it may happen, that some of
the symbols do not occur in the present set of equations. Such symbols
are listed at the end of the symbol list, where they can be row-wise
removed by the user (see below: Notes on editing tables).

After a mouse click inside a table **mouse wheel scrolling** is
possible.

**Very important:**

Most often it is necessary to add manually such symbols at the end of
the symbol table which do not explicitly occur in the equations but are
needed within those formulae describing standard uncertainties; e.g.
counting times tm or t0 are required to calculate the standard
uncertainty of gross or background counting rates, sqrt(Rg / tm) or
sqrt(R0 / t0), respectively.

**See** `Notes on editing tables <#within-tables-delete-rows-working-with-column-blocks>`__
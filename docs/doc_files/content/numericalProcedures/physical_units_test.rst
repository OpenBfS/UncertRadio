Calculation of physical units for dependent variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

With the program version 2.4.13, UncertRadio contains a menu item which
allows as a test to derive **the physical units of dependent variables**
based on a numerical algorithm. The units of the input quantities, often
given by "derived units" are changed to "basic units"; in addition to
this, the associated scaling factors are determined. If, for example, a
counting duration variable was given the unit "min", an associated
scaling factor of 60 is applied for changing to the basic unit "s". The
description of basic units and their derived units and of the algorithm
for "calculating" the units of dependent variables is given in `chapter
7.21 <#treatment-of-physical-units>`__.

A CSV file distributed with the program contains only a small number of
units, which is nearly sufficient for measurements of radioactivity. If
necessary, the basic units within the CSV file can be modified by the
user.

By the menu item "Edit – test physical units", the transformation to
basic units can be tested. In UncertRadio's text editor a comparison of
"original" and "converted" units is shown for the list of quantities
(symbols), as well as their associated values and uncertainties. At the
begin of this list, a first error message is shown in case of conversion
errors found by the program, which can indicate indirectly a wrong
combination of units in the indicated number of the equation.

By programming, the routine behind this menu item was applied to all of
UncertRadios example projects. There were indeed errors found, and it
was necessary in most of the examples to replace the unit "1" of a
detection probability by the unit "1/Bq/s" in order to get the unit "Bq"
for an activity as output quantity. More details are given in `chapter
7.21.3 <#invoking-the-test-of-unit-calculations>`__.
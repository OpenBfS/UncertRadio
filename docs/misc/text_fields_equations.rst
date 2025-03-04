Text field for equations
------------------------

Equations can be written into this text field line-by-line. A special
end-of-line character is not necessary; only in the case that **an
equation has to be continued in additional lines** each (but not the
last) line of this equation must have a “&” character at its end.

The **equations** must be **set up** in a **hierarchical** way.

One **starts with that basic equation** which defines the **output
quantity y**. This may for example read:

*y = w \* Rn - Ai*

Naturally, another symbol can be used for the output quantity. In the
following lines for those symbols used only in the right-hand parts of
the preceding equations, if they do not already represent a primary
input quantity, further equations are defined (**secondary equations**),
for example:

*Rn = Rg - R0* net counting rate

w *= 1. / (eps \* eta \* m) \* f1* procedure dependent calibration
factor

*f1 = exp(+log(2.) \* t1 / tr)* inverse decay factor

*Ai (=z2)* an interference contribution to be subtracted

Herein, *Ai* represents an interference contribution to the activity,
i.e. *Ai* equals the constant *FC* determined internally by the program.
The factor *w* corresponds to the other constant, *FL.*

Notes: a) If more than one output quantity were defined for the project,
e.g. three, then for each output quantity one basic equation must exist;
these then are the first three equations. b) In such cases where
interference by another radionuclide exists, *Rn* must be understood as
the “\ **procedure dependent net counting rate**\ ” the equation of
which contains an extra term calculated for this interference.

*The simple expression Rn = Rg - R0 may be used also in the case of
additional interference contributions. The latter (interference)
contribution is taken automatically into account by the internally
determined auxiliary quantity FC (*\ `see
also <#numerical-procedures>`__\ *).*

Because of the hierarchical structure, the **equations are evaluated
from bottom to top for obtaining values** for all the quantities. This
means that in any equation only such symbols can be used in it belonging
to secondary (auxiliary) equations following that equation. The program
internally tests whether this condition is fulfilled; if not, the user
will get an associated warning.

It is necessary to **use explicitly an equation defining the net
counting rate** *Rn.* In this important equation it is allowed for the
symbol of the gross counting rate to be multiplied with a factor; in
seldom cases, this may be necessary. The value of this factor is
identified by the program; it only may play a role for determining
Decision threshold and Detection limit.

Note: The gross counting rate symbol must be directly contained in the
equation defining the net counting rate, or, another symbol in the
latter equation points to a further auxiliary equation in which then
contains it.

Example: *Rn = Rn1 - Rblank*; *Rn1 = Rg - R0*.

The **procedure dependent factor w** in the above example contains the
inverse decay factor *f1* for correcting the radioactive decay of a
radionuclide r, having the half-live *tr,* in the time duration *t1*
between sampling and the beginning of the measurement. The detection
efficiency, chemical yield and sample mass are *eps*, *eta* and *m,*
respectively.

The **symbols** occurring in the equations to the left of the equation
sign are classified as **“dependent (a)”**, those of the symbols of the
right-hand sides and not occurring somewhere left of the equation sign,
as **“independent (u)” input quantities**.

It is possible to make full use of secondary equations. By doing this,
in the conventional way of uncertainty propagation it happens that
**easily overlooked covariances between dependent quantities occur**
**when using their uncertainties for propagation**. However, **this
cannot happen in UncertRadio**, because it uses only uncertainties from
independent quantities.

The **syntax for writing formula symbols** should be the **same as for
creating variable names in programming languages**. The program here
uses FORTRAN 90 internally. It is not differentiated between lower and
upper-case characters. However, it is recommended to the user to make
this differentiation for a better readability of the equations. The use
o f the underscore (\_) is allowed within symbol names, but not for the
first character of a name. A formula symbol must always begin with an
alphabetic character.

For numbers occurring in equations as well as in tables the **decimal
character** must always be a dot (decimal point). Numbers in equations,
e.g. 1. and 2. within the equations for *Fact* and *f1* shown above, are
interpreted always as double precision numbers internally by the
function parser.

**Internal functions and operators:**

All internal calculations are done with "double precision" arithmetic.

The following intrinsic arithmetic functions can be used, similarly -
but not fully identical - as in MS Excel:

sqrt(x) square root function

exp(x) exponential function

log(x), ln(x) natural logarithm

log10(x) common logarithm

A new function fd() with three parameters can be used which calculates a
decay factor averaged over the counting duration:

fd(tA,tm,xlam) = exp(-xlam*tA) \* (1 - exp(-xlam*tm)) / (xlam*tm)

This function did not exist in UR1.

Some projects may require applying an uncertainty u(x) of an input
quantity value x as an own value. A **function uval(x)** was therefore
introduced by extending the function parser. As an example, the relative
uncertainty :math:`u_{rel}(w)` can be introduced as a variable urelw as
follows:

urelw = uval(w) / w

The argument of the function uval() must be an existing single symbol
taken belonging to the symbol table. An arithmetic expression of more
than one variables is not allowed; the latter case, e.g. uval(a+b),
would mean to perform an uncertainty propagation for such an expression,
what uval() is not made for. If the value of uval(x) shall be treated as
a constant value, x must not represent a gross count or gross count
rate, because their values and uncertainties vary during calculating the
decision threshold and the detection limit.

In addition to conventional operators +, -, \* and /, for the
exponentiation one can use \*\*: a**b means a to the power of b, for
which writing a^b is also allowed.

**Notes:**

The program already contains a procedure which allows **estimating the
net counting rate as a result of weighted multi-linear Least squares
fitting applied to a measured decay curve**. This is available for decay
curves of Y-90 and may be easily applied e.g. to combined build-up/decay
curves measured in a source containing Y-90, Sr-89 and Sr-90.

*This tool is not yet in its final state. Therefore, it is necessary to
consider further applications; tips about such examples would be highly
acknowledged by the author!*

Further information: `use of Least squares fit <#URH_LSQ_EN>`__

For the **field of gamma spectrometry** there is a procedure available
allowing **the activity of a radionuclide with several gamma lines to be
estimated as a mean of single line activities.** Two methods for
calculating means are offered.

The first method is that of the **weighted mean**, for which so-called
“internal” and “external” standard deviations can be calculated. If the
values of the two standard deviations are of quite similar size, one can
draw the conclusion that the single line activity values are under
“statistical control”. This is a well-known procedure; however, it
should be noted that the use of the “external” standard deviation is not
really Bayes conform.

The second method uses a matrix-based least squares procedure instead of
formulae for the weighted mean. It is better suited for including
covariances.

This method can only be used, if the gamma lines used for calculating
the activity of the radionuclide are not interfered by gamma lines
belonging to other radionuclides.

Further information: `Activity calculation from several gamma
lines <#activity-determination-from-several-gamma-lines>`__
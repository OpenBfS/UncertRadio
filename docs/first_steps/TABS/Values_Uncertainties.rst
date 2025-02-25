TAB “Values, Uncertainties”
^^^^^^^^^^^^^^^^^^^^^^^^^^^

A major part of this TAB consists of a 10-column table for the input of
measurement values and uncertainties of the independent measured
quantities (input quantities). The columns are:

.. list-table::
    :widths: auto
    :header-rows: 0

    * - 1:
      - **Symbol**
      - text field
      - formula symbol

    * - 2:
      - **Type**
      - text field
      - Parameter type: **a** for dependent, **u** for independent,
        **m** for a quantity mean, **p**
        for a parameter to be calculated, without uncertainty

    * - 3:
      - **Unit**
      - text field
      - input of unit

    * - 4:
      - **Value**
      - number field
      - value of measured quantity

    * - 5:
      - **distribut.**
      - select. field
      - selection of the quantity`s distribution
        (normal / rectangular / triangular / others)
        The `(N+x) rule for very low count numbers <#low-level-applications-nx-rule>`__
        may be selected here, implying Gamma distributions of the associated
        counting rate variables

    * - 6:
      - **StdDev formula**
      - text field
      - formula of the standard deviation of the quantity;
        no formula if (N+x) rule has been selected;
        (the internal coverage factor is always 1);
        always "." has to used for the decimal point

    * - 7:
      - **StdDev value**
      - number field
      - value of the uncertainty for normal distribution
        if the (N+x) rule has been selected, **nothing** shall be entered
        these cells!(the internal coverage factor is always 1)

    * - 8:
      - **Half width**
      - number field
      - Half width of rectangular/triangular distribution
        (the internal coverage factor is always 1)

    * - 9:
      - **abs./rel.**
      - select. field
      - select whether the uncertainty from col. Sp. 6, 7 or 8
        are to be taken *absolute* or as *relative* value.

    * - 10:
      - **abs. std. Unc.**
      - number field
      - (combined) absolute standard uncertainty
        calculated by the program form the values of the columns 6, 7, 8 and 9;
        Note: a value entered by the user will always
        be over-written by the program!

The columns 1 to 3 have been taken from the preceding symbol list and
are disabled here.

For the user’s support **those cells in the table are colored with red
background which are not filled in by the user**: these are cells
belonging to dependent quantities. Only cells with white background
should be used for direct input by the user. That means, the rows
belonging to dependent quantities are found in the uppermost part of the
table, **the first row (row 1) is the one for the output quantity, or
the rows 1-3 in the case of three output quantities**.

Special feature of non-normal distributed numbers of counts and count
rates: `see <#treatment-of-numbers-of-counts-and-count-rates>`__.

For a data set-based quantity, mean and uncertainty are calculated
internally from the associated data set and transferred to the “values,
uncertainties” table with the button “Calculation of uncertainties”. The
corresponding rows in that table therefore have also a red colored
background.

The “white cells” in columns 4 to 9 must be filled in by the user.
**Numerical values must be input into the column “value”** while this is
not always necessary in columns 6 to 9 for each of the quantities. One
can often abstain from giving uncertainties, for instance for counting
times. The input of uncertainties is allowed in only one of the columns
6, 7 or 8. The selectable field “abs./rel.” however must be used, if in
the columns 6 to 8 an entry exists.

For counting rates or numbers of counts **the standard deviation can be
input as a formula** in column 6 (“StdDev formula”) - without preceding
equal sign. For **quantities declared as normal distributed** - also
possible for counting rates/numbers of counts - the numerical value of
the uncertainty is input in column 7 (“StdDev value”). In the case of
**rectangular and triangular distributed quantities** the uncertainty is
characterized by the value of the half-width in column 8. The latter
value is internally converted to a standard deviation (normal
distribution) according to the GUM rules which then is transferred to
the cell in column 10 (“abs. std.Unc.”).

**One cell** within the column “StdDev formula” **is** **highlighted by
green colour.** Here, the **standard deviation formula of the gross
counting rate** must be given. This formula is required later for the
numerical-iterative calculation of the Decision threshold and the
Detection limit. It is considered as **“uncertainty function” of the
gross counting rate (and implicitly of the (procedure dependent) net
counting rate)** **which allows estimating its uncertainty for any
varied (iterated) value of the gross counting rate.** It has, however,
no meaning for the calculation of the combined uncertainty of the output
quantity.

**Possible formulae of the standard deviation of the gross counting rate
Rg** (incomplete):

sqrt(Rg/tm) counting devices (counting time tm), single measurement

sqrt(Rg/tm/n) counting devices (counting time tm), n-times repeated
measurement

sqrt(Rg/2/tau) ratemeter measurements (time constant tau)

If in addition to the gross count rate Rg the **gross count number Ng**
is also used, e.g., by defining an equation Rg=Ng/tm, one should have in
mind that **the program also modifies Ng if Rg** **is modified**. From
this equation, Rg=Ng/tm, UncertRadio can identify the count number Ng
belonging to Rg. This ensures, that the program, during evaluating the
equations from the bottom upwards (Ng then is an independent quantity
and Rg is dependent), does not overwrite the Rg value and its
uncertainty primarily modified, by re-calculating Rg and its uncertainty
from Ng and its uncertainty, if the latter would not have been modified
also.

If the values of the **gross count rate Rg is obtained as a mean of
several individual values,** a linear interpolation is necessary between
its variance at measurement (u_Rgm^2) and the variance to be used for
calculating the decision threshold (u_R0^2). The formula for the
standard uncertainty may be expressed as:

.. math::
    :label: std_uncertainty

    \sqrt{u(R_{0})^{2} + (u(R_g)^2 – u(R_{0})^{2})*(R_{g} - R_{0}) / (R_{gm} – R_{0})}


Herein, Rg und Rgm are the modified (varied) and the measured value of
the gross count rate, respectively. If Rg takes the upper value, Rgm,
the result is sqrt(u_Rbm^2), while the result is sqrt(u_R0^2) for the
lower value Rg=R0. This requires appending the symbols Rgm, u_Rgm and
u_R0 to the symbol list (TAB “Equations”) of independent quantities.
They do not get an uncertainty and their values are to be given
explicitly in the “Values, uncertainties” table.

**Note:**
Since the program version 2.3.05 2020/01 it is no longer necessary that
the user supplies the equations :math:numref:`std_uncertainty` or a similar formula to UncertRadio.
The introduction of additional auxiliary quantities also can be omitted.
The chapter 6.9 generally informs how to supply datasets for calculating
means. Based on that, the chapter 6.12 describes the necessary
calculations and manipulations within the program for deriving an
extended version of equation (1).

The type of equation (1) for the uncertainty of the gross count
rate-related mean depends on how the mean of single values suffers by
additional random influences, which can be characterized as **unknown**
or **known**. According to these two options, different versions of Eq.
(1) are applied. This is also considered in chapter 6.12, where
corresponding example projects are also indicated.

It may happen with special measurement problems that the length of the
standard deviation formula of the gross counting rate is longer than the
visible part of that cell. For better editing, one can then copy this
formula by copy and paste (in the mouse context menu) from this cell to
the longer text cell (“\ **extra working cell for formulae editing**\ ”)
located above the table and back again.

Below the uncertainty table is located a smaller **table for the input
of covariances**. Its columns are as follows:

.. list-table::
    :widths: auto

    * - 1:
      - **Symbol A**
      - symbol list box for quantity A
    * - 2:
      - **Symbol A**
      - symbol list box for quantity B
    * - 3:
      - **Type**
      - list box for choosing input between covariance and
        correlation coefficient
    * - 4:
      - **Formula**
      - text field for defining the covariance as formula
        being a function of already defined symbols
    * - 5:
      - **(or) Value**
      - number field for input of the value of the
        covariance / correlation coefficient

In the columns 1 and 2 the symbols of correlated measured quantities are
selected. After having selected “covariance” in column 3 a formula for
the covariance may be entered in the column “Formula”. Otherwise, a
numerical value of the covariance / correlation coefficient can be
entered directly into the column “(or) Value”.

Relation between correlation coefficient **r** and covariance **cov**:

:math:`r(SymbolA,\ SymbolB) = \frac{cov(SymbolA,\ SymbolB)}{\sqrt{var(SymbolA) \bullet var(SymbolB)}}`

Having completed the input to the uncertainty and to the covariance
tables a mouse click on the **button “calculation of uncertainties”**
will initiate the following calculations:

• Note: All calculations within this TAB and the following refer to
  the actually selected output quantity, if more than one output
  quantities have been defined for the project. Under the menu item
  “Edit – Select output quantity” another output quantity may be
  selected;
• **Uncertainty table**: values of dependent quantities (red colored
  fields) are calculated and inserted into the corresponding cells;
  Formulae for standard deviations in the column “StdDev formula” are
  evaluated; standard uncertainties of independent quantities are
  evaluated into column “abs. std.Unc.“;
• **Covariance table:** evaluation of covariance formulae as
  numerical values into column “(or) Value”;

• **Uncertainty table:** now all variances/covariances are known for
  the complete uncertainty propagation; calculation of the standard
  uncertainties of the dependent quantities (a) below the output
  quantity and of the output quantity (row 1 in that table); the
  combined standard deviations of the dependent quantities (red
  colored) are exclusively calculated from the
  uncertainties/covariances of the independent quantities (white
  colored).

**Only after finalisation of these calculations the TAB “Uncertainty
budget” is enabled.**

With more complex measurement problems and a more slowly PC it may be
that the calculations take few seconds; it is indicated in the **status
bar segment at the lower right corner of the UncertRadio window** with
the entry “calculating…” that is still working. After termination of the
calculations the entry within this field will be “Ready!”.

**Note:** `Implication of changing parameters within the Options
menu <#implication-of-changes-within-the-options-menu>`__

**For working with tables**: see `Menu Edit -
Table <#within-tables-delete-rows-working-with-column-blocks>`__
Advice in case of problems
--------------------------

-  If the project runs though without any messages, but the results are
   not plausible, e.g. the detection limit iteration does not converge:

Then check at first whether the symbols for the net and gross counting
rates are correctly selected.

This means also, that always a separate equation must exist which
defines the net counting rate.

-  The detection limit iteration also starts working improperly if the
   relative uncertainty approaches a value of 61 % or even fails in case
   of more than 61 %. The reason for this behavior originates from ISO
   11929 itself.

-  Also, values in the table of the TAB "Values, uncertainties", which
   are equal to zero, lead to strange behavior. This may happen after
   re-editing the equations, where shifts between the lists of symbol
   names and values may occur. Thus, the latter should be checked for
   after one would have done significant re-editing of the equations.

-  *Symbols* which *became dispensable* after having edited the
   equations: it is referred to `section
   4.2 <#URH_Dispensable_Symbols_EN>`__ for their removal.

-  Calls to the special UR specific functions LINFIT or KALFIT should
   always be defined in separate equations; the closing brackets of
   these calls must not be followed by additional operators or symbols
   in the same equation.

-  For projects using linear unfolding (e.g., for combined
   Sr-89/Sr-90-analyes):

Since version 1.08 some modifications were applied to the dialog for
defining the model of a decay curve fitting. In the case of problems, it
is recommended therefore, to invoke also this dialog directly for
checking if selections/options are still the right ones.

-  A simple way in UR for initiating a complete re-calculation of the
   output values consists in selecting the current out output quantity
   again in the menu Edit - Output quantity.

-  The toolbar icon |view-refresh| initiates a complete
   re-calculation of characteristic values after having made changes to
   the model or the input data. If the program halts at the TAB "Values,
   Uncertainties", press the |view-refresh| icon once more.

-  Editing cells in tables:

one click into the cell row marks the row, the second click opens the
cell for editing. An entry into a cell must be confirmed with the ENTER
key; the TAB key is not sufficient.

-  Paste the Windows Clipboard into a table cell:

   Note that paste with the short-cut CTRL-V only works for the first
   time, but no longer thereafter;

   Thereafter, this works only correctly, after having marked the cell,
   by using the paste option from the associated context menu;

-  To optimize the column widths is possible by double-clicking the
   small vertical line between two column heads; the mouse pointer
   changes its shape shortly before double-clicking.

-  For importing column blocks from Excel or from Notepad++ into columns
   of a UR2 table: see the end of :numref:`within tables: delete rows, working with column blocks`.

-  Using the menu item Edit – Load missing values from project variant, values and
   uncertainties still missing in the actual project can be imported from another file
   variant of the project.

.. |view-refresh| image:: /icons/view-refresh.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

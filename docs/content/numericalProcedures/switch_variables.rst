Using switching variables in equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The function parser (fparser) implemented in UncertRadio allows to apply
**switching variables**, to which **only the two values 0 and 1 can be
attributed**. Such a variable allows to **activate another variable b**
with **b^1** or **to deactivate it by b^0**. In UncertRadio, these
variables can be declared by attaching the string "Trigger" to the
symbol name. They are, therefore, also called **"trigger variables"**.

Examples are: "min_Trigger", "kilo_Trigger"; with "60^min_Trigger" or
with "1000^kilo_Trigger" scaling factors of 60 (for minutes) or 1000 can
be switched; see chapter 2.2.7.

If a switching variable is to be used
for count rate variables, it must contain the part "Trigger" attached
to e.g. "min"; then they can be identified by the program which in turn
helps to prevent them from disturbing the process of finding such count
rates which directly contribute to the net count rate (see chapter 2.3).
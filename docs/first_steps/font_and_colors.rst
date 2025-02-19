Font and fontsize
-----------------

UR allows to change the font type and size.
The settings are stored in the :file:`Settings.ini` file:

.. code-block:: ini
    :caption: Settings.ini:
    :emphasize-lines: 3
    :name: settings.ini_ref

    [Settings]
    gtk-theme-name = win64
    gtk-font-name = Sans Normal 10

Note that the strings to the left of the = characters must not be
modified. On starting the UR program the :file:`Settings.ini` file is loaded.
This file shall be part of the UncertRadio.exe path.

For modifying values for *gtk-font-name* the **Fontname Icon** of the
toolbar can be used.

With the **fontname icon** |fontsize_icon| the font type and/or the font size
can be modified easily. It has to be considered that by enlarging the
font size the program’s window also increases.

With the dialog button "\ **Apply**\ " a selected fontname is applied to
the UR window. If the font is considered acceptable, it can be saved in
the file Settings.ini with the dialog button "\ **Save**\ "; this
however requires that this fontname had been applied once to the program
window. The new fontname will not be saved if this dialog is instead
closed by the button "\ **Quit**\ ".

.. |fontsize_icon| image:: /_static/icons/preferences-desktop-font.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link
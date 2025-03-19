Configuration
-------------

Since version 2.5, there is no further installation required for Windows.
Just download the archive, extract it and launch the
:file:`UncertRadio.exe` executable, located in the `/bin` directory.

Configuration file:
^^^^^^^^^^^^^^^^^^^

The `UR2_cfg.dat` file allows some simple adjustments:

+--------------------------------+--------------------------------------+
|| UncertRadio configuration     || Explanation                         |
|| keyword                       ||                                     |
+================================+======================================+
| [Path]                         | path section indicator               |
+--------------------------------+--------------------------------------+
|| Help_path=                    || path containing the help files      |
|| (default=UR2_CHM/)            ||                                     |
+--------------------------------+--------------------------------------+
|| log_path=                     || contains all log_files              |
|| (default=log/)                ||                                     |
+--------------------------------+--------------------------------------+
|| results_path=                 || contains all results                |
|| (default=results/)            ||                                     |
+--------------------------------+--------------------------------------+
|| example_path=                 || all included examples can be found  |
|| (default=pros/)               || in this directory                   |
+--------------------------------+--------------------------------------+
| [Local]                        | Localisation settings                |
+--------------------------------+--------------------------------------+
|| Decimal_point=                || set the desired decimal point       |
|| (possible values: ",", ".")   ||                                     |
+--------------------------------+--------------------------------------+
|| List_separator=;              || The character of the list separator |
|| (possible values: ";", ":")   || in csv files                        |
+--------------------------------+--------------------------------------+
|| Language=DE                   || Language of UncertRadio             |
|| (possible values: DE, EN, FR) ||                                     |
+--------------------------------+--------------------------------------+
|| Monitor#=1                    || select the desired Monitor          |
|| (possible values: 0, 1, 2)    ||                                     |
+--------------------------------+--------------------------------------+
|| ContrastMode=F                || The contrast mode of the program    |
|| (possible values: F, T)       || windows can be activated            |
+--------------------------------+--------------------------------------+

The entry "Monitor#=1" was introduced with Version 2.3.08; see the
paragraph "Using several monitors" at the end of this section.

After program start, the window can be enlarged.
However, with the next action, it turns back to the previous first
window size.


Country specific parameters
+++++++++++++++++++++++++++

The parameters found in the configuration file :file:`UR2_cfg.dat` under the
item [Local] may be defined as follows.

a)  one may omit the parameter values following the "=" character:

    Decimal_point=

    List_separator=

    Language=

    From this, the values for the decimal point character and the list
    separator character (for CSV files) are selected:

    DE or FR: comma as decimal point; semicolon as list separator character

    EN: point as decimal point; comma as list separator character

    note: The language which can be selected in the main menu - Options
    is only temporarily used, i.e., only within the actual program run.


b) If other specifications are desired, their parameter values can be
   inserted directly after the "="-sign in the configuration file. It
   may be sufficient to insert explicitly only the desired list
   separator character:

   List_separator=;

c) The language can also be selected in the dialog of the menu item
   "Options – Pre-settings" after the program has already been started
   (but only temporarily; see above). From the language defined there,
   the characters for decimal point and list separator are determined in
   the way as described above under a). Additionally, the list separator
   character can there be selected there explicitly.

d) For a graphical presentation the decimal point character is
   determined by the language shortcut DE, EN or FR


Choosing the language
+++++++++++++++++++++

The **user** of UncertRadio can be **guided** by the program through its
various dialogs **in the three language**. The user can choose
between German, English and French. This is controlled via the entry
'language=' in the above-mentioned configuration file :file:`UR2_cfg.dat`,
which is read at every program start. The parameter values are:

- DE for German,
- EN for English,
- FR for French.


The language can be switched within the running program with using the
dialog "Menu – Options – Pre-settings". Since **Version 2.3.07,** this
**includes also** **those "GTK stock buttons"**, labelled by an icon and
text, which previously were translated only after a new program start.
This has been achieved by converting all "stock buttons" into normal
buttons; the form of the button icons changed, but only slightly. For
the more complex GTK file chooser dialog, most items are translated, but
not all.


Font and fontsize
^^^^^^^^^^^^^^^^^

UR allows to change the font type and size.
The settings are stored in the :file:`Settings.ini` file:

.. code-block:: ini
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
the file :file:`Settings.ini` with the dialog button "\ **Save**\ "; this
however requires that this fontname had been applied once to the program
window. The new fontname will not be saved if this dialog is instead
closed by the button "\ **Quit**\ ".

.. |fontsize_icon| image:: /icons/preferences-desktop-font.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

Linking project files (.txp) to UncertRadio.exe
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Linking the file type .txp to ``UncertRadio.exe``, can be
established as follows in Windows:

-  highlight a txp file, click by the left mouse button "open with" and
   select "other app"

-  set the check mark and click on "further Apps"

-  scroll down the list of recommended applications

-  click on "search another App on this PC"

-  select the Uncertradio.exe


With the next double click on the txp filename the program should start
successfully.

Excel-controlled processing of UncertRadio
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Excel file :file:`UR2_SingleAutoRun_V12.xlsm` has changed compared
to that file used by the previous version by extending
the filenames to always include the
complete path. A further evaluation button "Start (autosep)" was added;
see :numref:`batch mode processing with an excel application`.
The Excel file can be used in the 32-bit as well as the 64-bit versions
of MS Office/Excel; see the notes in the introduction part of the VBA
module (Modul_Auto_single_UR) within the Excel file.

The VBA module of the Excel file now interprets an UncertRadio exit code
which has been introduced with version 2.2.03. An exit code equal to 3
(error occurred during the UncertRadio run) leads to an abortion of the
running VBA module.

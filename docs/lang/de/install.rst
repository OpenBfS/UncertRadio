Übersicht
=========
Die Software **UncertRadio** ermöglicht die automatisierte Berechnung der charakteristischen
Grenzen einer Aktivitätsbestimmung entsprechend
DIN ISO 11929. Im Detail werden die
Aktivitätskonzentration bzw. die spezifische Aktivität mit der dazugehörigen
kombinierten Standardmessunsicherheit, ihrem Unsicherheiten-Budget und den Werten
der Erkennungsgrenze und der Nachweisgrenze ermittelt. Die Unsicherheiten der
einzelnen Ergebnisgrößen werden nach ISO GUM mithilfe einer numerisch durchgeführten
Fortpflanzung der Unsicherheiten der Eingangsgrößen berechnet.

**UncertRadio** lässt sich für vielfältige Anwendungen
der Alpha-, Beta- und Gammamessung, aber auch der
Dosimetrie einsetzen. Die Software kann die charakteristischen Grenzen
simultan für bis zu drei Radionuklide zu ermitteln, deren Ergebniswerte,
z.B. Aktivitätsmesswerte, durch das Messverfahren bedingt voneinander abhängig sind.
Es ist auch für die Auswertung bei modernen Verfahren der Flüssigkeits-
szintillationsmessung von z.B. Strontium-Isotopen verwendbar.

Lizenz
-------
.. image:: /_static/en/about_window.png
    :alt: UncertRadio "About"-Window
    :align: center

**UncertRadio** ist Freie Software: Sie können es unter den Bedingungen
der GNU General Public License, wie von der Free Software Foundation,
Version 3 der Lizenz oder (nach Ihrer Wahl) jeder späteren
veröffentlichten Version, weiterverbreiten und/oder modifizieren.

**UncertRadio** wird in der Hoffnung, dass es nützlich sein wird, aber
OHNE JEDE GEWÄHRLEISTUNG, bereitgestellt; sogar ohne die implizite
Gewährleistung der MARKTFÄHIGKEIT oder EIGNUNG FÜR EINEN BESTIMMTEN ZWECK.
Siehe die GNU General Public License für weitere Details.

Sie sollten eine Kopie der GNU General Public License zusammen mit diesem
Programm erhalten haben. Wenn nicht, siehe <https://www.gnu.org/licenses/>.

Das Programm wurde vom Autor nach derzeitigem Stand von Wissenschaft,
Normung und Technik entwickelt und bezüglich der Richtigkeit der mathematischen Behandlung
der eingegebenen Modell-Gleichungen validiert.
Trotzdem wird vom Autor, vom TI und vom BMUV keine Gewährleistung für die Richtigkeit der damit vom Anwender
erzielten Ergebnisse gegeben und keine Haftung für daraus resultierende Ansprüche Dritter übernommen.


Download
--------
Die aktuelle Version wird auf der `GitHub Seite <https://github.com/OpenBfS/UncertRadio>`_
bereitgestellt.

Installation and Program start
------------------------------

Since version 2.5 UncertRadio can be compiled from the sources. Please consult the
`README file <https://github.com/OpenBfS/UncertRadio/blob/main/README.md>`_ for a detailed
compilation instruction.

Furthermore, Windows-Binarys are provided as well and there is no further installation necessary.
Just download the archive, extract it and launch the UncertRadio executable in the
`/bin` directory.

FAQ
---

**Configuration file UR2_cfg.dat:**

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

After program start, the window can be enlarged using the mouse,
however, with the next action, it turns back to the previous first
window size.

**Configuration file settings.ini**

[Settings]

gtk-theme-name = win64

gtk-font-name = Sans Normal 10

In this file related to the GTK3-GUI only the entry

gtk-font-name = Sans Normal 10

is allowed to be modified, which defines the font-type and its size.

**Country specific parameters**

The parameters found in the configuration file UR2_cfg.dat under the
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


**Choosing the language**

The **user** of UncertRadio can be **guided** by the program through its
various dialogs **in the necessary language**, which can be chosen
between German, English and French. This is controlled via the entry
'language=' in the above-mentioned configuration file UR2_cfg.dat, which
is read at every program start. The parameter values are:

DE for German,

EN for English,

FR for French.


The language can be switched within the running program with using the
dialog "Menu – Options – Pre-settings". Since **Version 2.3.07,** this
**includes also** **those "GTK stock buttons"**, labelled by an icon and
text, which previously were translated only after a new program start.
This has been achieved by converting all "stock buttons" into normal
buttons; the form of the button icons changed, but only slightly. For
the more complex GTK file chooser dialog, most items are translated, but
not all.

**Note about linking project files (.txp) to UncertRadio.exe**

Linking the file type .txp to UncertRadio.exe, can be established as follows in Windows:

-  highlight a txp file, click by the left mouse button "open with" and
   select "other app"

-  set the check mark and click on "further Apps"

-  scroll down the list of recommended applications

-  click on "search another App on this PC"

-  select the Uncertradio.exe


With the next double click on the txp filename the program should start
successfully.

**Note on the use of the Excel-controlled processing of UncertRadio**

The Excel file `UR2_SingleAutoRun_V12.xlsm` has changed compared to that file used by the previous version by extending
the filenames to always include the
complete path. A further evaluation button "Start (autosep)" was added;
see chapter 5.2. The treatment of VBA errors has been slightly improved.
The Excel file can be used in the 32-bit as well as the 64-bit versions
of MS Office/Excel; see the notes in the introduction part of the VBA
module (Modul_Auto_single_UR) within the Excel file.

The VBA module of the Excel file now interprets an UncertRadio exit code
which has been introduced with version 2.2.03. An exit code equal to 3
(error occurred during the UncertRadio run) leads to an abortion of the
running VBA module.


**UR Help and network drives**

The Windows Help file `UR25_HELP_EN.CHM` of the program cannot be used
from a network drive because of Windows safety reasons. Therefore, it is
recommended to install UR incl. its Windows Help on a local drive.
However, it would be sufficient to move only the CHM Help file to a
local drive. Within the configuration file UR2_cfg.dat its full pathname
can be defined by the entry Help_path=.

The following may also be recommended:

-  Open the context menu »Properties« by clicking on the CHM file with
   the RIGHT mouse button

-  open the tab »Safety«; therein, a text like » This file came from
   another computer and might be blocked to help protect this computer«
   is shown

-  click the button for allowing access.

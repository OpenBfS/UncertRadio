# UncertRadio
### Software zur Berechnung charakteristischer Grenzen nach ISO 11929 für Messungen der Aktivität
Die Software „UncertRadio“ ermöglicht die automatisierte Berechnung der charakteristischen
Grenzen einer Aktivitätsbestimmung entsprechend
DIN ISO 11929 auf einem Computer mit Windows-Betriebssystem. Im Detail werden die
Aktivitätskonzentration bzw. die spezifische Aktivität mit der dazugehörigen
kombinierten Standardmessunsicherheit, ihrem Unsicherheiten-Budget und den Werten
der Erkennungsgrenze und der Nachweisgrenze ermittelt. Die Unsicherheiten der
einzelnen Ergebnisgrößen werden nach ISO GUM mit
Hilfe einer numerisch durchgeführten Fortpflanzung der Unsicherheiten der Eingangsgrößen berechnet.

UncertRadio lässt sich für vielfältige Anwendungen
der Alpha-, Beta- und Gammamessung, aber auch der
Dosimetrie einsetzen. Die Software kann die charakteristischen Grenzen
simultan für bis zu drei Radionuklide zu ermitteln, deren Ergebniswerte,
z.B. Aktivitätsmesswerte, durch das Messverfahren bedingt voneinander abhängig sind.
Es ist auch für die Auswertung bei modernen Verfahren der Flüssigkeits-
szintillationsmessung von z.B. Strontium-Isotopen verwendbar.

![image](icons/UR2MC_EN.png)

Die Software unterscheidet zwei mögliche analytische Ansätze, die sich in den Gleichungen zur Auswertung unterscheiden:

- Verfahren ohne lineare Entfaltung: die Grundgleichung ist linear in nur einer (verfahrensbezogenen) Nettozählrate (Kanisch, 2016a),

- Verfahren mit linearer Entfaltung: die Gleichungen verwenden zusätzlich ein lineares
Least-squares-Verfahren für z.B. Abkling- oder Aufbaukurven mehrerer Zählraten (Kanisch, 2016b).

Alternativ kann die Auswertung für beide Varianten auch mit Hilfe der Monte Carlo-Simulation erfolgen.
Dies entspricht einer Unsicherheits-Fortpflanzung ganzer Verteilungen nach
GUM Supplements 1 und 2 und ist dann im Vorteil, wenn die Verteilung der Ergebnisgröße
deutlich von der einer Normalverteilung abweicht.

Das bedeutet allerdings auch, dass der Nutzer die erforderlichen Gleichungen
zur Auswertung formulieren können muss. Ein besonderer Vorteil ist jedoch, dass
keine partiellen Ableitungen einzugeben sind. Zum besseren Verständnis der
Datenhandhabung innerhalb der Software und der hinterlegten Gleichungen und
Funktionen ist eine umfangreiche Sammlung von Anwendungsbeispielen als Projektdateien beigefügt.

Viele der Anwendungsbeispiele stammen aus der Arbeitsgruppe "AK-SIGMA" des
"Fachverbandes für Strahlenschutz", den Messanleitungen der Leitstellen und
aus der Literatur. Diese Beispiele haben, ebenso wie die im Beiblatt 1 zur
DIN ISO 11929 (2014) und die in der neueren ISO 11929-4 genannten Beispiele,
wesentlich zur Validierung von UncertRadio beigetragen.

An dieser Stelle sei den Anwendern, vor allem aus den in der Überwachung
der Umweltradioaktivität nach AVV-IMIS tätigen Kreisen der Leitstellen
und Messstellen, gedankt. Sie haben durch ihre Rückmeldungen oder durch neue
Anforderungen wesentlich zur Weiterentwicklung von UncertRadio und seiner
praktischen Anwendbarkeit beigetragen.

Die aktuell bereitgestellte Version ist die 2.5.1.

Seit der Version 2.5.1 steht der Quellcode online zur Einsicht zur Verfügung und kann aus
den Quellen erstellt werden. Eine entsprechende Anleitung findet sich weiter unten.
Für Windows werden weiterhin vorkompilierte Pakete bereitgestellt. Diese bestehen
aus einem gepackten Archiv mit allen benötigten Datein. Dieses muss zur Nutzung nur
entpackt werden. Im Anschluss kann das Programm mit der "UncertRatio.exe" Datei im
Unterverzeichnis "bin/" gestartet werden.

Die Version 2.4.32 ist die letzte Version, die mit einem Windows-Installationsprogram
bereitgestellt wurde.

Seit Version 2.1.4 besteht der Download aus einer ausführbaren Installationsdatei.
Sie enthält alle benötigten Komponenten einschließlich einer Windows-Hilfe,
einer Kurzanleitung für die Installation, und eine Sammlung von Anwendungsbeispielen.
Zusätzlich kann eine Übersicht über den Aufbau der wichtigsten Programmdialoge
der Software heruntergeladen werden.

Seit der Version 1.08 (2013) kann UncertRadio zudem als Schnittstelle zwischen
der Software zur Erfassung der Messwerte und der Übergabe der charakteristischen
Werte in ein modernes Laborinformationssystem verwendet werden. Dazu wird das
csv-Format für den Daten-Import und –Export genutzt.

Der Autor des Programms ist Günter Kanisch. Ansprechpartner für Fragen und Anregungen
ist Dr. Marc-Oliver Aust von der "Leitstelle für Fisch und Fischereierzeugnisse,
Krustentiere, Schalentiere, Meereswasserpflanzen" im Thünen-Institut für Fischereiökologie.

**WICHTIGER HINWEIS:**

UncertRadio ist Freie Software: Sie können es unter den Bedingungen
der GNU General Public License, wie von der Free Software Foundation,
Version 3 der Lizenz oder (nach Ihrer Wahl) jeder späteren
veröffentlichten Version, weiterverbreiten und/oder modifizieren.

UncertRadio wird in der Hoffnung, dass es nützlich sein wird, aber
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


### Software for calculation of characteristic threshold values accordung to ISO 11929 for radioactivity measurements

The software „UncertRadio“ enables the automated calculation of characteristic
threshold values of activity mesurements according to ISO 11929.
These include the activity concentration or specific activity and its combined
standard measurement uncertainty, an uncertainty budget and values of
decision threshold and the detection limit.
The uncertainties of the single output values are calculated using
numerical error propagation according to ISO GUM.

UncertRadio can be used for a variety of applications from Alpha, Beta and
Gamma measurements, but also from dosimetry. It has the capability to derive
the characteristic values for up to three radionuclides simultaneously, whose
output quantity values, e.g. activity values, of which are dependent from each
other due to the measurement. Therefore, it is especially suited for modern
liquid scintillation measurement procedures of e.g. Strontium isotopes.

![image](icons/UR2MC_EN.png)

There are two main analytical approaches used within the software
differing by the equations for the evaluation:

- procedures without linear unfolding: the basic evaluation model is linear in
the net count rate; numerically applied propagation of uncertainty
values of input quantities (Kanisch, 2016a),

- procedures with linear unfolding: the model in addition includes linear least squares
procedures for fitting e.g. for decay or build-up curves (Kanisch, 2016b).


Alternatively, an evaluation by Monte Carlo simulation may be used within
both approaches. This represents the method of propagating whole distributions
and is in advantage in the case of significant deviations from the normal
distribution; see ISO GUM Supplements 1 and 2.

Applying UncertRadio means that the user should be able to formulate the
equations used for the calculating values of the output quantity. A special
advantage is, however, that no partial derivatives are to be supplied. For a
better understanding of the course of the software and of user-supplied
equations and functions an extensive bundle of application examples as project
files is included into the installation.

Many of the application examples are from the working group "AK-SIGMA" of the
“German-Suisse Association for Radiation Protection”, from Measuring
Instructions of the German Co-ordinating Offices and from the literature.
These examples including those of a German National Supplement 1 of DIN ISO
11929 (2014) as well as the examples from the more recent
standard ISO 11929-4 contributed to the validation of UncertRadio.

The author is grateful for the feedback and new requirements of the user-community,
especially from federal coordination centres and the laboratiories of the
German Federal States working after AVV-IMIS on monitoring of environmental
radioavitiy, and significantly improved the quality and usability of UncertRadio.

The actual version is 2.5.1.

Since version 2.5.1, the source code is available online and can be
created from the sources. The associated installation guide is given below.
Pre-compiled packages are still provided for Windows. These consist of a packed
archive containing all required files. After unpacking, the program can be started
with the "UncertRatio.exe" found within the "bin/" subdirectory.

Version 2.4.32 is the last version that can be installed with an installation program
for Windows.

Since version 2.1.4 (2017) the download consists of one executable file, which
combines all required components of teh Software (help-files, short installation
guide, collection of validated example projects). Additionally, a brief instruction
to use the software may be downloaded. The private or commercial use of the
software is free of charge.

Since version 1.08 (2013) UncertRadio can be used as an interface between
the software for acquiring measurement data and the transfer of characteristic
values into a modern laboratory information system. The csv format is used for
data import and export.

The program's author is Günter Kanisch. Contact person for questions and
suggestions is Dr. Marc-Oliver Aust from the “Federal co-ordinating Office
for fish and fishery products, crustaceans, mollusks and marine algae” in
the Thünen- Institute of Fisheries Ecology.

**Important Note:**

UncertRadio is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

UncertRadio is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with UncertRadio. If not, see <http://www.gnu.org/licenses/>.

The software was developed by the author following state-of-the-art of science,
standardization and technology and validated with respect to the correct mathematical
treatment of the model input equations of the evaluation model.
Nevertheless, no warranty is given for the correctness of results obtained by the user working with
UncertRadio, neither by the author nor by TI and BMUV, and no responsibility
is taken for emerging demands by any third party.

## How to cite
If you are using UncertRadio, please consider citing the following papers:

```
KANISCH, G.: Generalized evaluation of environmental radioactivity measurements with UncertRadio. Part I: Methods without linear unfolding.
Appl. Radiat. Isot. 110, 2016, 28–41
http://dx.doi.org/10.1016/j.apradiso.2015.12.003

KANISCH, G.: Generalized evaluation of environmental radioactivity measurements with UncertRadio. Part II: Methods with linear unfolding.
Appl. Radiat. Isot. 110, 2016, 74–86
http://dx.doi.org/10.1016/j.apradiso.2015.12.046
```

## How to compile UncertRadio for Windows
### Download and install MSYS2
https://www.msys2.org/

### Start the MSYS2 MINGW64 environment and update the system
```
pacman -Syuu
```
Restart the MSYS2 MINGW64 terminal if required

### Install required tools and programms
```
pacman -S git mingw-w64-x86_64-cmake mingw-w64-x86_64-toolchain mingw-w64-x86_64-gtk3 mingw-w64-x86_64-fgsl mingw-w64-x86_64-plplot mingw-w64-x86_64-wxwidgets3.2-msw
```

### Clone this repository
```
git clone https://gitea.florianober.ddnss.de/BMUV/UncertRadio.git
```

### Now it should be possible to build UncertRadio
```
cd UncertRadio
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
```

### Install and compress the directory to an archive (optional)
```
cmake --install build --prefix=UR2
tar -czvf UR2.tar.gz UR2
```

## To be done

- [x] carefully correct all (old) Licence informations
  - [x] add Licence for the included libraries
  - [x] add a Licence for UR2

- [x] translate the README to english

- [ ] add linux compilation instructions

- [ ] add a JOSS Paper draft (branch paper)

- [x] check all included examples ~~& create an automatic test~~

- [ ] create a sphinx documentation and migrate the (Windows-chm) help files (see sphinx branch)

- [ ] refactor and simplify the complete translation (gettext? -> not available for fortran)

- [ ] refactor the logging system (there are still unopened files)

- [ ] create an automatic building and upload system for Windows binaries

## Known issues

you tell us ;) -> please use the issue tab or create a pull request. We are grateful for every help. Please get involved.

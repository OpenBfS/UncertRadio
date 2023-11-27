# UncertRadio
# Software zur Berechnung charakteristischer Grenzen nach ISO 11929 für Messungen der Aktivität
Die Software „UncertRadio“ ermöglicht die auto-
matisierte Berechnung der charakteristischen
Grenzen einer Aktivitätsbestimmung entsprechend
DIN ISO 11929 auf einem Computer mit Windows-Betriebssystem. Im Detail werden die Aktivitäts-
konzentration bzw. die spezifische Aktivität mit der dazugehörigen kombinierten Standardmess-
unsicherheit, ihrem Unsicherheiten-Budget und den Werten der Erkennungsgrenze und der Nachweis-
grenze ermittelt. Die Unsicherheiten der einzelnen Ergebnisgrößen werden nach ISO GUM mit Hilfe einer numerisch durchgeführten Fortpflanzung der Unsicherheiten der Eingangsgrößen berechnet.

UncertRadio lässt sich für vielfältige Anwendungen
der Alpha-, Beta- und Gammamessung, aber auch der
Dosimetrie einsetzen. Die Software kann die charakteristischen Grenzen simultan für bis zu drei Radionuklide zu ermitteln, deren Ergebniswerte, z.B. Aktivitätsmesswerte, durch das Messverfahren bedingt voneinander abhängig sind. Es ist auch für die Auswertung bei modernen Verfahren der Flüssigkeits-
szintillationsmessung von z.B. Strontium-Isotopen verwendbar.

Die Software unterscheidet zwei mögliche analytische Ansätze, die sich in den Gleichungen zur Auswertung unterscheiden:

- Verfahren ohne lineare Entfaltung: die Grundgleichung ist linear in nur einer (verfahrensbezogenen) Nettozählrate (Kanisch, 2016a) ,

- Verfahren mit linearer Entfaltung: die Gleichungen verwenden zusätzlich ein lineares Least-squares-Verfahren für z. B. Abkling- oder Aufbaukurven mehrerer Zählraten (Kanisch, 2016b);

Alternativ kann die Auswertung für beide Varianten auch mit Hilfe der Monte Carlo-Simulation erfolgen. Dies entspricht einer Unsicherheits-Fortpflanzung ganzer Verteilungen nach GUM Supplements 1 und 2 und ist dann im Vorteil, wenn die Verteilung der Ergebnisgröße deutlich von der einer Normalverteilung abweicht.

Das bedeutet allerdings auch, dass der Nutzer die erforderlichen Gleichungen zur Auswertung formulieren können muss. Ein besonderer Vorteil ist jedoch, dass keine partiellen Ableitungen einzugeben sind. Zum besseren Verständnis der Datenhandhabung innerhalb der Software und der hinterlegten Gleichungen und Funktionen ist eine umfangreiche Sammlung von Anwendungsbeispielen als Projektdateien beigefügt.

Viele der Anwendungsbeispiele stammen aus der Arbeitsgruppe "AK-SIGMA" des "Fachverbandes für Strahlenschutz", den Messanleitungen der Leitstellen und aus der Literatur. Diese Beispiele haben ebenso wie die im Beiblatt 1 zur DIN ISO 11929 (2014) genannten, wesentlich zur Validierung von UncertRadio beigetragen.

An dieser Stelle sei den Anwendern, vor allem aus den in der Überwachung der Umweltradioaktivität nach AVV-IMIS tätigen Kreisen der Leitstellen und Messstellen, gedankt. Sie haben durch ihre Rückmeldungen oder durch neue Anforderungen wesentlich zur Weiterentwicklung von UncertRadio und seiner praktischen Anwendbarkeit beigetragen.

Die aktuell bereitgestellte Version ist die 2.5.1.

Seit der Version 1.08 (2013) kann UncertRadio zudem als Schnittstelle zwischen der Software zur Erfassung der Messwerte und der Übergabe der charakteristischen Werte in ein modernes Laborinformationssystem verwendet werden. Dazu wird das csv-Format für den Daten-Import und –Export genutzt.

Seit Version 2.1.4 besteht der Download aus einer ausführbaren Installationsdatei. Sie enthält alle benötigten Komponenten einschließlich einer Windows-Hilfe, einer Kurzanleitung für die Installation, und eine Sammlung von Anwendungsbeispielen. Zusätzlich kann eine Übersicht über den Aufbau der wichtigsten Programmdialoge der Software heruntergeladen werden.

Die Version 2.4.32 ist die letzte Version, die mit einem Windows-Installationsprogram bereitgestellt wurde.

Seit Version 2.5.1 steht der Quellcode zur Einsicht online zur verfügung. UncertRadio kann selber aus den Quellen erstellt werden. Eine entsprechende Anleitung findet sich weiter unten. Für Windows werden weiterhin vorkompilierte Pakete bereitgestellt. Diese bestehen aus einem gepackten Archiv mit allen benötigten Datein. Dieses muss zur Nutzung nur entpackt werden. Im Anschluss kann die "UncertRatio.exe" Datei im Unterverzeichnis "bin/" gestartet werden.

Der Autor des Programms ist Günter Kanisch. Ansprechpartner für Fragen und Anregungen ist Dr. Marc-Oliver Aust von der "Leitstelle für Fisch und Fischereierzeugnisse, Krustentiere, Schalentiere, Meereswasserpflanzen" im Thünen-Institut für Fischereiökologie.

**WICHTIGER HINWEIS:**
Das Programm wurde vom Autor nach derzeitigem Stand von Wissenschaft, Normung und Technik entwickelt und bezüglich der Richtigkeit der mathematischen Behandlung der eingegebenen Modell­gleichungen validiert. Trotzdem wird vom Autor, vom TI und vom BMUV keine Gewährleistung für die Richtigkeit der damit vom Anwender erzielten Ergebnisse gegeben und keine Haftung für daraus resultierende Ansprüche Dritter übernommen.


## How to compile UncertRadio for windows
### download and install MSYS2
https://www.msys2.org/

### start the MSYS2 MINGW64 environment and update the system
```
pacman -Syuu
```

### restart the terminal if required
### install required tools and programms
```
pacman -S git mingw-w64-x86_64-cmake mingw-w64-x86_64-toolchain mingw-w64-x86_64-gtk3 mingw-w64-x86_64-fgsl mingw-w64-x86_64-plplot mingw-w64-x86_64-wxwidgets3.2-msw
```

### pull this repository
```
git pull https://gitea.florianober.ddnss.de/florianober/UncertRadio.git
```

### now it should be possible to build UncertRadio
```
cd UncertRadio
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
```

### (optional) install and compress the directory to an archive
```
cmake --install build --prefix=UR2
tar -czvf UR2.tar.gz UR2
```
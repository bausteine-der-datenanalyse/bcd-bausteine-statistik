# TODO

Offene Punkte zu den Modulen (Skript, Aufgaben, Folien). Hintergrund und Details zur Umstellung des Skripts in
`umstellung-skript.md`.

- [ ] Kapitel 1 (`m-statistische-daten`): Aufgabe `aufgaben/10-organisation-von-daten.qmd` nutzt dasselbe Beispiel
  wie Skript und Folien (Länder, Fälle, Bevölkerung, vier Lösungen). Für die Aufgabe ein neues Beispiel nehmen,
  dazu `01-daten/beispiel-rohdaten.xlsx`, `beispiel-rohdaten-loesung.xlsx`, Kontrollwerte und Lösungsbild
  `l-sd-10.jpg` anpassen.
- [x] Committen in dieser Reihenfolge: zuerst dieses Repo (neue Module, `DESCRIPTION`, `.gitignore`) committen und
  pushen, dann im Hauptrepo den Submodul-Stand zusammen mit `lernpfad/skript/content.yml`,
  `lernpfad/skript/_quarto.yml`, `lernpfad/skript/literatur.qmd`, `DESCRIPTION`, `todo.md`, `CLAUDE.md`.
  Umgekehrt bricht `collect-content.R` in der CI ab, weil die Modulordner fehlen.
- [ ] Kapitel 7 (`m-wahrscheinlichkeit`): Bild `antoine-gombaud.png` („Chevalier de Méré“) prüfen, sieht aus wie
  ein zweites Porträt von Pascal.
- [ ] Kapitel 11 (`m-zuverlaessigkeit-von-tragwerken`): $E_d$ beim Zugstab prüfen. Alt stand
  $1.5 \cdot 21000 = 23737.4\,\mathrm{N}$, korrigiert auf $31500\,\mathrm{N}$ und $E_d/R_d = 1.47$. Falls ein
  anderer Faktor gemeint war, anpassen.
- [ ] Kapitel 10 (`m-zufallsvariablen`): Übersichtstabelle `@tbl-zufallsvariablen` (diskret/stetig) ist im PDF eng
  gesetzt, ansehen.
- [ ] Kapitel 12 (`m-schliessende-statistik`): Entscheiden, ob in drei Module/Kapitel aufteilen (Einführung,
  Schätzung von Parametern, Testen von Hypothesen), wie im alten Skript.
- [ ] Kapitel 8 (`m-kombinatorik`): Mathematica-Screenshot der exakten Geburtstags-Rechnung ggf. durch R ersetzen.
- [ ] Alle Kapitel: Ehemalige `tcolorbox`-Kästen, die keine Definition sind, stehen als Absatz mit fettem Anfang
  ohne Rahmen da (Produktsatz, Satz von Bayes, Urnenmodelle …). Ggf. einheitlich gestalten.
- [ ] Kapitel 7 (`m-wahrscheinlichkeit`): Häufigkeitstabelle der Reißzwecken hat im PDF eine leere Kopfzeile
  (`column_labels.hidden` wirkt nur im HTML).
- [ ] Vielleicht später: Würfel als Unicode-Zeichen ⚀–⚅ (U+2680–2685) statt Zahlenpaaren wie $(3,1)$
  darstellen. HTML geht direkt, für PDF muss in `_bcd-setup.tex` eine Schrift mit den Zeichen eingebunden werden
  (z. B. DejaVu Sans, Befehl wie `\wuerfel{⚂⚀}`), in Formeln in `\text{…}`. Betrifft
  `m-grundbegriffe-wahrscheinlichkeitsrechnung`, `m-wahrscheinlichkeit`, `m-kombinatorik` und
  `m-rechnen-mit-wahrscheinlichkeiten`.
- [ ] Alle Kapitel (Skripte): Die 54 `::: beispiel` auf Quartos Beispiel-Umgebung `::: {#exm-…}` mit Titel
  (`## …`) umstellen, analog zu den Definitionen (`#def-`). `tinh-notes` hat die eigene Umgebung `beispiel`
  nicht mehr (kein Balken in HTML und PDF), dafür bekommen `#exm-` (Balken links) und `#def-` (Kasten) dort ihr
  Aussehen. Vorher die Extension in `_extensions/matthiasbaitsch/tinh-notes` aktualisieren. Die meisten in
  `m-kenngroessen` (17) und `m-zufallsvariablen` (6). Tabellen in Beispielen haben im PDF keine untere Linie
  (longtable in einer Box), vermutlich auch mit `#exm-`.
- [ ] Kapitel 1 (`m-statistische-daten`): Tidy-Data-Tabellen wieder mit `gt` statt `tinytable` (Commit `9bd1ca4`
  zurücknehmen). `tab_tidydata()` in `01-daten/statistische-daten.R` wieder auf `gt(d) |> tab_options(...)`, im
  Skript (Lösung 4) `group_tt()` wieder durch `tab_spanner()` ersetzen. Wirkt auch auf die Folien. `gt` sieht in den
  Typst-Folien besser aus und ist kleiner (siehe `m-kenngroessen/folien`). Danach Folien rendern und prüfen, ob
  `tab_spanner` in Typst sauber aussieht.
- [ ] Kapitel 3 (`m-kenngroessen`): Im Skript am Anfang von „Lagemaße“ erklären, was ein Lagemaß ist und welche
  Eigenschaften es hat, wie auf der Folie „Lagemaß: Ein Maß für das Zentrum“ (analog zu den Streuungsmaßen).

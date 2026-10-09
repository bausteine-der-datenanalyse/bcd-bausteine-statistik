# TODO

Offene Punkte zur Umstellung des Skripts (Kapitel 6–12). Hintergrund und Details in `umstellung-skript.md`.

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

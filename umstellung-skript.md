# Umstellung LaTeX-Skript → Quarto (Kapitel 6–12)

Stand: 8. Oktober 2026. Quelle: `/Users/maba/sciebo/mathematik-fbb/mathematik-b/01-skript.old`.
Alle neuen Module liegen in `bausteine/bcd-bausteine-statistik`. Offene Punkte stehen in `todo.md`.

## Überblick

| Altes Kapitel                        | Modul                                         | Kapitel im Skript |
|--------------------------------------|-----------------------------------------------|-------------------|
| 06-grundbegriffe                     | `m-grundbegriffe-wahrscheinlichkeitsrechnung` | 7 (Teil II)       |
| 07-wahrscheinlichkeit                | `m-wahrscheinlichkeit`                        | 8 (Teil II)       |
| 08-kombinatorik                      | `m-kombinatorik`                              | 9 (Teil II)       |
| 09-rechnen-mit-wahrscheinlichkeiten  | `m-rechnen-mit-wahrscheinlichkeiten`          | 10 (Teil II)      |
| 10-zufallsvariablen                  | `m-zufallsvariablen`                          | 11 (Teil II)      |
| 11-zuverlaessigkeit-von-tragwerken   | `m-zuverlaessigkeit-von-tragwerken`           | 12 (Teil II)      |
| 12-schliessende-statistik            | `m-schliessende-statistik`                    | 13 (Teil III)     |

## Allgemeine Entscheidungen (alle Kapitel)

- **Text neben Bild** (`\mbildmittext`, `\mtextmitbild`, `minipage`) steht jetzt untereinander.
- **Kästen** (`tcolorbox`, `eigenschaften`): Wo es eine Definition ist, als `::: {#def-…}`, sonst als Absatz mit
  fettem Anfang ohne Rahmen (z. B. Produktsatz, Satz von Bayes, Urnenmodelle).
- **Würfel** (`\dice`, `\ddice`, `\ddicec`) als Zahlenpaare $(3,1)$; Unicode-Würfel siehe `todo.md`.
- **Reißzwecken-Symbole** (`\rzwa`, `\rzwb`) als $O$ (Spitze oben) und $U$ (Spitze auf dem Boden), in Kapitel 7
  einmal mit den Symbolen als kleine SVGs erklärt.
- **Vereinslogos** (Kapitel 6) durch ausgeschriebene Vereinsnamen ersetzt (Logos in Formeln nicht möglich,
  rechtlich heikel).
- **Bilder** als SVG (aus den `-eps-converted-to.pdf` bzw. PDFs per `pdf2svg`), Fotos als JPG/PNG.
- **Originale zum Bearbeiten**: Illustrator-EPS und Illustrator-PDFs in `00-bilder/orig/` des jeweiligen Moduls
  (wird von `collect-content.R` nicht kopiert, da nicht rekursiv).
- **R-Grafiken** aus dem vorhandenen Quellcode als Chunks im qmd, Daten/Hilfsfunktionen in `01-daten/<name>.R`.
  Nur kleine Modernisierungen (`|>`, `after_stat()`, `guide = "none"`), Plotgrößen angepasst, Einzelplots
  teils mit patchwork zusammengefasst.
- **Typos und Kommas** in allen Kapiteln stillschweigend korrigiert; inhaltliche Korrekturen siehe unten.

## Gesamtskript (`lernpfad/skript`)

- `_quarto.yml`: Teile „Beschreibende Statistik“, „Wahrscheinlichkeitstheorie“, „Schließende Statistik“.
- `literatur.qmd`: `{.unnumbered}` und `\bookmarksetup{startatroot}`. `\backmatter` geht nicht, da Dokumentklasse
  `scrreprt`.
- `DESCRIPTION` (Hauptrepo und Bausteine): `ggforce` ergänzt (für `darts.R`).

## Kapitel 6: Grundbegriffe der Wahrscheinlichkeitsrechnung

- Titel geändert: „Grundbegriffe“ → „Grundbegriffe der Wahrscheinlichkeitsrechnung“.
- Mengendiagramme in einer Abbildung mit Unterbildern (`@fig-mengen`, `@fig-mengen-teilmenge` usw.), aus dem Text
  referenziert.
- Diagonalargument als Formel (`array`) mit roter Diagonale nachgesetzt, Erklärtext darunter.
- Korrekturen: „Diagonalelement“ → „Diagonalargument“; kartesisches Produkt „für den zweiten 6“ → „2“; Menge im
  kartesischen Produkt mit geschweiften statt runden Klammern; „weniger als fünf Minuten“ → „höchstens“
  (passend zu $x \leq 12900$).

## Kapitel 7: Wahrscheinlichkeit

- Das Bild `antoine-gombaud.png` („Chevalier de Méré“) sieht aus wie ein zweites Porträt von Pascal
  (war im alten Skript schon so).
- Reißzwecken-Diagramm: Es gab kein R-Skript mehr. Aus der Wurfsequenz im LaTeX-Kommentar in R berechnet
  (Tabelle und Plot), Ergebnis identisch mit der alten Tabelle.
- Porträts als Abbildung mit Unterbildern, feste Höhe 42 mm wie im LaTeX.
- Axiomensystem von Kolmogoroff als Definition.
- Korrekturen: $f_6 = 3/5 = 0.6$ → $4/6 = 0.667$; (K3) $A_i \cup A_2$ → $A_1 \cup A_2$; Rechenregel 5
  $A_i \subset \mathcal{A}$ → $A_i \in \mathcal{A}$.
- Häufigkeitstabelle (`gt`): Im PDF bleibt eine leere Kopfzeile stehen (`column_labels.hidden` wirkt nur im HTML).

## Kapitel 8: Zufallsstichproben und Kombinatorik

- Geburtstags-Diagramm war Mathematica, trotzdem in R erzeugt (einfache Formel), Tabelle $n$ / $P(A)$ ebenfalls.
- Mathematica-Screenshot der exakten Rechnung als PNG belassen (Text verweist auf Mathematica).
- Mäxchen 36 → 21 als zwei Zahlenpaar-Felder mit $\leadsto$.
- Zusammenfassung als normale Tabelle.

## Kapitel 9: Rechnen mit Wahrscheinlichkeiten

- Dartscheibe mit 2500 Würfen aus `darts.R` (benötigt `ggforce`).
- `darts-a-b` war in Illustrator nachbearbeitet → als SVG übernommen.
- Korrekturen: „mit den HI-Virus“ → „dem“; „Stochastisch Unabhängig“ → „Stochastische Unabhängigkeit“;
  einheitlich „Triple“; fehlendes Komma in Menge $B$ (Mäxchen).

## Kapitel 10: Zufallsvariablen

- Alle R-Grafiken aus den Skripten in `00-pics-r/R` bzw. `darts.R`. Experiment-Werte identisch mit altem Skript.
- Hofkirchen-Daten als `01-daten/hofkirchen-6342800.day`: `collect-content.R` überschreibt keine vorhandenen
  Dateien in `c/01-daten`, und `6342800.day` gibt es schon in `m-statistische-daten`. Inhalt identisch (nur
  CRLF/LF).
- Übersicht diskret/stetig (farbige Kästen) als dreispaltige Tabelle `@tbl-zufallsvariablen`. Im PDF eng
  gesetzt.
- Zentraler Grenzwertsatz mit 5 Mio. Werten: `cache: true`.
- Normalverteilung: $\mu$, $\sigma^2$ der drei Spalten jetzt in der Bildunterschrift statt über den Plots.
- Fehler in den alten R-Skripten (Legenden) korrigiert:
  - Lognormal: `sd = 0.25` war mit „σ = 1/16“ beschriftet → „σ = 1/4“.
  - Weibull: Legenden für k = 0.5 und k = 1 waren vertauscht.
  - Student-t: alphabetische Sortierung („f20“ vor „f5“) → Legenden n = 5/n = 20 vertauscht; Spalten in
    `f01`, `f02`, `f05`, `f20` umbenannt.
  - Fisher: „m = 3, m = 100“ → „m = 3, n = 100“.
  - Hofkirchen: Kontrollzeile `sum(d$X) / ny` entfernt (`ny` undefiniert).
- Fehler im Text korrigiert:
  - $f(25) = 0.0046$ → $0.0074$ (auch in der Summenprobe $0.004$ → $0.0074$).
  - $\lim f(x) = 170/14450 = 1/86$ → $1/85$.
  - Stetige Zufallsvariable: $f(x) \leq 0$ → $f(x) \geq 0$.
  - Eigenschaften stetiger Verteilungsfunktion: $P(X \leq x) = 1 - F(a)$ → $P(X > x) = 1 - F(x)$; diskret
    $P(a < x \leq b)$ → $P(a < X \leq b)$.
  - Lineare Transformation: erste Formel $\operatorname{Var}(Z)$ → $\operatorname{E}(Z)$.
  - Student-Verteilung: $\operatorname{Var}(Z)$ → $\operatorname{Var}(T)$.
  - Weibull: „Verteilungsfunktion“ → „Dichte“; Bernoulli: „Verteilungsfunktion“ → „Wahrscheinlichkeitsfunktion“.
  - Hochwasser: $0.1 \cdot (1-0.9)^3 \cdot 0.1$ → $(1-0.1)^3$.
  - Selbstgebaute Verteilung: $-d \leq x \leq x$ → $\leq d$.
  - Poisson und geometrische Verteilung: Wertebereich „bis $n$“ → unbegrenzt.
  - „Cumulative density function“ → „Cumulative distribution function“.

## Kapitel 11: Zuverlässigkeit von Tragwerken

- Alt stand $E_d = 1.5 \cdot 21000 = 23737.4\,\mathrm{N}$ und $E_d/R_d = 1.1$. Korrigiert auf
  $31500\,\mathrm{N}$ und $1.47$ (Aussage „Nachweis nicht erfüllt“ bleibt).
- R-Anweisungen (`pnorm`, Monte-Carlo) jetzt ausführbare Chunks mit `echo: true`; Monte-Carlo-Ergebnis ändert sich
  bei jedem Rendern.
- Dichtekurven Zugstab (Mathematica/Illustrator) als SVG.
- `a_beton.svg` aus `lernpfad/skript/00-bilder` ins Modul kopiert; Schneefoto in `schneelast.jpg` umbenannt.
- Korrekturen: „dass für gegebene Werte“ → „das“; „$(x_1, \dots, x_n) < 0$“ → „$G(x_1, \dots, x_n) < 0$“;
  „Monte-Carlo Methode“ → „Monte-Carlo-Methode“.

## Kapitel 12: Schließende Statistik

- **Struktur:** Im alten Skript drei Kapitel (Einführung, Schätzung von Parametern, Testen von Hypothesen) als
  Teil III. Jetzt ein Modul/Kapitel „Schließende Statistik“ mit drei Abschnitten.
- Umfrage-Tabellen werden berechnet:
  - Alte Zeile „Anteil in Prozent“ der Grundgesamtheit war falsch (identisch mit Umfrage 3, Kopierfehler).
    Richtig bei 6900 Studierenden: 8.7 / 22.8 / 17.0 / 5.8 / 12.3 / 33.3 %.
  - Umfragewerte weichen vom alten Skript ab, weil `sample()` ab R 3.6 trotz `set.seed(1)` andere Stichproben
    liefert.
- Mittelwert-Experiment: `cache: true`.
- Korrekturen: $g : \mathbb{R} \to \mathbb{R}$ → $\mathbb{R}^n \to \mathbb{R}$; Standardfehler
  „$\sigma^2 = \sqrt{\operatorname{Var}(T)}$“ → $\sigma_T$; „mittlere Standardabweichung (MSE)“ → „mittlere
  quadratische Abweichung“; „Parameter $n$ zu wählen“ → $p$; $L(p) = 0$ → $L'(p) = 0$; „untersuchen den
  Erwartungswert $\overline{X}$“ → „den Mittelwert“; Bernoulli „Verteilungsfunktion“ →
  „Wahrscheinlichkeitsfunktion“.
- Verweis auf lineare Regression zeigt auf `@def-lineare-regression` (Abschnitt in `m-zwei-merkmale` hat kein
  Label).
- Abschnitt Intervallschätzung ist (wie im Original) nur ein Verweis auf die Literatur.

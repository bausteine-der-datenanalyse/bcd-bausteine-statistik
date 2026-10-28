# Formatierung qmd-Dokumente für Statistikbausteine

- Gleichung: Keine Leerzeilen vor und nach \$\$ ... \$\$

- Definitionslisten: Leerzeile zwischen Begriff und Erklärung

    ```qmd
    Begriff 1

    : Erklärung 1

    Begriff 2

    : Erklärung 2
    ```

    Sonst fügt Quarto `\tightlist` ein

- Leerzeilen bei Aufzählungslisten und nummerierten Listen, s.o.

- Beispiele wie Definitionen (`#def-`) mit Quartos Beispiel-Umgebung, der Titel ohne "Beispiel"

    ```qmd
    ::: {#exm-bezeichnung}
    ## Bezeichnung
    Text zum Beispiel
    ...
    :::
    ```

- Gendern: Neutrale Formen, wo es geht (Studierende, Angehörige, Befragte, Person), sonst Doppelpunkt
  (Absolvent:innen). Keine Paarformen wie „Student oder Studentin“.

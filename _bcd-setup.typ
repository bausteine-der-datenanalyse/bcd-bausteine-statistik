// Befehle, Gegenstück zu _bcd-setup.tex (Klassen per `commands:` im YAML)
#let neuerbegriff(body) = text(fill: rgb(177, 52, 52), weight: "bold", body)
#let ja(body) = text(fill: rgb("#2e7d32"))[✓]
#let nein(body) = text(fill: red)[✗]

// Vertikaler Abstand wie .downN in hsbo-maba, z. B. []{.down20}
#let down(n) = body => { v(n * 1pt); body }
#let down10 = down(10)
#let down20 = down(20)
#let down30 = down(30)
#let down40 = down(40)
#let down50 = down(50)

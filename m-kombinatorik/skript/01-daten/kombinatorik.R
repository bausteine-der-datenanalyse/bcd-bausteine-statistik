# -------------------------------------------------------------------------------------------------
# Geburtstagsparadoxon
# -------------------------------------------------------------------------------------------------

# Wahrscheinlichkeit für mindestens einen gemeinsamen Geburtstag bei n Personen
p_geburtstag <- Vectorize(function(n) 1 - prod((365 - 0:(n - 1)) / 365))

d_geburtstage <- tibble(
  n = 1:100,
  p = p_geburtstag(n)
)

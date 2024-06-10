tbl1 <- tribble(
  ~"Land",  ~"Jahr", ~"Typ", ~"Anzahl",
  "Italien", 1999, "Fälle", 745,
  "Italien", 1999, "Bevölkerung", 19987071,
  "Italien", 200, "Fälle", 2666,
  "Italien", 2000, "Bevölkerung", 20595360,
  "Brasilien", 1999, "Fälle", 37737,
  "Brasilien", 1999, "Bevölkerung", 172006362,
  "Brasilien", 2000, "Fälle", 80488,
  "Brasilien", 2000, "Bevölkerung", 174504898,
  "China", 1999, "Fälle", 212258,
  "China", 1999, "Bevölkerung", 1272915272,
  "China", 2000, "Fälle", 213766,
  "China", 2000, "Bevölkerung", 1280428583,
)

tbl2 <- tribble(
  ~"Land",  ~"Jahr", ~"Fälle", ~"Bevölkerung",
  "Italien", 1999, 745, 19987071,
  "Italien", 2000, 2666, 20595360,
  "Brasilien", 1999, 37737, 172006362,
  "Brasilien", 2000, 80488, 174504898,
  "China", 1999, 212258, 1272915272,
  "China", 2000, 213766, 1280428583,
)

tbl3 <- tribble(
  ~"Land",  ~"Jahr", ~"Anteile", 
  "Italien", 1999, "745/19987071",
  "Italien", 2000, "2666/20595360",
  "Brasilien", 1999, "37737/172006362",
  "Brasilien", 2000, "80488/174504898",
  "China", 1999, "212258/1272915272",
  "China", 2000, "213766/1280428583",
)

tbl41 <- tribble(
  ~"Land",  ~"1999", ~"2000", 
  "Italien", 745, 2666,
  "Brasilien", 37737, 80488,
  "China", 212258, 213766,
)

tbl42 <- tribble(
  ~"Land",  ~"1999", ~"2000", 
  "Italien", 19987071, 20595360,
  "Brasilien", 172006362, 174504898,
  "China", 1272915272, 1280428583,
)

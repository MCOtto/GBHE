CurrentYr <-
  2023
cYear <-
  c(2015, 2023)
nYear <-
  2L
cFrame <-
  c("List", "Area")
nFrame <-
  2L
cStr <-
  c("SC", "Co", "ND")
nStr <-
  3L
cYrStr <-
  c("2015.SC",
    "2015.Co",
    "2015.ND",
    "2023.SC",
    "2023.Co",
    "2023.ND")
nYrStr <-
  6L
cYFS <-
  c(
    "2015.List.SC",
    "2015.List.Co",
    "2015.List.ND",
    "2015.Area.SC",
    "2015.Area.Co",
    "2015.Area.ND",
    "2023.List.SC",
    "2023.List.Co",
    "2023.List.ND",
    "2023.Area.SC",
    "2023.Area.Co",
    "2023.Area.ND"
  )
nYFS <-
  12L
StrArea <-
  structure(
    list(
      Str_PK = c("SC", "Co", "ND"),
      cStratum = c("South Central", "Coastal", "North and Downeast"),
      KMSq = c(17602.905886034401, 6745.9621559483603, 59489.682796224297)
    ),
    class = "data.frame",
    row.names = c("SC", "Co", "ND")
  )
fStr <-
  structure(1:3, levels = c("SC", "Co", "ND"), class = "factor")
cSeat <-
  c("Front", "Rear", "Dirt")
cObsr <-
  c(
    "SB",
    "BC",
    "MC",
    "AD",
    "DD",
    "AF",
    "VH",
    "CK",
    "DK",
    "AM",
    "KM",
    "LM",
    "BP",
    "RP",
    "JR",
    "AS",
    "CS",
    "KS",
    "SS",
    "CT",
    "MT",
    "BCU",
    "CW",
    "BZ",
    "Gnd"
  )
nObsr <-
  25L

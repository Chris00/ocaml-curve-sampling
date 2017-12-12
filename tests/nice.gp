set terminal pdfcairo
set output "nice.pdf"

set grid
set title "Graph of a nice function"
plot "nice0.dat" with l lt 2, "nice.dat" with lp lt 1 pt 6 ps 0.5

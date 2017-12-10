set terminal pdfcairo
set output "test_nice.pdf"

set grid
set title "Graph of a nice function"
plot "test_nice0.dat" with l lt 2, "test_nice.dat" with lp lt 1 pt 6 ps 0.5

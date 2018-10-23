set terminal pdfcairo
set output "abs.pdf"
set grid

set title "40 evaluations"
plot "abs0.dat" with l lt 5, "abs.dat" with l lt 1

plot "abs0.dat" with l lt 5, "abs.dat" with p lt 1 pt 6 ps 0.5

set title "50 evaluations"
plot "abs1.dat" with l lt 5, "abs2.dat" with l lt 1
plot "abs1.dat" with l lt 5, "abs2.dat" with p lt 1 pt 6 ps 0.5

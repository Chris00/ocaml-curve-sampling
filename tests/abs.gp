set terminal pdfcairo
set output "abs.pdf"
set grid

plot "abs0.dat" with l lt 5, "abs.dat" with l lt 1

plot "abs0.dat" with l lt 5, "abs.dat" with p lt 1 pt 6 ps 0.5

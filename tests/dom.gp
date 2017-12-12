set terminal pdfcairo
set output "dom.pdf"
set grid

plot [-0.2:] "dom0.dat" with l lt 5, "dom.dat" with l lt 1

plot [-0.2:] "dom0.dat" with l lt 5, "dom.dat" with p lt 1 pt 6 ps 0.5

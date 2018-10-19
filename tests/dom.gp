set terminal pdfcairo
set output "dom.pdf"
set grid

plot [-0.2:] "dom0.dat" with l lt 5, "dom.dat" with l lt 1

plot [-0.2:] "dom0.dat" with l lt 5, "dom.dat" with p lt 1 pt 6 ps 0.5

plot [-0.2:] [0:1000] "dom1.dat" with l lt 5, "dom2.dat" with l lt 1

plot [-0.2:] [0:1000] "dom1.dat" with l lt 5, "dom2.dat" with p lt 1 pt 6 ps 0.5
plot [-0.2:] [0:1000] "dom1.dat" with l lt 5, "dom3.dat" with p lt 1 pt 6 ps 0.5

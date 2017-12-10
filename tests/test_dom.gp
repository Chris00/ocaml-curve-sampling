set terminal pdfcairo
set output "test_dom.pdf"
set grid

plot [-0.2:] "test_dom0.dat" with l lt 5, "test_dom.dat" with l lt 1

plot [-0.2:] "test_dom0.dat" with l lt 5, "test_dom.dat" with p lt 1 pt 6 ps 0.5

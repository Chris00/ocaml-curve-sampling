set terminal pdfcairo
set output "test_abs.pdf"
set grid

plot "test_abs0.dat" with l lt 5, "test_abs.dat" with l lt 1

plot "test_abs0.dat" with l lt 5, "test_abs.dat" with p lt 1 pt 6 ps 0.5

set terminal pdfcairo
set output "test_osc.pdf"

set grid
set title "Graph of x * sin(1/x)"
plot "test_osc0.dat" with filledcurves y1=0 lt 5, "test_osc.dat" with l lt 1

plot "test_osc0.dat" with filledcurves y1=0 lt 5, \
     "test_osc.dat" with p lt 1 pt 6 ps 0.5

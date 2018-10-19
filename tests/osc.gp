set terminal pdfcairo
set output "osc.pdf"

set grid
set title "Graph of x * sin(1/x)"
plot "osc0.dat" with filledcurves y1=0 lt 5, "osc.dat" with l lt 1

unset title
plot "osc0.dat" with filledcurves y1=0 lt 5, \
     "osc.dat" with p lt 1 pt 7 ps 0.15

set title "Graph of sin(1/x)"
plot "osc1.dat" with filledcurves y1=0 lt 5, "osc2.dat" with l lt 1
unset title
plot "osc1.dat" with filledcurves y1=0 lt 5, \
     "osc2.dat" with p lt 1 pt 7 ps 0.15

set title "Graph of sin on [-42π, 42π]"
plot "osc3.dat" with l lt 1
unset title
plot "osc3.dat" with filledcurves y1=0 lt 5, \
     "osc3.dat" with p lt 1 pt 7 ps 0.15

set terminal pdfcairo
set output "clip_gp.pdf"

set grid
set title "Path clipped to [0,1]²"
plot "clip0.dat" with l lt 1, "clip1.dat" with l lt 6 lw 3

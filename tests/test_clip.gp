set terminal pdfcairo
set output "test_clip.pdf"

set grid
set title "Path clipped to [0,1]²"
plot "test_clip0.dat" with l lt 1, "test_clip1.dat" with l lt 6 lw 3

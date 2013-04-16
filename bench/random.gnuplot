set terminal postscript portrait enhanced color dashed lw 1 "Helvetica" 14 
set output "random.eps"
set xlabel "Population"
set ylabel "Time (s)"
plot    "random.dat" using 1:2 title 'unsafe' with linespoints , \
            "random.dat" using 1:3 title 'new-safe' with linespoints   , \
            "random.dat" using 1:4 title 'old-safe' with linespoints
set output
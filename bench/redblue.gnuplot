set terminal postscript portrait enhanced color dashed lw 1 "Helvetica" 14 
set output "redblue.eps"
set xlabel "Population"
set ylabel "Time (s)"
plot    "redblue.dat" using 1:2 title 'netlogo-parsing' with linespoints , \
            "redblue.dat" using 1:3 title 'netlogo-jvm' with linespoints   , \
            "redblue.dat" using 1:4 title 'hlogo-1core' with linespoints   , \
            "redblue.dat" using 1:5 title 'hlogo-2cores-4threads' with linespoints

set output  
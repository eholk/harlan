set term pdfcairo font ",12"
set output "mandelbrot-sep-cmp.pdf"

set ylab "Execution time (ms)
set xlab "Iteration count"

set key left
set xtics 0,1000

plot 'mandelbrot-sep-gpu.dat' using 1:2 title "GPU", \
     'mandelbrot-sep-cpu.dat' using 1:2 title "CPU"

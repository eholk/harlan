
set ylab "Execution time (ms)
set xlab "Iteration count"

set key left

plot 'mandelbrot-hivequeen-gpu.dat' using 1:2 title "GPU"

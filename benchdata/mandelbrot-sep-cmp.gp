
set ylab "Execution time (µs)
set xlab "Iteration count"

set key left

plot 'mandelbrot-sep-gpu.dat' using 1:2 title "GPU", \
     'mandelbrot-sep-cpu.dat' using 1:2 title "CPU"

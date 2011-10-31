
set ylab "Execution time (µs)
set xlab "Vector size"

set key off

plot 'dot-prod-sep-gpu.dat' using 1:($2/1000)


set ylab "Execution time (µs)
set xlab "Vector size"

set key off

plot 'identity-sep-cpu.dat' using 1:($2/1000)

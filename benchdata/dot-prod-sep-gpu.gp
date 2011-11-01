set term pdfcairo font ",12"
set output "dot-prod-sep-gpu.pdf"

set ylab "Execution time (µs)
set xlab "Vector size"

set key off
set xtics 0,2500000

plot 'dot-prod-sep-gpu.dat' using 1:($2/1000)

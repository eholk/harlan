set term pdfcairo
set output "dot-prod-sep-cpu.pdf"

set ylab "Execution time (µs)
set xlab "Vector size"

set key off

plot 'dot-prod-sep-cpu.dat' using 1:($2/1000)
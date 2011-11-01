set term pdfcairo font ",12"
set output "add-vector-sep-gpu.pdf"

set ylab "Execution time (µs)
set xlab "Vector size"

set key off

plot 'add-vector-sep-gpu.dat' using 1:($2/1000)

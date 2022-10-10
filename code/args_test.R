#!/usr/bin/env Rscript

args <- R.utils::commandArgs(trailingOnly = TRUE, )

x <- as.integer(args[1])
y <- args[2] 
z <- args[3]

print(x)
print(y)
print(z)

if (z) {
  print("HALLO")
} else {
  print("BYE")
}
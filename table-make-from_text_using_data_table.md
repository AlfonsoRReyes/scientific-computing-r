---
title: "R Notebook"
output: html_notebook
---


```r
library(data.table)
fread("A,B
1,2
3,4
")
```


```r
fread("C D
100 200
300 400
")
```


```r
# separate the columns with consistent delimiters
# in this case we are pressing tabs that are replaced by spaces
table_text <- "
MD  TVD Pres    Temp
0   0   100.0   92.7
994.9   994.9   225.6   102.9
1989.7  1989.7  370.0   113.2
2238.5  2238.5  408.7   115.7
3233.3  3233.3  578.7   125.9
4228.2  4228.2  788.0   136.1
5223.1  5223.1  1037.2  146.2
5471.8  5471.8  1105.3  148.7
6217.9  6217.9  1323.9  156.0
7212.8  7212.8  1641.3  165.4
8207.7  8207.7  1968.0  173.5
9700.0  9700.0  2458.9  180.0
"
fread(table_text)
```


```r
# separate the columns with consistent delimiters
# in this case we are pressing tabs that are replaced by spaces
table_text <- "
MD, TVD, Pres, Temp
0, 0, 100.0, 92.7
994.9, 994.9, 225.6, 102.9
1989.7, 1989.7, 370.0,   113.2
2238.5, 2238.5,  408.7,   115.7
"
fread(table_text)
```

---
title: "R Notebook"
output: html_notebook
---


```r
write.table(read.table(text = "
 CHR_A     BP_A          SNP_A  CHR_B         BP_B          SNP_B           R2 
 1    154834183      rs1218582      1    154794318      rs9970364    0.0929391 
 1    154834183      rs1218582      1    154795033     rs56744813      0.10075 
 1    154834183      rs1218582      1    154797272     rs16836414     0.106455 
 1    154834183      rs1218582      1    154798550    rs200576863    0.0916789 
 1    154834183      rs1218582      1    154802379     rs11264270     0.176911 ", 
 sep="x"),
          "Type1.txt", col.names=FALSE, row.names=FALSE, quote=FALSE)  
```



```r
require(data.table) # v1.9.5+
ans <- fread("Type1.txt")
ans
```



```r
write.table(read.table(text="
CHR_A BP_A SNP_A CHR_B BP_B SNP_B R2
1 154834183 rs1218582 1 154794318 rs9970364 0.0929391
1 154834183 rs1218582 1 154795033 rs56744813 0.10075
1 154834183 rs1218582 1 154797272 rs16836414 0.106455
1 154834183 rs1218582 1 154798550 rs200576863 0.0916789
1 154834183 rs1218582 1 154802379 rs11264270 0.176911", sep=" "),
            "Type2.txt", col.names=FALSE, row.names=FALSE, quote=FALSE, sep="\t")
```


```r
require(data.table) # v1.9.5+
ans <- fread("Type2.txt")
ans
```
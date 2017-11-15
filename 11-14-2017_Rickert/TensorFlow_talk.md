TensorFlow_talk
========================================================
author: 
date: 
autosize: true

First Slide
========================================================

For more details on authoring R presentations please visit <https://support.rstudio.com/hc/en-us/articles/200486468>.

TensorFlow has emerged as the premiere machine learning platform for:
- Deep Learning
- Machine Learning
- Production Deployment

Slide With Code
========================================================


```r
library(keras)
mnist <- dataset_mnist()
str(mnist,give.attr = FALSE)
```

```
List of 2
 $ train:List of 2
  ..$ x: int [1:60000, 1:28, 1:28] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ y: int [1:60000(1d)] 5 0 4 1 9 2 1 3 1 4 ...
 $ test :List of 2
  ..$ x: int [1:10000, 1:28, 1:28] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ y: int [1:10000(1d)] 7 2 1 0 4 1 4 9 5 9 ...
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-2](TensorFlow_talk-figure/unnamed-chunk-2-1.png)

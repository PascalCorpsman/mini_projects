# Wave function collapse (Overlap model)

There are lots and good [explenations](https://discourse.processing.org/t/wave-collapse-function-algorithm-in-processing/12983) online available, which show what the algorithm is doing. But even with this i was not able to implement my own version.

![](preview1.png)
 
![](preview2.png)

So i searched github for a existing one, which i can translate into FreePascal and then improve to my own whishes. Finally i found [this](https://github.com/D-T-666/wave-function-collapse-p5). It has some easy to fix bugs (as it mirrors and somehow rotates the input image to the output only relevant, if symmetry flag is off).

Before diving into the code i highly recommend to read the explanations from above, otherwise this code is really hard to understand.
 
## How to use this program

1. Load a pattern image from the data folder
2. Set parameters


  | Parameter | Description |
  | --- | --- |
  | N | Affects the detailgrade of the subpatterns, typically you should set N somewhere between 2 and 7 (for the most sample images use 3)  
  |allow rotate | if set, the source pattern can "Rotate" freely, otherwise not (best to see with "demo-1.png")
  | floor | disable wrap vertically (automatically disables allow rotate) (test with "demo-flowers.png")
  | allow wrap | disables the N sampling to go "over" the edges of the image (ignores a N-Pixel wide range on the left and buttom side of the image)

3.  Hit int button and wait until initialization is finished (this can take quit long if N is large)
4.  Define output width and height (typically ~40, but the only limit is the RAM of your machine and the time you want to wait until it is finished)
5.  Hit run and enjoy image creation.


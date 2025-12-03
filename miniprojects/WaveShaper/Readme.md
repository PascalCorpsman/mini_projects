# WaveShaper

Inspired by this Youtube [Video](https://youtu.be/qeUAHHPt-LY?si=haVvvZ1xqHj8FCQ9) i tried to implement my own version of that "problem".

![](preview.png)

Features:
- shape sinewave by given image
- shape wave by given image

Dependencies:
- bass.pas, download it from [here](https://www.un4seen.com/)

Needed Lazarus packages:
- none

# What needs to be done to use it
Copy the binary from [release](https://github.com/PascalCorpsman/mini_projects/releases/tag/latest) for your operating system and rename it appropriately.

- Start the application
- Load Image (it should wider than taler)
- click "->" button to see the target wave shape that will be possible (ignore the red dots..)
- If everything is fine you can 
   1. Create a sinewave in the shape of to image or
   2. Load a different .wav file that will be reshaped to the new shape
- Preview the created wave file

# How to enable preview ?
By default the application does not use preview, because this is only possible, when using the bass.dll / basslib.so. This is done to make "testing" the software more easily, as you now can download the application and run it with no further configuration.

If you want to use sound preview you need to recompile the code with enabled {$Define UseBassSound} switch.

## Hidden features
Right click on the image "right" of the "->" button opens a save image dialog.
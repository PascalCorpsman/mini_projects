## How to setup .ini files

At best you start with an existing .ini file and modify all parameters as needed.

To be able to upload a hex file, all it takes is the avrdudename name.
avrdudename can be searched in this list:

http://www.nongnu.org/avrdude/user-manual/avrdude_4.html 

or you could run the command "avrdude -c usbasp" / "avrdude -c avrisp" this will show a list of all supported types.

All the other parameters can be read out of the according datasheet of the µ-controller.


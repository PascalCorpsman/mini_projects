# Usermanual for PixelEditor

This is the usermanual for Pixeleditor, mainly to show the "hidden" features like key combinations and other stuff.

## Installation

- Download the executable from the [bin](bin) subfolder and store it onto your harddrive
- create a subfolder named "GFX" where you stored the executable and copy all files from [GFX](GFX) into this folder
- [Linux users only], chmod +x the executable

Run the executable and enjoy ;).

## Mouse actions
| Action | Description |
| --- | ---
| wheel (up/down)| zoom (in/out)
| SHIFT + wheel (up/down)| change cursor size
| CTRL + wheel (up/down)| change tool shape
| CTRL + Left | Copies selection to image before drag
| Right | when clicked inside a selected area gives access to additional features (see below)
| double Right | access to background image dialog 

## Keyboard actions
The keyboard commands are mostly oriented on the aseprite program.

| Action | Description |
| --- | ---
| 4 | select "mirror horizontal and vertical" option, when mirror tool is active
| B | select "pencil" tool
| C | select toggle "mirror on pixeledge / pixelcenter" option, when mirror tool is active
| D | select "darken" tool
| E | select "eraser" tool
| F | select "filled" option, when rectangle / ellipse tool is active
| G | select "bucket" tool
| H | select toggle "mirror horizontal" option, when mirror tool is active
| I | select "pipette" tool
| L | select "line" tool
| M | select "select" tool
| O | select "outline" option, when rectangle / ellipse tool is active
| R | rotate image / selection 90° counter clock wise
| U | select "rectangle" tool
| V | select toggle "mirror vertical" option, when mirror tool is active
| X | toggle color/monochron view
| SHIFT | when pressed during line / rectangle / ellipse creation the shape will be forced to be equal in width / height
| SHIFT + D | select "brighten" tool
| SHIFT + H | horizontal flip image / selection
| SHIFT + M | select "mirror horizontal" option, when mirror tool is active
| SHIFT + U | select "ellipse" tool
| SHIFT + V | vertical flip image / selection
| CTRL + A | Select all
| CTRL + C | Copy selected / all
| CTRL + E | Open Resize dialog (for selected or image)
| CTRL + L | Open "Load dialog"
| CTRL + N | New image
| CTRL + O | show options dialog
| CTRL + S | Save / Save as
| CTRL + V | paste image from clipboard
| CTRL + Z | Undo last pixeloperation
| CTRL + ´ | toggle grid view
| Cursors | when selected a subimage "move" the image
| + / - | Zoom in / out
| DEL + BACK | clears "selected" part if something is selected
| ESC | skips actual selection / terminates application in error mode

## The color match dialog

The select by color and the floodfill (bucket) feature are dependant to the color match settings.

![colormatch](colormatch.png)

You can switch between "Exact match" and 1% .. 30%. This feature is espacially helpfull if you want to work with dithered images (like compressed JPEG).

## Select context menu

When clicking with the right mouse button into a selected subimage you get access to the additional features

![contextmenu](contextmenu.png)

| Name | Description |
| --- | ---
| Copy selection to clipboard | copies whatever is selected as bitmap into the clipboard
| Export selection | Exports whatever is selected as image
| Convert to grayscale | converts all pixels into grayscale values
| Invert colors | inverts the colors using the RGB-Cube
| Spritify | surrounds the selected image (using transparent to detect the border) with the selected color
| Select by color | selects inside the selection all pixels that match the selected color
| Invert selection | inverts the selection of the pixels inside the actual selection
| Hide | hides the contextmenu

## Edit background image menu

Menu is activated by double right click on the image area (without any selection)

![backgroundcontextmenu](backgroundmenu.png)

| Name | Description |
| --- | ---
| Load background image | Loads a arbiture image as "background" image, replacing the transparent pattern if enabled
| Clear background image | clears the background image

If set the background image will always be scaled to the image dimensions.


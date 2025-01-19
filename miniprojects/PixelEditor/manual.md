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
| CTRL + Left | Copies selection to image before drag
| Right | when clicked inside a selected area gives access to additional features (see below)
| double Right | access to background image dialog 

## Keyboard actions
| Action | Description |
| --- | ---
| CTRL + A | Select all
| CTRL + C | copy selected 
| CTRL + E | Open Resize dialog (for selected or image)
| CTRL + N | New image
| CTRL + O | show options dialog
| CTRL + S | Save / Save as
| CTRL + V | paste image from clipboard
| CTRL + Z | Undo last pixeloperation
| + / - | Zoom in / out
| DEL | clears "selected" part if something is selected
| ESC | skips actual selection / terminates application in error mode
| SHIFT | when pressed during line / rectangle / ellipse creation the shape will be forced to be equal in width / height


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

### The Color match dialog

The select by color and the floodfill (bucket) feature are dependant to the color match settings.

![colormatch](colormatch.png)

You can switch between "Exact match" and 1% .. 30%. This feature is espacially helpfull if you want to work with dithered images (like compressed JPEG).
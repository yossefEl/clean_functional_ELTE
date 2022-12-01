definition module Text.Drawille.Drawille

/**
 * Monochrome canvas for drawing with Braille characters on UTF-8 terminal.
 *
 *  (0, 0) pixel is in the top-left corner.
 *  x coordinate grows right.
 *  y coordinate grows down.
 */

:: PixelValue :== Bool

setPixel   :== True
unsetPixel :== False

/**
 * Canvas for drawing with Braille characters.
 * @var current X dimension of canvas
 * @var current Y dimension of canvas
 * @var current X dimension of internal bitmap
 * @var current Y dimension of internal bitmap
 * @var array, which contains the bitmap in rows
 */
:: Canvas = { size_x :: !Int, size_y :: !Int,
              real_size_x :: !Int, real_size_y :: !Int, data :: !.{#PixelValue}}

/**
 * Empty Canvas without any drawings. Also see {{`create`}}.
 */
empty :: .Canvas

/**
 * Create Canvas with given dimensions. Also see {{`empty`}}.
 * @param The number of columns
 * @param The number of rows
 * @result The blank canvas.
 */
create :: !Int !Int -> .Canvas

/**
 * Create Canvas from list of pixel coordinates. These pixels
 * will be set, canvas will be autoadjusted.
 * @param The number of columns
 * @param The number of rows
 * @result The blank canvas.
 */
fromList :: ![(Int, Int)] -> .Canvas

/**
 * Get the value of a pixel on canvas at given coordinates.
 *
 * @param The canvas
 * @param x coordinate of the pixel (column)
 * @param y coordinate of the pixel (row)
 *
 * @result The value of the pixel
 */
get :: !Canvas !Int !Int -> PixelValue

/**
 * Get the value of a pixel on canvas at given coordinates.
 * Unique version.
 *
 * @param The canvas
 * @param x coordinate of the pixel (column)
 * @param y coordinate of the pixel (row)
 *
 * @result The value of the pixel and unmodified canvas
 */
uget :: !u:Canvas !Int !Int -> *(PixelValue, v:Canvas), [u <= v]

/**
 * Set pixel with coordinates on Canvas.
 *
 * @param The canvas
 *
 * @param x coordinate of the pixel (column)
 * @param y coordinate of the pixel (row)
 *
 * @result Updated canvas
 */
set :: !*Canvas !Int !Int -> *Canvas

/**
 * Unset pixel with coordinates on Canvas.
 *
 * @param The canvas
 *
 * @param x coordinate of the pixel (column)
 * @param y coordinate of the pixel (row)
 *
 * @result Updated canvas
 */
unset :: !*Canvas !Int !Int -> *Canvas

/**
 * Toggle pixel with coordinates on Canvas.
 *
 * @param The canvas
 *
 * @param x coordinate of the pixel (column)
 * @param y coordinate of the pixel (row)
 *
 * @result Updated canvas
 */
toggle :: !*Canvas !Int !Int -> *Canvas

/**
 * Render canvas contents as a list of monochrome Braille
 * character strings (UTF-8 encoded).
 *
 * @param Canvas to render
 *
 * @result List of UTF-8 encoded strings
 */
frame :: !Canvas -> [String]

// Below are debug functions, used for testing.
/**
 * Braille character code to the list of coordinates
 *
 * @param Braille character code
 *
 * @result List of coordinates of "set" pixels
 */
brailleToList :: !Int -> [(Int, Int)]

/**
 * Recognise canvas contents as a bitmap of
 * Braille characters. Simple bit encoding based on `pixmap`
 * is used, character codes are in the range [0..255].
 *
 * @param Canvas to parse
 *
 * @result 2D array (first index - rows) of Braille chars
 */
toBrailleCodes :: !Canvas -> {*{#Int}}

/**
 * Debug print canvas.
 *
 * @param canvas to print.
 *
 * @result string, where each character represents canvas pixel.
 */
toDebugString :: !.Canvas -> String

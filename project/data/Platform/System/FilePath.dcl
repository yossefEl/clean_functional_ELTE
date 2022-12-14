definition module System.FilePath

/**
 * Module for manipulation of file and directory paths
 *
 * @property-bootstrap without default imports
 *     import Gast.Gen, Gast.GenLibTest, Gast.CommandLine
 *     from Gast.Testable import
 *         :: Testoption(..),
 *         :: PrintOption(..),
 *         class TestArg,
 *         instance Testable Property, instance Testable (a->b)
 *     from Gast.StdProperty import
 *         :: Property,
 *         class /\(..), instance /\ Bool Bool,
 *         class ==>(..), instance ==> Bool,
 *         location_and_name, possibleFailReasons, =.=
 *     import Testing.TestEvents, Text.GenPrint
 *     import StdEnv, Text
 *     import Data.Maybe
 *     import System.FilePath
 *
 *     :: GFilePath :== [GFilePathPart]
 *     :: GFilePathPart = PathSep | ExtSep | Name GName
 *     :: GName = A | B | C | D
 *
 *     derive genShow GFilePathPart, GName
 *     derive ggen GFilePathPart, GName
 *     derive gPrint GFilePathPart, GName
 *
 * @property-test-generator GFilePath -> FilePath
 *     gen gfp = concat (map toString gfp)
 *     where
 *         toString PathSep  = {pathSeparator}
 *         toString ExtSep   = {extSeparator}
 *         toString (Name A) = "a"
 *         toString (Name B) = "ab"
 *         toString (Name C) = "abc"
 *         toString (Name D) = "abcd"
 */

from Data.Error import :: MaybeError
from System.OSError import :: OSError, :: OSErrorCode, :: OSErrorMessage, :: MaybeOSError

:: FilePath :== String

/**
* Returns the default platform path separator
*/
pathSeparator :: Char

//* `pathSeparator` as a String.
pathSeparatorString :: String

/**
* Returns a list of all allowed platform path separators
*/
pathSeparators :: [Char]

/**
* Returns the default file extension separator
*/
extSeparator :: Char

//* `extSeparator` as a String.
extSeparatorString :: String

/**
* Concatenates two paths
*/
(</>) infixr 5 :: !FilePath !FilePath -> FilePath

//* Concatenate a list of paths.
concatPaths :: ![FilePath] -> FilePath

/**
 * Split a FilePath into filename and extension. The result does not include the extension separator (.).
 *
 * @property no separators in extension: A.fp :: FilePath:
 *     let
 *             (fname,ext`) = splitExtension fp
 *             ext = [c \\ c <-: ext`] in
 *         not (isMember extSeparator  ext) /\
 *         not (isMember pathSeparator ext)
 *
 * @property identity with addExtension: A.fp :: FilePath:
 *     let (fname,ext) = splitExtension fp in
 *         not (endsWith {extSeparator} fname) ==>
 *             addExtension fname ext =.= fp
 */
splitExtension :: !FilePath -> (String, String)

/**
* Take the extension of a FilePath, excluding the separator
*/
takeExtension :: !FilePath -> String

/**
* Remove the extension and extension separator of a FilePath
*/
dropExtension :: !FilePath -> String

/**
* Add an extension to a FilePath
*/
addExtension :: !FilePath !String -> FilePath

/**
* Infix version of addExtension
* @type !FilePath !String -> FilePath
*/
(<.>) infixr 6
(<.>) path ext :== addExtension path ext

/**
* Replace the extension of a FilePath
*/
replaceExtension :: !FilePath !String -> FilePath

/**
* Take the directory part of a FilePath. If the FilePath is a directory, 
* the result is the parent directory.
*/
takeDirectory :: !FilePath -> FilePath

/**
* Drop the directory part of a FilePath. Keep only the filename.
*/
dropDirectory :: !FilePath -> String

/**
* Split a filename into directory and file.
*/
splitFileName :: !FilePath -> (String, String)

/**
* Get the file name.
*/
takeFileName :: !FilePath -> FilePath

/**
* Set the filename.
*/
replaceFileName :: !FilePath !String -> FilePath

/**
* Drop the filename.
*/
dropFileName :: !FilePath -> FilePath

/**
 * Get the full path name, without '.', '..' or symbolic links.
 */
getFullPathName :: !FilePath !*World -> (!MaybeOSError FilePath, !*World)


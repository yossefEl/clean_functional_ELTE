definition module Clean.Parse.ModuleName

from Data.Error import :: MaybeError
from System.File import :: FileError
from System.FilePath import :: FilePath

/**
 * Guess the module name of a Clean file.
 *
 * @param The path to the Clean file
 * @result
 *   `Error`, if the file could not be read;
 *   `Ok ?None, if the module name could not be guessed;
 *   `Ok (?Just name)` in case of success.
 */
guessModuleName :: !FilePath !*World -> *(!MaybeError FileError (?String), !*World)

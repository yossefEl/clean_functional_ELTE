Directory - a Clean module to access and manipulate directories
---------------------------------------------------------------
  Version 1.1.1

Installation
------------

  This module is based on two other libraries:
   - "StdLib 1.0" (or higher) for the Date and Time types
   - "ExtendedArith 1.0" (or higher) for the BigInt type

  If you don't have these libraries already then download them from
  the Clean homepage (www.cs.kun.nl/~clean), install them, and set
  a path to each of these libraries. Also set a path to the Directory
  module. See the CleanIDE's documentation about setting paths.

  Use this module with Clean 1.3.3 or Clean 2.0 and the corresponding
  CleanIDE.
  mailto:clean@cs.kun.nl if a problem occurs.

Documentation
-------------
 
  There is a brief description of the Directory module at the Clean
  homepage (www.cs.kun.nl/~clean).

Differences to the previous version
-----------------------------------

  This version also works with network path names.

  The upgrade to version 1.1 is _not_ downward compatible. These are the
  incompatible changes (there are no other changes in the dcl module):

    - The Date` and Time` types of the previous version were replaced with
      the Date  and Time types of the StdLib. Now The Object I/O library
      and the Directory module share the same types for date and type
      information:

      previous version:
       :: Time` = { hours` :: !Int, minutes` :: !Int, seconds` :: !Int }
       :: Date` = { year` :: !Int, month` :: !Int, day` :: !Int, dayNr` :: !Int }

      this version:
       :: Time = { hours :: !Int, minutes :: !Int, seconds :: !Int }
       :: Date = { year :: !Int, month :: !Int, day :: !Int, dayNr :: !Int }

    - The file size is now stored in an arbitrary size integer (BigInt) and not
      in a 64 bit integer:

      previous version:
       :: PI_FileInfo = { fileSize :: !Integer64 ... 

      this version:
       :: PI_FileInfo = { fileSize :: !BigInt ... 

  Sorry for that inconvenience.


Contents of the package
------------------------
  Directory.dcl/icl                    - the module itself
  Clean System Files\cDirectory.obj    - necessary object file
  Clean System Files\directory_library - necessary import library
  README                               - this file
  Clean System Files\cDirectory.c      - C source used to build cDirectory.obj 
                                         (can be removed)

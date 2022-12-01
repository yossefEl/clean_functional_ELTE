module copyfile

/*
Commandline version of a file copying program.

Run the program using the "Basic Values Only" option.

*/

import StdEnv, StdFile

Start::*World -> *File
Start world =  fwrites "\nGoodbye.\n" stdinout`
where 
	(stdinout`,_)	= accFiles (CommandLoop stdinout) world`
	(stdinout ,world`)	= stdio world
		

CommandLoop::*File *Files -> (*File,*Files)
CommandLoop stdio files =  CommandLoop` stdio` files`
where 
	(files`,stdio`)	= Copy files stdio
		

CommandLoop`::*File *Files -> (*File,*Files)
CommandLoop` stdio files
	| answer<>'y' && answer<>'Y'	=  (stdio2,files)
									=  CommandLoop` stdio` files`
where 
	(files`,stdio`)	= Copy files stdio2
	answer         	= FirstChar answ
	(answ  ,stdio2)	= freadline stdio1
	stdio1         	= fwrites "\nCopy another file (y/n)? " stdio
		

Copy::*Files *File -> (*Files,*File)
Copy files io
	| source == dest	=  (files, fwrites "\nCopying succeeded.\n" io4)
						=  CopyFile (StripNewline source) (StripNewline dest) files io4
where 
	(dest,io4)		= freadline io3
	io3				= fwrites "\nDestination file: " io2
	(source,io2)	= freadline io1
	io1				= fwrites "\nSource file: " io

CopyFile::String String *Files *File -> (*Files,*File)
CopyFile source dest files io
	|	not sopen	= 	(files1,alert1)
	|	not dopen	= 	(files2,alert2)
	|	io_error	= 	(files4,alert3)
	|	not dclose	= 	(files4,alert4)
	|	not sclose	= 	(files4,alert5)
					= 	(files4,alert6)
where 
	(sclose,files4)         	= fclose sfile` files3
	(dclose,files3)         	= fclose dfile` files2
	(io_error,sfile`,dfile`)	= CopyFiles sfile dfile
	(dopen,dfile,files2)    	= fopen dest FWriteText files1
	(sopen,sfile,files1)    	= fopen source FReadData files

	alert1	= fwrites "\nCopying failed.\nSource file could not be opened.\n" io
	alert2	= fwrites "Copying failed.\nDestination file could not be opened.\n"  io
	alert3	= fwrites "Copying failed.\nFile I/O error.\n" io
	alert4	= fwrites "Copying failed.\nDestination file could not be closed.\n"  io
	alert5	= fwrites "Copying failed.\nSource file could not be closed.\n"  io
	alert6	= fwrites "\nCopying succeeded.\n" io
		

CopyFiles::*File *File -> (Bool, *File, *File)
CopyFiles source dest
	| srcend || wrterror	=  (wrterror,source1,dest1)
							=  CopyFiles source2 (fwritec byte dest1)
where 
	(_,byte,source2)	= freadc source1
	(srcend,source1)    	= fend source
	(wrterror,dest1)    	= ferror dest

StripNewline::String -> String
StripNewline "" =  ""
StripNewline str =  str % (0, size str - 2)

FirstChar::String -> Char
FirstChar "" =  ' '
FirstChar str =  str.[0]

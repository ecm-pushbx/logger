LOGGER.COM [ ON | OFF ] [ PRINT | ANSI | VIEW | CLEAR | MESSAGE text ]

	ON		Force logging on when finished.
	OFF		Leave logging disabled when finished.

	CLEAR		Empty the log. (leaves logging off)
	PRINT		Output the log text to STDOUT. (leaves logging off)
	ANSI		Just like PRINT, but includes ANSI color sequences.
	HTML		Just like PRINT, but generates very simple HTML.

	VERSION, VER	Display version information.
	HELP, ?		Display this help text.
	INFORMATION	Display some status information about the driver.
	QUIET		Do not display error when logging driver is not loaded.

	VIEW		View the log.
	MESSAGE text	Add the remaining command line as text to the log.
	SNAPSHOT	Store a snapshot of the screen in the log.
	THRU		Log and pass along any redirected standard input text.

most options can be abbreviated by using only their first letter (P for PRINT).
see LOGGER.TXT for more information.
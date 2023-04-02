LOGGER.COM [ ON ] [ PRINT | ANSI | VIEW | CLEAR | MSG text ]

Options to override or change the state of logging:

	ON		Force logging on when finished.
	OFF		Leave logging disabled when finished.

Options that turn leave logging disabled:

	CLEAR		Empties the current log
	PRINT		Outputs the log text to STDOUT.
	ANSI		Like PRINT, but includes ANSI color sequences.

Options that will return logging to its previous state:

	VERSION		Display version information.
	HELP		Display this help text.
	STATUS		Display some status information about the driver.
	VIEW		View the log.
	MESSAGE text	Add the remaining command line as text to the log.
	SNAPSHOT	Store a snapshot of the screen in the log.

LOGGER always disables logging when executed. Based on the options provided on
the command line, logging may or may not be enabled when the program finishes
interfacing with the device driver. The state of logging when finished can be
overriden by using the OFF and ON options.

Any redirected text waiting in the standard input buffer will automatically be
added to the Log. If there were not options specified, the interface program
will exit after appending the Log.

If no options are provided and there is no redirected text waiting, the default
behaviour is to VIEW the log.

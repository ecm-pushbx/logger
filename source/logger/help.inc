LOGGER.COM [ ON ] [ PRINT | ANSI | VIEW | CLEAR | MSG text ]

Options to override or change the state of logging:

	ON		Force logging on when finished.
	OFF		Leave logging disabled when finished.

Options that turn leave logging disabled:

	CLEAR		Empties the current log
	PRINT		Outputs the log text to STDOUT.
	ANSI		Like PRINT but includes ANSI color sequences.
	VIEW		View the log, and leave off. (not yet implemented)

Options that will return logging to its previous state:

	VERSION		Display version information.
	HELP		Display help text.
	STATUS		Display some status information about the driver.

	MSG text	Add the remaining command line as text to the log.
	STDIN		Add text from STDIN to the log.

LOGGER always disables logging when executed. Based on the options provided on
the command line, logging may or may not be enabled when the program finishes
interfacing with the device driver. The default behaviour can be overriden by
using the OFF and ON options.

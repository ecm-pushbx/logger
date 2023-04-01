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

LOGGER always disables logging when executed. Based on the options provided on
the command line, logging may or may not be enabled when the program finishes
interfacing with the device driver. That default behaviour can be overriden by
using the OFF and ON options.

Any text waiting standard input buffer, will automatically be added to the Log.

If no options are provided and there is no standard input waiting, the default
behaviour is to VIEW the log.

Possible upcoming option:

	SNAPSHOT	Store a snapshot of the screen in the log.
	HOTKEY key	Set/change a keyboard shortcut for taking snapshots.


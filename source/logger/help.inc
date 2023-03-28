LOGGER.COM [ ON ] [ PRINT | ANSI | VIEW | CLEAR | MSG text ]

	ON		Turns logging back on when finished.
	CLEAR		Empties the current log.
	PRINT		Outputs the log text to STDOUT. (default)
	ANSI		Outputs the log text (with color) to STDOUT.
	VIEW		View the log. (not yet implemented)
	MSG text	Add the remaining command line as text to the log.
	STDIN		Add text from STDIN to the log.
	STATUS		Display some status information about the driver.

Anytime LOGGER.COM is executed, it turns off message logging. It does not
turn the message logging back on unless the ON option is provided. The
exception to that is when LOGGER.COM adds messages to the log manually with
either the MSG or STDIN options. Those options return logging to its previous
OFF or ON state.
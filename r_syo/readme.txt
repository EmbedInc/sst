This front end uses the "old" syntaxer library SYO.  This library was
called SYN, but was renamed to SYO when the "new" SYN syntaxer library was
created.

The SST Pascal front end still calls the SYO library, and has the Pascal
syntax defined with a .SYO file instead of a .SYN file.  The old syntax
library must be kept around until the SST PAS front end is converted to
use the SYN library.

The only remaning .SYO files are the Pascal and SYO syntax definition
files.  These are in the SST/R_PAS and SST/R_SYO directories,
respectively.

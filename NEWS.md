adfExplorer 2.0.3
-------------

  * Plugged memory leaks and fixed undefined behaviour
  * Added `demo_adf()`
  * Improved test coverage
  * Fixed several minor bugs

adfExplorer 2.0.0
-------------

  * A complete rewrite of the package using the
    C library by Laurent Clévy, it can:
    * Connect to ADF and ADZ files
    * Create new virtual floppy disks (including file system)
    * Connect to files on virtual floppy disks
    * Move files between virtual and physical devices
    * Obtain information
      * On virtual device
      * On files and directories on the virtual device
    * List files and directories on virtual device
    * Create directories on virtual device
    * Remove files and directories from virtual device
  * Note that with the complete overhaul, a large set
    of functions and classes are deprecated and no longer in use

adfExplorer v0.1.8
-------------

 * added adf.file.info, adf.file.mode, adf.file.time
   and adf.file.size methods.

 * added dir.exists.adf and improved adf.file.exists
 
 * Corrections and updates in manual and vignettes
 
 * Added `pkgdown` website

adfExplorer v0.1.6
-------------

 * URL fix in vignette to pass CRAN checks

adfExplorer v0.1.5
-------------

 * Added 'remove.adf.file' method

 * Updated manual and vignette

 * Minor correction to manual

adfExplorer v0.1.4
-------------

 * Fix to pass CRAN checks.

adfExplorer v0.1.3
-------------

 * Minor corrections to the manual.

 * Directories can now be created on the virtual Amiga
   disk with the 'dir.create.adf' method.

 * with the newly added method 'put.adf.file' it is now
   possible to put a file onto a virtual Amiga disk.

 * Functions 'rawToBitmap' and 'bitmapToRaw' have been
   improved and are now exported and documented.

adfExplorer v0.1.2
-------------

 * Added adf.file.exists method.
 
 * Modified 'displayRawData' to convert more special
   characters to dots.

 * Minor adjustments in the manual

 * Vignette contained r-code output with non-supported
   characters. As a quick fix the display of this output
   now is suppressed in the vignette.

adfExplorer v0.1.1
-------------

First release:

 * This is the first release that has basic functionality to
   - Read and write adf files
   - Detect the presence of a file system (Amiga OS 3.x and earlier)
   - List files within the adf
   - Retrieve files from within adf files
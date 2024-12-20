adfExplorer v0.1.8 (Release date: 2024-03-06)
=============

 * added adf.file.info, adf.file.mode, adf.file.time
   and adf.file.size methods.

 * added dir.exists.adf and improved adf.file.exists
 
 * Corrections and updates in manual and vignettes
 
 * Added `pkgdown` website

adfExplorer v0.1.6 (Release date: 2021-09-05)
=============

 * URL fix in vignette to pass CRAN checks

adfExplorer v0.1.5 (Release date: 2021-09-04)
=============

 * Added 'remove.adf.file' method

 * Updated manual and vignette

 * Minor correction to manual

adfExplorer v0.1.4 (Release date: 2018-03-05)
=============

 * Fix to pass CRAN checks.

adfExplorer v0.1.3 (Release date: 2018-03-04)
=============

 * Minor corrections to the manual.

 * Directories can now be created on the virtual Amiga
   disk with the 'dir.create.adf' method.

 * with the newly added method 'put.adf.file' it is now
   possible to put a file onto a virtual Amiga disk.

 * Functions 'rawToBitmap' and 'bitmapToRaw' have been
   improved and are now exported and documented.

adfExplorer v0.1.2 (Release date: 2017-11-03)
=============

 * Added adf.file.exists method.
 
 * Modified 'displayRawData' to convert more special
   characters to dots.

 * Minor adjustments in the manual

 * Vignette contained r-code output with non-supported
   characters. As a quick fix the display of this output
   now is suppressed in the vignette.

adfExplorer v0.1.1 (Release date: 2017-10-28)
=============

First release:

 * This is the first release that has basic functionality to
   - Read and write adf files
   - Detect the presence of a file system (Amiga OS 3.x and earlier)
   - List files within the adf
   - Retrieve files from within adf files
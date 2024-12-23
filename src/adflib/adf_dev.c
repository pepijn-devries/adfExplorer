/*
 *  ADF Library. (C) 1997-2002 Laurent Clevy
 *
 *  adf_dev.c
 *
 *  $Id$
 *
 *  device code
 *
 *  This file is part of ADFLib.
 *
 *  ADFLib is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  ADFLib is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Foobar; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#include "adf_dev.h"

#include "adf_dev_dump.h"
#include "adf_dev_flop.h"
#include "adf_dev_hd.h"
// #include "adf_env.h"
#include "adf_nativ.h"

#include <stdlib.h>
#include <string.h>


/*
 * adfOpenDev
 *
 * open a device without mounting it, used by adfMountDev() or
 * for partitioning/formatting with adfCreateFlop/Hd
 *
 * Note that:
 * - an opened device must be closed with adfCloseDev()
 *   before mounting it with adfMountDev()
 *
 * WARNING: IT IS NOT CHECKING WHETHER THERE IS ANY EXISTING FILESYSTEM
 *          ON THE DEVICE, IT DOES NOT LOAD ROOTBLOCK ETC.
 *          IF UNSURE USE adfMountDev() FIRST TO CHECK IF FILESYSTEM STRUCTURES
 *          EXIST ALREADY ON THE DEVICE(!)
 */
struct AdfDevice * adfOpenDev ( const char * const filename,
                                const BOOL         ro )
{
    struct AdfDevice * dev = ( struct AdfDevice * )
        malloc ( sizeof ( struct AdfDevice ) );
    if ( ! dev ) {
        // (*adfEnv.eFct)("adfOpenDev : malloc error");
        return NULL;
    }

    dev->readOnly = ro;

    /* switch between dump files and real devices */
    // struct AdfNativeFunctions * nFct = adfEnv.nativeFct;
    //dev->isNativeDev = ( *nFct->adfIsDevNative )( filename );
    dev->isNativeDev = FALSE;
    /* Edit PdV Prevent uninitialized values as it results in unexpected behaviour */
    dev->size = 0;
    dev->nVol = 0;
    dev->volList = NULL;
    dev->cylinders = 0;
    dev->heads = 0;
    dev->sectors = 0;
    dev->fd = NULL;
    /* PdV end edit */
    
    RETCODE rc;
    // if ( dev->isNativeDev )
    //     rc = ( *nFct->adfInitDevice )( dev, filename, ro );
    // else
        rc = adfInitDumpDevice ( dev, filename, ro );
    if ( rc != RC_OK ) {
        // ( *adfEnv.eFct )( "adfOpenDev : device init error" );
        free ( dev );
        return NULL;
    }

    dev->devType = adfDevType ( dev );
    dev->nVol    = 0;
    dev->volList = NULL;

    /* PdV Not setting device format here will break adding volumes to disk! */
    if ( dev->devType == DEVTYPE_FLOPDD ) {
      dev->sectors = 11;
      dev->heads = 2;
    } else if ( dev->devType == DEVTYPE_FLOPHD ) {
      dev->sectors = 22;
      dev->heads = 2;
    } else if ( dev->devType == DEVTYPE_HARDFILE ) {
      dev->sectors = 1;
      dev->heads = 1;
    } else if ( dev->devType == DEVTYPE_HARDDISK ) {
      return NULL;
    } else {
      return NULL;
    }
    dev->cylinders = dev->size / ( dev->sectors * dev->heads * 512 );
    
    return dev;
}


/*
 * adfCloseDev
 *
 * Closes/releases an opened device.
 * Called by adfUnMountDev()
 */
void adfCloseDev ( struct AdfDevice * const dev )
{
    if ( ! dev )
        return;

    // free volume list
    //if ( dev->volList ) {
    if ( dev->nVol > 0 ) {
        for ( int i = 0 ; i < dev->nVol ; i++ ) {
            free ( dev->volList[i]->volName );
            free ( dev->volList[i] );
        }
        free ( dev->volList );
        dev->nVol = 0;
    }

    if ( dev->isNativeDev ) {
        // struct AdfNativeFunctions * const nFct = adfEnv.nativeFct;
        // ( *nFct->adfReleaseDevice )( dev );
    } else
        adfReleaseDumpDevice ( dev );

    free ( dev );
}


/*
 * adfDevType
 *
 * returns the type of a device
 * only based of the field 'dev->size'
 */
int adfDevType ( struct AdfDevice * dev )
{
    if( (dev->size==512*11*2*80) ||		/* BV */
        (dev->size==512*11*2*81) ||		/* BV */
        (dev->size==512*11*2*82) || 	/* BV */
        (dev->size==512*11*2*83) )		/* BV */
        return(DEVTYPE_FLOPDD);
    else if (dev->size==512*22*2*80)
        return(DEVTYPE_FLOPHD);
    else if (dev->size>512*22*2*80)
        return(DEVTYPE_HARDDISK);
    else {
        //(*adfEnv.eFct)("adfDevType : unknown device type"); /* Edit PdV */
        return(-1);
    }
}


/*
 * adfDeviceInfo
 *
 * display information about the device and its volumes
 * for demonstration purpose only since the output is stdout !
 *
 * can be used before adfCreateVol() or adfMount()
 */
void adfDeviceInfo ( struct AdfDevice * dev )
{
  /* Edit PdV */
  /* 
   const char * devTypeInfo = NULL;
    switch ( dev->devType ) {
    case DEVTYPE_FLOPDD:
        devTypeInfo = "floppy dd";
        break;
    case DEVTYPE_FLOPHD:
        devTypeInfo = "floppy hd";
        break;
    case DEVTYPE_HARDDISK:
        devTypeInfo = "harddisk";
        break;
    case DEVTYPE_HARDFILE:
        devTypeInfo = "hardfile";
        break;
    default:
        devTypeInfo = "unknown device type!";
    }

    printf ( "\nADF device info:\n  Type:\t\t%s, %s\n", devTypeInfo,
             dev->isNativeDev ? "real (native device!)" : "file (image)" );

    printf ( "  Geometry:\n"
             "    Cylinders\t%d\n"
             "    Heads\t%d\n"
             "    Sectors\t%d\n\n",
             dev->cylinders, dev->heads, dev->sectors );

    printf ( "  Volumes (%d):\n"
             "   idx  first bl.     last bl.    name\n",
             dev->nVol );

    for ( int i = 0 ; i < dev->nVol ; i++ ) {
        if ( dev->volList[i]->volName )
            printf("    %2d    %7d      %7d    \"%s\"", i,
                   dev->volList[i]->firstBlock,
                   dev->volList[i]->lastBlock,
                   dev->volList[i]->volName);
        else
            printf("    %2d    %7d      %7d\n", i,
                   dev->volList[i]->firstBlock,
                   dev->volList[i]->lastBlock);
        if ( dev->volList[i]->mounted )
            printf("    mounted");
        putchar('\n');
    }
    putchar('\n');
    */
}


/*
 * adfMountDev
 *
 * mount a dump file (.adf) or a real device (uses adf_nativ.c and .h)
 *
 * adfInitDevice() must fill dev->size !
 */
struct AdfDevice * adfMountDev ( const char * const filename,
                                 const BOOL         ro )
{
    RETCODE rc;
    uint8_t buf[512];

    struct AdfDevice * dev = adfOpenDev ( filename, ro );
    if ( ! dev ) {
        //(*adfEnv.eFct)("adfMountDev : malloc error");
        return NULL;
    }

    switch( dev->devType ) {

    case DEVTYPE_FLOPDD:
    case DEVTYPE_FLOPHD:
        if ( adfMountFlop ( dev ) != RC_OK ) {
            adfCloseDev ( dev );
            return NULL;
        }
        break;

    case DEVTYPE_HARDDISK:
        rc = adfReadBlockDev ( dev, 0, 512, buf );

       /* BV ...from here*/
        if( rc != RC_OK ) {
            adfCloseDev ( dev );
            //(*adfEnv.eFct)("adfMountDev : adfReadDumpSector failed"); /*Edit PdV*/
            return NULL;
        }

        /* a file with the first three bytes equal to 'DOS' */
    	if (!dev->isNativeDev && strncmp("DOS",(char*)buf,3)==0) {
            if ( adfMountHdFile ( dev ) != RC_OK ) {
                adfCloseDev ( dev );
                return NULL;
            }
        }
        else if ( adfMountHd ( dev ) != RC_OK ) {
            adfCloseDev ( dev );
            return NULL;								/* BV ...to here.*/
        }
	    break;

    default:
        //(*adfEnv.eFct)("adfMountDev : unknown device type"); /*Edit PdV*/
        adfCloseDev ( dev );
        return NULL;								/* BV */
    }

	return dev;
}


/*
 * adfUnMountDev
 *
 */
void adfUnMountDev ( struct AdfDevice * const dev )
{
    adfCloseDev ( dev );
}


RETCODE adfReadBlockDev ( struct AdfDevice * const dev,
                          const uint32_t           pSect,
                          const uint32_t           size,
                          uint8_t * const          buf )
{
    RETCODE rc;

/*printf("pSect R =%ld\n",pSect);*/
    if ( dev->isNativeDev ) {
      return RC_ERROR;
        // struct AdfNativeFunctions * const nFct = adfEnv.nativeFct;
        // rc = (*nFct->adfNativeReadSector)( dev, pSect, size, buf );
    } else
        rc = adfReadDumpSector( dev, pSect, size, buf );
/*printf("rc=%ld\n",rc);*/
    return rc;
}


RETCODE adfWriteBlockDev ( struct AdfDevice * const dev,
                           const uint32_t           pSect,
                           const uint32_t           size,
                           const uint8_t * const    buf )
{
    RETCODE rc;

/*printf("nativ=%d\n",dev->isNativeDev);*/
    if ( dev->isNativeDev ) {
      return RC_ERROR;
        // struct AdfNativeFunctions * const nFct = adfEnv.nativeFct;
        // rc = (*nFct->adfNativeWriteSector)( dev, pSect, size, buf );
    } else
        rc = adfWriteDumpSector ( dev, pSect, size, buf );

    return rc;
}

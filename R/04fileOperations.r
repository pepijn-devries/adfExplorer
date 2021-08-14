# How to rename an entry ?
# 
# 1. Compute the new hashvalue
# 2. Move the first sector pointer from the old hashvalue index to the new one
# 3. Change the name in the directory or file header block
# 4. Recalculate and update checksums in the blocks above.

############################################################

# Bitmap related
# 
# * Bitmap allocation starts at root block, upto highest block. The next allocated blocks are located just after boot blocks and finally the last allocated block is the sector before root block.
# root -> max -> boot+1 -> root-1
# 
# -> means "followed on disk by"
# 
# If you free some blocks by deleting a file, for example, the first next used block will be the first free block encountered starting from the Rootblock. The just freed blocks will be reused. It means that when you delete a file and you want to recover it, don't write anything else to the disk.
# This strategy must have been chosen to minimize fragmentation.

############################################################
# * The order in which data and file extension blocks for a given file are written on disk differs with OFS and FFS.
# OFS & FFS : All the data blocks of the file header block are written first.
# FFS : Then follow all the file extension blocks of the file, then all the remaining data blocks are written.
# OFS : Each file extension block is followed by the related data blocks. So the last extension block is followed by the remaining data blocks.
# 
# OFS:
#   header -> data blocks -> ext. block -> data blocks -> ext. block -> data blocks
# FFS:
#   header -> data blocks -> all ext. block -> all remaining data blocks
# 
# -> means "followed on disk by"
# 
# This difference is probably the main reason why FFS is faster then OFS.
# 
# Under FFS, the hash chains are sorted by block number.
# 
# Comparison chart of the ADF logical blocks
# 
# root  dir 	fileh 	hlink 	slink 	fext	data 	dirc
# ----------------------------------------------------------------------------------------
#   0/ 0x00 1st_type    2 	  2 	2 	2 	2 	16	8 	33
# 4/ 0x04 header_key  / 	  x 	x 	x 	x 	x 	x 	x
# 8/ 0x08  	    / 	  / 	nb_blo	/ 	/ 	nb_blo 	block# 	PARENT
# 12/ 0x0c table_size  72 	  / 	/ 	/ 	/ 	/ 	nb_data nb_rec
# 16/ 0x10 list 	    / 	  / 	data#1 	/ 	/ 	/ 	next 	next
# 20/ 0x14 chksum 	    x 	  x 	x 	x 	x 	x 	x 	x
# 24/ 0x18 table 	    ht 	  ht 	blocks 	/ 	/ 	blocks  data 	records
# ...
# BSIZE-184/-0xb8	comment_len /	  x 	x 	/ 	/ 	/ 	/ 	/
#   BSIZE-183/-0xb7 comment     /	  x 	x 	/ 	/ 	/ 	/ 	/
#   ...
# BSIZE- 92/-0x5c	days 	    x	  x	x 	x 	x 	/ 	/ 	/
#   BSIZE- 88/-0x58 mins 	    x	  x	x 	x 	x 	/ 	/ 	/
#   BSIZE- 84/-0x54 ticks 	    x	  x	x 	x 	x 	/ 	/ 	/
#   BSIZE- 80/-0x50	name_len    x 	  x 	x 	x 	x 	/ 	/ 	/
#   BSIZE- 79/-0x4f name 	    x 	  x 	x 	x 	x 	/ 	/ 	/
#   ...
# BSIZE- 16/-0x10	hash_chain  / 	  x 	x 			/ 	/ 	/
#   BSIZE- 12/-0x0c	parent	    / 	  x 	x 	x 	x 	fhdr 	/ 	/
#   BSIZE-  8/-0x08	extension   cache cache	fext    / 	/ 	next 	/ 	/
#   BSIZE-  4/-0x04	2nd_type    1 	  2 	-3	-4/4 	3 	-3 	/ 	/
#   ----------------------------------------------------------------------------------------
#   
#   type of blocks :
#   root=rootblock,  dir=directory,  fileh=file header,  fext=file extension,
# hlink=hard link,  slink=soft link,  dirc=directory cache,  data=OFS data.
# 
# special values :
#   /=unused
# x=used
# next=next block of same type
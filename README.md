#Siege of Avalon : Open Source#

Siege of Avalon : Open Source is an attempt to keep this great isometric RPG game alive by continuing development on it. Our aim is to create a SDL version of the engine that will work on Win32, Linux and where ever Pascal compilers and SDL are supported.

This is a fork from the CartBlanche repository with an eye on getting things "fixed" enough to keep to the original thinking of the game since the entire episode set is available on GoG for download.  The other reason for the "fork" is to provide a sort of "safe" spot to grab the codebase under the PROPER license of LGPL 2.0 as indicated in the headers from the SourceForge bit-rotted CVS drop.

#Status as of Jan 10th, 2017#

Fixed the build .sh script to build right, removed non-assets from the assets pile, properly RE-licensed the repository, and fixed a most of cowpiling to compile instead.  Fixed the missing unit with a FIXME type hack of just simply removing a bunch of junk old crufty code for process execution for the BinkPlayer...with the sysutils function that does ALL of what they were trying to do there. (Sigh...that was in there at the time that this was "converted" to build for FPC.) Even then, we're unlikely to get it "up" yet.  The whole thing relied on Jedi-SDL, which was based off of SDL 1.x, has suffered quite a bit of bitrot (which was part of the cause for the cowpiling).  Honestly, this should've been finished _*AGES*_ ago because it should've been a matter of re-wiring effects libs, etc. to a cross-platform solution, etc.  I'll refrain from snark here and just try to see if I can manage to get this back living again as a putzing project to get me back sharp with everything else.

As it stands, it's still having issues finding asset files.  Going to compare what assets are in the Assets dir with the original CVS repo and see if CartBlanche oopsed something in the migration to over here.  This will be followed up with a conversion AWAY from Bink for the video cutscenes because we flatly don't need it and shouldn't even HAVE this in the assets because it's technically not supposed to be there in the FIRST PLACE.

#Status of May 8th, 2018

Raptor/Rucksacksepp (http://soamigos.de/wbb4/index.php/Thread/4458-Siege-exe-aus-Source-Code-Delphi-4/?postID=91558#post91558) of SOAAmigos fixed build with Delphi 4, generalized the resolution code.... lets see if an port is possible.

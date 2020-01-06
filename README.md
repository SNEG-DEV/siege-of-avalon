# Siege of Avalon : Open Source #

_"Siege of Avalon : Open Source is an attempt to keep this great isometric RPG game alive by continuing development on it."_

![Siege of Avalon in HD with room for party of 4.](SoAOS_HD.png)Siege of Avalon in HD with room for party of 4.

## How to complie ##
Install Delphi 10.3.3 or newer - the free Community Edition found at https://www.embarcadero.com/products/delphi/starter will do fine.  
Open the Siege.dpr project file and compile.

## Installing ##

To play the game your need additional files in the assets folder:
- soundlib.dll
- fmod.dll
- DFX_P5S.DLL
- DFX_P6S.DLL

all found in the original free playable 1. chapter/demo - http://soaos.sourceforge.net/FreePage.htm

You must also currently include the newest ddraw.dll from [**DDrawCompat**](https://github.com/narzoul/DDrawCompat) - binary can be downloaded under releases.

## Playing ##

You launch the game by running Siege.exe that is build into the assets folder.

## New Settings added ##

Unlike the original game, it is now possible - based on the work of Rucksacksepp from the SOAmigos forum - in the Options dialog to select either Original, HD or FullHD sceen resolutions. The game runs only full screen.  
Also added are improvements in how hit point are distributed to the party of companions and their AI behaviour - these are found in the siege.ini file:

- AdjustedPartyHitPoints (defaults to false)
- AdjustedCompanionAI (defaults to true)

As mentioned there is also:

- ScreenResolution (defaults to 600) - possible other values are 720 and 1080

There is also a language path that was not surfaced in the Options dialog of the released code - but found in later retail releases, so that needs currently to be changed manually - english, spanish and german are available currently.  
On the TODO list is to add that into the Options dialog - but right now you have to edit the seige.ini file:

- LanguagePath (defaults to english) 


## Focus and origin ##

This is a fork from the [**gondur/Siege-of-Avalon-Open-Source**](https://github.com/gondur/Siege-of-Avalon-Open-Source) repository, which again is a fork of the original released repository moved over from Sourceforge where it was released in 2003.

The main focus of this fork: 

1. Moving the Win32 Delphi 4 source code to Delphi 10.3 or later - including its free Community Editons. **DONE** (and keep it updated to the latest Delphi release)
2. Fixing various glitches. **DONE** (until proved wrong)
3. Heavy refactoring, to make the code a joy to work with - and benefit a modern Object Pascal dialect. **ONGOING**
4. Include HD/FullHD support like already done by gondur. **DONE** (someone should improve the interface graphics I did)
5. Possibly replace DX with SDL2/other to gain crossplatform support, but since the DirectX headers are maintained for latest Delphi versions - this has lower priority.
6. Added improvements and fixes found by the community. **ONGOING**
7. Support Ashes of Avalon (AoA) and other Mods out of the box.

Also check out the gondur fork for changes, and the thread on SOAmigos (in german), where Raptor/Rucksacksepp (http://soamigos.de/wbb5/forum/index.php?thread/4458-hd-und-fullhd-version-zu-siege-of-avalon-aus-dem-source-code-mit-delphi-4/&postID=91286#post91286) originally fixed building with Delphi 4 amoung other things.

Look in the CHANGELOG for status, changes and planned changes.

Tooling and documentation might be found here but would probably be tied to their seperate tooling repositories:

[**POX Studio**](https://github.com/SteveNew/POXStudio) - POX file editor and fileformat documentation.
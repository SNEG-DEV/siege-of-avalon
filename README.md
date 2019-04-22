# Siege of Avalon : Open Source #

_"Siege of Avalon : Open Source is an attempt to keep this great isometric RPG game alive by continuing development on it. Our aim is to create a SDL version of the engine that will work on Win32, Linux and where ever Pascal compilers and SDL are supported."_

This is a fork from the [**gondur/Siege-of-Avalon-Open-Source**](https://github.com/gondur/Siege-of-Avalon-Open-Source) repository.

The main focus of this fork: 

1. Moving the Win32 source to newer Delphi 10.3 or later including its free Community Editons. **DONE**
2. Fixing various glitches and heavy refactoring.
3. Include HD/FullHD support like already done by gondur. **DONE**
4. Possibly replace DX with either SDL2/Vulkan/other to gain crossplatform support, but since the DX is maintain for latest Delphi versions - this has lower priority.

Also check out the gondur fork for changes, and the thread on SOAAmigos (in german), where Raptor/Rucksacksepp (http://soamigos.de/wbb5/forum/index.php?thread/4458-hd-und-fullhd-version-zu-siege-of-avalon-aus-dem-source-code-mit-delphi-4/&postID=91286#post91286) fixed building with Delphi 4 amoung other things.

To play the game your need additional files in the assets folder:
- soundlib.dll
- fmod.dll
- DFX_P5S.DLL
- DFX_P6S.DLL

all found in the original free playable 1. chapter/demo - http://soaos.sourceforge.net/FreePage.htm

You must currently include the newest ddraw.dll from [**DDrawCompat**](https://github.com/narzoul/DDrawCompat) - binary can be downloaded under releases. Specific fixes has been applied until a new version of ddraw.dll is released.

Look in the changelog for status, changes and planned changes.
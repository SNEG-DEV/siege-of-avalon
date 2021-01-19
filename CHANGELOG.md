# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]
- Refactor all save/load rutines - honor file-formats
- Reduce the file I/O overhead
- Add documentation on the assets file-formats used.
- Getting rid of need for custom ddraw.dll
- Replacing DirectDraw with cross-platform library
- Mod selection - more DLC style - extendable instead of destructive copy/paste exercises.
- Better multi-language support - right now it is crippled, like other parts of the UI code.
- REFACTOR: Remove casting and add helpers to improve readability/mainability
- REFACTOR: Too much copy/paste code - (re)Moved method up to base class or extract
- REFACTOR: Reorg classes

## 2021-01-19
### Fixed
- Detecting current screen resolution of primary monitor, to stop picking unsupported HW resolution.

## 2021-01-18
### Fixed
- Small issue with language picker

### Changed
- Cleaned up the folder structures for the repo and moved the SoAOS assets out. Makes tesing easier and is also more correct since they are not part of the engine but included when code was originally released.

## 2021-01-17
### Fixed
- On demand load of resources like spells were broken.

## 2021-01-16
### Fixed
- A problem with double entries in title.db from the SoAmigos Path v0.7 fixed.
- Character selection with only 1 character are now dressed - it is still winter.

## 2020-06-11
### Changed
- Using RTL boolToStr and cleanup of MouseEvent signatures.

## 2020-05-24
### Added
- Support for Delphi 10.4 Sydney.

### Changed
- Many try-except conversions, replaced by StrToXDef one-lines.

## 2020-01-30
### Added
- .LVL map documentation - not fully complete.

### Changed
- TZone and TAniFigure typed lists - so removed some casting and default properties - to improve readability and type checking.

## 2020-01-28
### Added
- Items.db and Title.db documentation.

## 2020-01-27
### Added
- Xref.db documentation - moved into documentation folder with POX file format documentation - both variantions described.

## 2020-01-26
### Added
- Character selection. Will check for PlayerX.pox files and check if their "naked" resource file is found. Apart from just copying the Player files, most of them found here needs to be adjusted for the older versions - since resource names and locations are different :( But playing as Yeti is fun :D

### Changed
- Some code has been rewritten and cleaned-up in the Character creation, but long way to go - a mess.  

### Removed
- Some redundant code
- All DirectX $IFDEFs, and all $IFNDEF DirectX code

## 2020-01-21
### Fixed
- A lot of errors introduced or "missings" in regards to HD/FullHD resolution have now been fixed or resolved.
- A lot of errors in regards to "asset" variations between original game data and new game data.
- Xref.db Version1 FieldByName issue
- HighDPI scaling issue - not allowing the game to run if scaled resolution exceed actual resolution. 4K with >100% DPI.

### Changed
- All "Menu" dialogs are now centered on screen, and their unit names have changed to SoAOS.Intrface.Dialogs...

## 2020-01-16
### Added
- SoAOS.Data.DB unit replaces ItemDatabase.pas - TDataset-style with Locate and TField.as... notation. Also handles Xref.db version variations.
- SoAOS.Intrface.Transit replaces Transit.pas - could need more cleaning

### Changed
- PartsManager and TitleManager changed to use "TDataSet"-styles - improved readability of remaining code. 
- Type casting hell gone in "Transit", and code consolidated  

### Removed
- Much unreadable code

## 2020-01-12
### Added
- Check of old DB format - used in parts - the whole "Database" thing should be rewritten
- Support for both old and new "Menu" resources
- HD resources are now added, and are used if no HD files found - so old retail releases can be played in HD/FullHD with extra new files.
- Load of BMP resource into IDirectDrawSurface

### Changed
- Increased A* path finding grid - to support HD/FullHD, again based on @Rucksacksepp findings
- Game Launcher setting dialog handles old retail releases - with single language/resolution
- INI path handling supports absolute/relative an *nix paths - and cleanedup

## 2020-01-10
### Added
- Game Launcher settings dialog - for Language and screen resolution changes

### Removed
- All ingame Option screen resolution setting code/ui removed

## 2020-01-08
### Changed
- Refactor of 'WideScreen' title code - now read-only TCharacter.Vision property

### Fixed
- Savegames where not property compatible between screen reolutions

## 2020-01-06
### Added
- Improved AI metrics based on @Rucksacksepp AoA code include in ScreenMetric records
- Adding the improvments from @Rucksacksepp AI changes done in AoA - will be default

### Fixed
- A few typos
- RunAway bug found by @Rucksacksepp
- Manual language selection - and color button bug hack

### Changed
- MysticVision in 800x600 mode increased based on @Rucksacksepp findings - existed as a new ScreenMetric record a few hours.
- Updated README to better reflect what has been added, were it is going and differencies to the original released code.

## 2020-01-05
### Changed
- Moved Delay, Walking and CollisionCount to TAI

### Fixed
- A few POS() mishaps

## 2020-01-02
### Changed
- Changed all use of POS() to string helpers - syntactical sugar and recommended
- Commented more unused code - will be removed next
- Moved TFacing and helper into SoAOS.Types

### Added
- TFacing record helper

### Removed
- All FacingString code

## 2020-01-01
### Added
- TTokenString string helpers
- Missing License header in new files 

### Removed
- All old strFunctions - replaced by custom string helpers or newer alternatives

## 2019-12-28
### Changed
- $DODEBUG "noise" isolated
- A bit of INI parsing refactor
- ScreenMetrics includes popup rects for Original/HD/FullHD

### Fixed
- Popup working for Original/HD/FullHD

## 2019-08-12
### Removed
- Temporary fix for DDrawCompat draw.dll issue (see 2019-04-22) - fixed in newest experimental builds. 

## 2019-07-16
### Changed
- New Tools: discontinued - and continued as POXStudio at: https://github.com/SteveNew/POXStudio 

### Added
- Pure DirectX branch - cleared of IFDEFS

## 2019-06-14
### Fixed
- Pink bug on flickering TLightZones.

## 2019-06-13
### Added
- New Tools: added more actions and fixed several bugs in POX Viewer

### Fixed
- Included bug fixes by Rucksacksepp - on pink tile artifacts - a great thanks for finding these.

### Removed
- Commented out unused Mouse pointer animation code

## 2019-06-12
### Removed
- Old SoAOS folder - the unfinished SDL (only Menus done)

### Added
- New Tools folder - including new cross-platform POX viewer

## 2019-06-07
### Added
- POX File documentation

## 2019-04-22
### Fixed
- Save/Load game now working again.
- Temporary fix for DDrawCompat latest ddraw.dll issue - save game/screenshot and arrow incorrect drawn due to upside-down bitmap context.

### Removed
- A few digifx references

### Added
- A few units for the sake of separation

## 2019-04-14
### Removed
- A few TImage gone

## 2019-04-07
### Changed
- Cleaned out a bit of TBitmaps and wrapped some DD code.
- Most BMPs are loaded to GPU surfaces - but still too many times - so just faster not better :)

### Removed
- Some VCL dependency
- Unused GIF to POX conversion code - will be back in a different form.


## 2019-04-06
### Added
- Added Original/HD/FullHD option in option dialog - requires restart/activate - will leave it at that until SDL2
- HD and FullHD interface assets - SoA-styled, feel welcome to improve these - a bit of a rush job

### Fixed
- Cursor artifacts just introduced
- Color glitch in menu - I tried to ignore

## 2019-03-31 
### Fixed
- Replaced 4 "corrupt/missing" assets - causing out of memory errors and crashes

## 2019-03-29 
### Fixed
- A bit more spring cleaning - this time again on colors and functions

### Removed
- Some VCL dependency
- Anigrp30cfg.inc file

## 2019-03-26
### Added
- Added preliminary support keymappings.
- Added preliminary support for Original/HD/FullHD option

### Fixed
- A bit more spring cleaning - this time on colors

## 2019-03-25
### Fixed
- Replaced winapi functions with cross-platform alternatives

### Removed
- A fair part of windows-ism - Windows unit only left in DirectDraw related stuff

## 2019-03-24
### Added
- Moved change log/status from readme into changelog file.
- MMTimer unit missing from project file

### Fixed
- Save games back to normal
- Cleaned a bit of uses and reduced compiler warnings
- Absolute vs relative path fun with initial startfile (lvl)
- All critical warnings gone - mostly cause by unicode transition

### Removed
- string32 unit
- security unit

## 2019-03-22
### Fixed
- Playable - compiled with Delphi 10.3.1 Rio, so ready for Community Editon!

## 2019-03-21
### Changed
- Updated readme and merge win32_d7onwards branch with master, so now Delphi 2007 is minimum.

## 2019-03-20
### Changed
- Stepped back. Now playable in Delphi 2007

### Removed
- Unused uses clean + a bit strfunc remove - 0 Hints, 0 Warnings in Delphi 2007

## 2019-03-17
### Changed
- Compiles and runs in DX 10.2.3 - non-playable yet
- Character screen has glitches, but works
- Loading 90% - missing lightning/tiles

## 2019-03-14
### Fixed
- Replaced 3 "corrupt/missing" assets

### Changed
- Replaced and updated DX files - using files from unDelphiX (http://www.micrel.cz/Dx/)
- Converted source to compilable in Delphi 7, being fully playable.

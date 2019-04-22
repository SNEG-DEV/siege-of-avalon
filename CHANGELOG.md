# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]
- Refactor all save/load rutines - honor file-formats
- Reduce the file I/O overhead
- Add documentation on the assets file-formats used.
- Evaluate the tools - maybe redo/merge them - and include them in the repository.
- Getting rid of need for custem ddraw.dll
- Replacing DirectDraw with cross-platform library
- Add a character selector - again probably re-organizing the "Character Creator" dialog
- Character in the center - add male/female choices.
- Mod selection - more DLC style - extendable instead of destructive copy/paste exercises.
- Better multi-language support - right now it is crippled, like other parts of the UI code.

## 2019-04-22
### Fixed
- Save/Load game now working again.
- Temporary fix for DDrawCompat lastest ddraw.dll issue - save game/screenshot and arrow incorrect drawn due to upside-down bitmap context.

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

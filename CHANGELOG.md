# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]
- Remove all none-critical warnings in code compilation under DX 10.3.1, all due to unicode
- Refactor all save/load rutines - honor file-formats
- Reduce the file I/O overhead
- Merge the non HD-related fixes from gondur repo
- Add documentation on the assets file-formats used.
- Evaluate the tools - maybe redo/merge them - and include them in the repository.
- Getting rid of need for custem ddraw.dll
- Replacing DirectDraw with cross-platform library
- Remove/minimize window-ism
- Add HD/FullHD support - either add a "launcher" or re-organizing the "Options"
- Add a character selector - again probably re-organizing the "Character Creator" dialog
- Character in the center - add male/female choices.
- Mod selection - more DLC style - extendable instead of destructive copy/paste exercises.
- Better multi-language support - right now it is crippled, like other parts of the UI code.


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

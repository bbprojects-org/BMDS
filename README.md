## BMDS

BMDS started life as an emulator for the Microtan 65, and was known as MDS (Microtan Development System). Having got most of that working, I moved on to emulating Space Invaders building on the existing architecture, then the CHIP-8. As a result it became BMDS (Beyond the Microtan Development System)!.

## Introduction

BMDS is written with Lazarus / Free Pascal, although it was originally coded for Delphi, and should work on Windows as well as Mac. In fact with mods it can probably be made to work on most platforms given the flexibility of Lazarus/FPC.

Functionality includes:

- **Microtan 65 Emulation**: ...

- **Space Invaders Emulation**: ... 

- **CHIP-8 Emulation**: ...

- For each emulation also supports some of the following additional functionality; assembler, disassembler, execution trace, ...

## Notes

When compiling, Lazarus/FPC will put all build files in a "*_obj*" folder, and the executable in a "*_bin*" folder. I put the underscore in front of these (and some other folders) to remind me which folders do not need to be pushed to GitHub.

### Windows

Not yet tested on Windows.

### Mac

Having cloned / downloaded the files there are some additional preparations required:

- **Create App bundle**: create a ".app" bundle for *BMDS* via Lazarus' "*Project | Project Options*" and the "*Create Application Bundle*" button. This will put a ".app" file in the *_bin* folder with the *BMDS* executable

- **Copy resources to App bundle**: copy the Resources folder, with the machine ROMS etc, into the BMDS ".app" package (*BMDS.app/Contents/MacOS/* folder)

Note: when ready to use the ".app" file elsewhere ensure you move the *BMDS* executable file into the *BMDS.app/Contents/MacOS/* folder, replacing the alias put there by Lazarus. Put the ".app" file in a DMG file for distribution

## License

This software has been released under the [GNU General Public License](https://www.gnu.org/licenses/) as published by the Free Software Foundation


## XRef.DB file format

A text file where the first line represents columns/fields and the first "element" on the following lines represents the "key" value. A | character is used as column/field seperator. Beware that every line ends with a seperator. 

There are two formats of this file - the original 2001 and the one released with the source code - hereafter refered to as V1 and V2.

**Characteristics:** The file starts with the string `Base`

**V1:** A single .gif file reference in each column without relative path:  
Line 1: `Base|BaseHumanMale.gif|BaseHumanFemale.gif|BaseSkeleton.gif|BaseAhoul.gif|`

There is a variation of V1 introduced by the Elves around chapter 3 - no extra columns/fields where added for this so the they used the first column as placeholder?

**V2:** Multiple .gif files references in a column with relative path:  
Line 1: `Base|humanmalelayers\BaseHumanMale.gif,humanmalelayers\BaseSkeleton.gif,humanmalelayers\BaseAhoul.gif,humanmalelayers\BaseShaman.gif,humanmalelayers\BaseHumanBlank.gif|humanfemalelayers\BaseHumanFemale.gif|humanfemale2layers\BaseHumanFemale.gif|ElfMaleLayers\BaseElf.gif|NagaMale\BaseNaga1.gif,NagaMale\BaseNaga1.gif|`  

It seems that since in V1 most entries where the same for HumanMale, Skeleton and Ahoul, and additional classes were added that fit in that group - Shaman and HumanBlank - the change was done to minimize the level of redundancy and thereby file size. There are now two female classes - the classic and the new playable, an Elf and two Naga classes (should probably have been Naga1 and Naga2).  

The following linies start with the key `prt_<name>`, examples are:  

**V1:** `prt_Axe|HumanMaleLayers\Axe.gif|HumanMaleLayers\Axe.gif|HumanMaleLayers\Axe.gif|HumanMaleLayers\Axe.gif|`  
**V2:** `prt_Axe|HumanMaleLayers\Axe.gif||HumanFemale2Layers\FineWarAxe.gif|ElfMaleLayers\ElfSword.gif|NagaMale\NagaFineAxe.gif|`  

Shown in table form below:  

| V1 | BaseHumanMale.gif | BaseHumanFemale.gif | BaseSkeleton.gif | BaseAhoul.gif |  
| ---- | ---- | ---- | ---- | ---- |
|prt_Axe|HumanMaleLayers\Axe.gif|HumanMaleLayers\Axe.gif|HumanMaleLayers\Axe.gif|HumanMaleLayers\Axe.gif|

| V2 | "humanmalelayers\BaseHumanMale.gif"s | humanfemalelayers\BaseHumanFemale.gif | humanfemale2layers\BaseHumanFemale.gif | ElfMaleLayers\BaseElf.gif | NagaMale\BaseNaga1.gif,NagaMale\BaseNaga1.gif |  
| ---- | ---- | ---- | ---- | ---- | ---- |
|prt_Axe|HumanMaleLayers\Axe.gif||HumanFemale2Layers\FineWarAxe.gif|ElfMaleLayers\ElfSword.gif|NagaMale\NagaFineAxe.gif|
  
Notice that the classic female can now not wear/use an Axe.

As in most (all?) places in the engine the resource files are refered to as `.gif`, but are actually POX files - and that has to be handled and changed everytime a file is read, but kept because of refered like that from other resources (INI).

The file is used by the **PartManager**, together with the Items.db file.

Disclaimer: The documentation is based on current findings - and might be proved wrong - if you see any wrong conclusions or have additional info, please let me know.

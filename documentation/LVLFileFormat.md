## LVL (Level/Map) file format

There are a few missing parts, so still WIP.

| Offset | Bytes | Description
| ------ | ----- | -----------
|0       |4      |`D3 F9 57 00`
|4       |4      | Vesion (other than 3?)
||| ... the below seqeuence of reading various "blocks" is repeated until the end of the file.
|8 + n |4      |**Block type** 0-19
|        |       | 0: mbHeader
|||1 : mbMap
|||2 : mbRectResourceList
|||3 : mbDiamResourceList
|||4 : mbObjResourceList
|||5 : mbLayer0
|||6 : mbLayer1
|||7 : mbLayerDiamond
|||8 : mbLayerTag - not used
|||9 : mbObject
|||10 : mbScene
|||11 : mbZones
|||12 : mbSceneKnown
|||13 : mbTheme
|||14 : mbChapt - does nothing
|||15 : mbAltObjResourceList
|||16 : mbAltRectResourceList
|||17 : mbAltDiamResourceList
|||18 : mbBB1 - does nothing
|||19 : mbBB2 - does nothing
|12 + n |4| Block size - does include size of type and size - so size-8 remains to be read
|||... file should end with `$A0A0`

...	the various "blocks":

**mbHeader**

| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| Map.TileSize

**mbMap** - might be skipped and read from cache

| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| Map.Width
|4|4| Map.Height
|8|4| RandSeed
|12|4| FillIndex - is incremented by 1
|16|4| FillVarLimit - is decremented by 1
RIndex[FillIndex] is used by SetTile for Width x Height of Map adding random of FillVarLimit for Layer 0, Zone 1 - so mbRectResourceList or mbAltRectResourceList should have been read first since they define RIndex.

**mbRectResourceList, mbAltRectResourceList** - might be skipped and read from cache.

| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| Resource stringlist size
|4|size| RNames stringlist data - an empty string is inserted as item 0.
RIndex array is set by loading and blitting the TileResources given by RNames. RIndex contains an array of TileIndex which is incremented by the number of frames in the given TileResource. Starts with [0]=0 and [1]=1.

**mbDiamResourceList, mbAltDiamResourceList** - might be skipped and read from cache.

| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| Resource stringlist size
|4|size| DNames stringlist data - an empty string is NOT inserted as item 0.
DIndex array is set similar to RIndex (see above), and DVariations is set by reading the INI datas ImageList from theTileResources given by DNames.

**mbObjResourceList, mbAltObjResourceList**

| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| Resource stringlist size
|4|size| ONames stringlist data - an empty string is inserted as item 0.
OIndex array is an index array of TStaticObjects, TDoorResources or TCharacters - ONames starting with 'editor\' are "skipped". And ZINdex is set. A lot goes on here so more details to come.

**mbLayer0** - might be skipped and read from cache.  
Read until the given block size of data is read, in the following chunks:
| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| iMapPos - gives the X (iMapPos mod Width) and Y (iMapPos div Width)
|4|2| iPos - is incremented by 1
|6|2| Offset
|8|1| Variation
If the Offset is 0, then Map.SetTile( X, Y, 0, 1, RIndex[ iPos ] + Variation - 1 ) is called.

**mbLayer1** - might be skipped and read from cache.
Same as mbLayer0 - just setting layer 1.

**mbLayerDiamond** - might be skipped and read from cache.
Read until the given block size of data is read, in the following chunks:
| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| iMapPos - gives the X (iMapPos mod Width) and Y (iMapPos div Width)
|4|1| iSlice - number of slices
|||... below is repeated the number of slices
|0|1| Slice index in TDTileInfo structure
|1|2| iPos - local-based index
|3|1| Element
|4|1| Variation
All this is used to call Map.SetDiamondTile(), based on some extra logic.

**mbObject**

| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| X
|4|4| Y
|8|4| Z
|12|4| Index - is incremented by 1 (-1 exits)
|16|4| ImageIndex
|20|4| Size of Attribute string
|24|size| Attribute stringlist
A lot goes on here so more details to come.


**mbScene**

| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| AmbientColor
|4|4| AmbientIntensity - adjusted by GlobalBrightness
|8|4| BaseLightType
|12|4| Size of scenename string
|16|size| Scene name - where AmbientColor and AmbientIntensity is allied.

**mbZones**

| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| Count
|4|4| Size of zones string
|8|size| Zones stringlist
....Missing the rest for now, but .zit and cache is involved.

**mbSceneKnown**

| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| Number of entries
|sceneIdx * 4|4| MapKnown if not 0
Checkes if the current SceneIdx is a known map.
            
**mbTheme**

| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| If value = to SceneIdx-1 read rest
|4|4| Size of theme name string (size1)
|8|size1| Theme name
|8 + size1|4|Size of themes string (size2)
|8 + size1 + 4|size2|Themes item stringlist
Result is added to the Themes stringllist as a key value pair - \<ThemeName>=\<comma delimited list of Themes>




See POX file documentation, for more details on the resource data loaded/read during loading of the map/lvl  file.

Disclaimer: The documentation is based on current findings - and might be proved wrong - if you see any wrong conclusions or have additional info, please let me know.


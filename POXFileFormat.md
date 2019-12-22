## POX (Proprietary Object eXtension) file format

| Offset | Bytes | Description
| ------ | ----- | -----------
|0       |4      |`POXA`
|4       |2      |**Object type**
|        |       |`ST` : Static Resource
|||`CC` : Non-Layered Character Resource
|||`LC` : Layered Character Resource
|||`DS` : Door Resource
|||`TT` : Tile Resource
|||`PR` : Projectile Resource
|||`SC` : Cast Resource
|||`LL` : (Linked) Layer Resource
|||`II` : Inventory Resource
|||`SP` : Resource
|6|2|`\r\n` - filler
|8|4|Size in bytes of Ini Data Block - *iniLength*
|12|*iniLength*|Ini file content - ansi encoded
|12+*iniLength*|2|`BB` - Begin of RLE bitmap data stream (TRLESprite) marker

...	RLE bitmap data stream

| Offset | Bytes | Description
| ------ | ----- | -----------
|0|4| Picture Count - *picCnt*
|4|4| RLE data size - *rleSize*
|8|*picCnt* * RLEHDR record size| RLEHDR records - with pointer to RLE data offset
|9+(*picCnt* * RLEHDR record size)|`rleSize`|RLE data - run length encoded image data

Pascal pseudo code to read and decode the RLE data into bitmap/pixel data:
```
type
  PRLEHDR = ^RLEHDR;
  RLEHDR = record
    SrcX : integer;
    SrcY : integer;
    Wdh : DWORD;
    Hgh : DWORD;
    AdjX : integer;
    AdjY : integer;
    PixFmt : DWORD;
    DataPtr : PChar;
  end;

var
  Size, BuffSize, i : DWORD;
  lpRLE, RelocOffset : PChar;
  p : PRLEHDR;
  
  Stream.Read( PicCnt, SizeOf( PicCnt ) );
  Stream.Read( BuffSize, SizeOf( BuffSize ) );
  Size := PicCnt * SizeOf( RLEHDR );
  GetMem( lpSpr, Size );
  Stream.Read( lpSpr^, Size );
  GetMem( lpRLE, BuffSize );
  Stream.Read( lpRLE^, BuffSize );

  RelocOffset := PChar( lpRLE - lpSpr.DataPtr );
  p := lpSpr;
  for i := 1 to PicCnt do
  begin
    p.DataPtr := PChar( p.DataPtr + DWORD( RelocOffset ) );
    DecodeRLE(p); // previously handled by digifxConvertRLE( dfx_hnd, p ) - which blocks any further progress
    Inc( p );
  end;
  
  procedure DecodeRLE(rle: PRLEHDR);
  begin
    x := 0;
    y := 0;
    
    fin := TBytesStream.Create();
    fin.Write(rle.DataPtr^, BuffSize);
    fin.Position := 0;
    try
    while true do
    begin
      fin.Read(&c, 1);
      case c of
        1 : begin // colour/pixel data
          fin.Read(&i, 4);
          while i > 0 do
          begin
            fin.Read(&colour, 2);
            SetPixel(X+rle.AdjX, Y+rle.AdjY, colour); // fill pixelmap/bits
            inc(x);
            dec(i);
          end;
        end;
        2 : begin // add x offset
          fin.Read(&i, 4);
          i := i div 2;
          inc(x, i);
        end;
        3 : inc(y); // new line, carriage return
        0 : break;
      end;
    end;
    finally
      fin.Free;
    end;
  end;
  ```
  the above code is based on findings by the_Bug (Jun 20th 2011) - on the german forums.

## **Ini data**

**Legend**  
<sub>1</sub>TResource.LoadData  
<sub>2</sub>TResource.LoadAction  
<sub>3</sub>TCharacterResource.LoadData  
<sub>4</sub>TProjectileResource.LoadData  
<sub>5</sub>TDoorResource.LoadData  
<sub>6</sub>TDoorResource.Define - might be code smell here!!!  
<sub>7</sub>TStaticResource.Define  
<sub>8</sub>TStaticResource.LoadData (TTileResource)  
<sub>9</sub>LoadMap (TTileResource.LoadData)  
<sub>10</sub>TLayerResource.LoadData  
<sub>11</sub>TInventoryResource.LoadData  
<sub>12</sub>TLinkLayered  
<sub>13</sub>Not used/read  

**[Header]**

| Property | Values | Description | Default| Use
| ------ | ----- | ----------- | ---- | ---
|ImageWidth|int|set FrameWidth|96, 0<sub>8</sub>|<sub>1,11</sub>
|ImageHeight|int|set FrameHeight|86, 0<sub>8</sub>|<sub>1,11</sub>
|Blend|add, [sub, subtract], alpha|set SpecialEffect (seAdd, seSubtract, seTranslucent, seNone)||<sub>1</sub>
|BlendAmount|int|set Alpha|100|<sub>1</sub>
|UseLighting|none, vert, \<else>|set UseLighting and Vertical||<sub>1</sub>
|Highlightable|yes, \<else>|set Highlightable : Enables a resource to be "highlighted"||<sub>1</sub>
|Shadow|[no, none], \<else>, simple|set DrawShadow and ComplexShadow||<sub>1</sub>
|ShadowColor|int|set ShadowColor|0|<sub>1</sub>
|CollisionRadius|int|set Radius|16|<sub>1</sub>
|FrameMultiplier|int|set FrameMultiplier|1|<sub>1</sub>
|CollisionOffset|int|set CenterX|FrameWidth div 2|<sub>1</sub>
|CollisionHeight|int|set CenterY|FrameHeight-10|<sub>1</sub>
|Speed|string|set Speed (and RunSpeed)|'5.0'|<sub>1</sub>
|Actions|string|read list of "actions" whos inidata needs loaded and their TScripts are created||<sub>1</sub> 
|UseCastAnimation|'false',\<else>|set UseCastAnimation||<sub>3</sub>
|GameClassType|'multiimage'|set local var MultiImage (also true when FrameCount>1) - might be code smell here!!!||<sub>6,7</sub>
|DepthAnchors|'','XX',\<else>|call LoadArray( S, DepthAnchors )||<sub>6,7</sub>
|LightPoints|'','XX',\<else>| call LoadArray( S, LightPoints )||<sub>6,7</sub>
|XRayable|'no','',\<else>|if <else>: calculate Slope for Map.DefineItem||<sub>6,7</sub>
|CollisionMask|'','XX',\<else>|call LoadArray( S, CollisionMask )||<sub>6,7</sub>
|LineOfSightMask|'','XX',\<else>|call LoadArray( S, LineOfSightMask )||<sub>6,7</sub>
|GameImageFrame|int|set ItemFrame|191|<sub>10</sub>
|LinkedLayerFile|string|set LinkedResource||<sub>10</sub>
|LayeredFramesToBack|string|set BackLayer array (0..383)||<sub>10</sub>
|FileName|string|||<sub>13</sub>
|GameClass|string|||<sub>13</sub>
|TransparentColor|int|||<sub>13</sub>
|ImagePacking|bool|||<sub>13</sub>
|EditorImage|int|||<sub>13</sub>
|TileType|string|||<sub>13</sub>
|Presentation||||<sub>13</sub>
|ValidLayers|string|||<sub>12,13</sub>
|LayeredParts|string|||<sub>12,13</sub>

>**MultiImage sections:**
>
>**[CollisionMasks]**<sub>7</sub>
>
>|Property | Values | Description | Default|Use
>| ------ | ----- | ----------- | ---- | --
>|Frame<ImageIndex + 1>|'','XX',\<else>|call LoadArray( S, CollisionMasks )||<sub>7</sub>
>
>**[LineOfSightMasks]**<sub>7</sub>
>
>|Property | Values | Description | Default|Use
>| ------ | ----- | ----------- | ---- | --
>|Frame<ImageIndex + 1>|'','XX',\<else>|call LoadArray( S, >LineOfSightMasks )||<sub>7</sub>
>
>**[DepthAnchors]**<sub>6, 7</sub>
>
>|Property | Values | Description | Default|Use
>| ------ | ----- | ----------- | ---- | --
>|Frame<ImageIndex + 1>|'','XX',\<else>|call LoadArray( S, DepthAnchors )||<sub>6, 7</sub>
>
>**[LightPoints]**<sub>6, 7</sub>
>
>|Property | Values | Description | Default|Use
>| ------ | ----- | ----------- | ---- | --
>|Frame<ImageIndex + 1>|'','XX',\<else>|call LoadArray( S, LightPoints )||<sub>6, 7</sub>
>
>**[XRayable]**<sub>6, 7</sub>
>
>|Property | Values | Description | Default|Use
>| ------ | ----- | ----------- | ---- | --
>|Frame<ImageIndex + 1>|'no','',\<else>|if \<else>: calculate Slope for Map.DefineItem - !!Only last frame is used!!||<sub>6, 7</sub>
>

**[Action\<X>]**<sub>2</sub>

|Property | Values | Description | Default|Use
| ------ | ----- | ----------- | ---- | ---
|FrameMultiplier|int|set Multiplier|FrameMultiplier|<sub>2</sub>
|Frames|string|if <>'' load script with list of frames for \<X> with Multiplier - TScript is created with Multiplier, array of frames and type/tag, and add these to the TAniResource.Script stgringlist with either TScript objects.
|SSFrames|string|if <>'' load script with list of frames<sup>*</sup> for \<X>SS with Multiplier||<sub>2</sub>
|SEFrames|string|if <>'' load script with list of frames<sup>*</sup> for \<X>SE with Multiplier||<sub>2</sub>
|EEFrames|string|if <>'' load script with list of frames<sup>*</sup> for \<X>EE with Multiplier||<sub>2</sub>
|NEFrames|string|if <>'' load script with list of frame<sup>*</sup> for \<X>NE with Multiplier||<sub>2</sub>
|NNFrames|string|if <>'' load script with list of frames<sup>*</sup> for \<X>NN with Multiplier||<sub>2</sub>
|NWFrames|string|if <>'' load script with list of frames<sup>*</sup> for \<X>NW with Multiplier||<sub>2</sub>
|WWFrames|string|if <>'' load script with list of frames<sup>*</sup> for \<X>WW with Multiplier||<sub>2</sub>
|SWFrames|string|if <>'' load script with list of frames<sup>*</sup> for \<X>SW with Multiplier||<sub>2</sub>

<sup>*</sup>list of frames is a comma seperated list of number with a end marker/script tag. - 'loop', 'random', 'die', <'end',''>

**[Action Walk]**<sub>1</sub>

|Property | Values | Description | Default|Use
| ------ | ----- | ----------- | ---- | ---
|MovementPerFrame|string|set Speed - if <>''||<sub>1</sub>

**[Action Run]**<sub>1</sub>

|Property | Values | Description | Default|Use
| ------ | ----- | ----------- | ---- | ---
|MovementPerFrame|string|set RunSpeed - if <>''||<sub>1</sub>

**[Action Death]**<sub>1</sub>

|Property | Values | Description | Default|Use
| ------ | ----- | ----------- | ---- | ---
|MovementPerFrame|string|set DeathSlide (false = '-1' else true)||<sub>1</sub>

**[Action Attack1]**<sub>3</sub>

|Property | Values | Description | Default|Use
| ------ | ----- | ----------- | ---- | ---
|TriggerFrame|int|set FContactFrame|1|<sub>3</sub>

**[Action BowAttack]**<sub>3</sub>

|Property | Values | Description | Default|Use
| ------ | ----- | ----------- | ---- | ---
|TriggerFrame|int|set FReleaseFrame|1|<sub>3</sub>

**[Action Cast]**<sub>3</sub>

|Property | Values | Description | Default|Use
| ------ | ----- | ----------- | ---- | ---
|TriggerFrame|int|set FCastFrame|1|<sub>3</sub>

**[Action Explode]**<sub>4</sub>

|Property | Values | Description | Default|Use
| ------ | ----- | ----------- | ---- | ---
|TriggerFrame|int|set FContactFrame|1|<sub>4</sub>

**[Layers]**<sub>3</sub>

|Property | Values | Description | Default|Use
| ------ | ----- | ----------- | ---- | ---
|leg1|string|set Equipment[ slLeg1 ]|''|<sub>3</sub>
|boot|string|set Equipment[ slBoot ]|''|<sub>3</sub>
|leg2|string|set Equipment[ slLeg2 ]|''|<sub>3</sub>
|chest1|string|set Equipment[ slChest1 ]|''|<sub>3</sub>
|chest2|string|set Equipment[ slChest2 ]|''|<sub>3</sub>
|arm|string|set Equipment[ slArm ]|''|<sub>3</sub>
|belt|string|set Equipment[ slBelt ]|''|<sub>3</sub>
|chest3|string|set Equipment[ slChest3 ]|''|<sub>3</sub>
|gauntlet|string|set Equipment[ slGauntlet ]|''|<sub>3</sub>
|outer|string|set Equipment[ slOuter ]|''|<sub>3</sub>
|helmet|string|set Equipment[ slHelmet ]|''|<sub>3</sub>
|weapon|string|set Equipment[ slWeapon ]|''|<sub>3</sub>
|shield|string|set Equipment[ slShield ]|''|<sub>3</sub>
|tabar|string|set Equipment[ sltabar ]|''|<sub>3</sub>
|misc1|string|set Equipment[ slMisc1 ]|''|<sub>3</sub>
|misc2|string|set Equipment[ slMisc2 ]|''|<sub>3</sub>
|misc3|string|set Equipment[ slMisc3 ]|''|<sub>3</sub>
|naked|string|set NakedResource, UseDefaultPants and Female ('humanmalelayers\basehumanmale.gif', 'humanfemale2layers\basehumanfemale.gif', 'elfmalelayers\baseelf.gif')|''|<sub>3</sub>
|head|string|set HeadResource|''|<sub>3</sub>

<sup>*</sup>set FAttackVariations based on number of [Action Attack\<N>] sections<sub>3</sub>

**[Properties]**<sub>3</sub>

|Property | Values | Description | Default|Use
| ------ | ----- | ----------- | ---- | ---
|% - read section into Defaults strings
|AIMode=IdleAI
|CombatAI=BasicFight
|IdleAI=Meander
|PartyAI=Companion
|AllowTalk=True
|BaseCourage=5
|Combative=False
|Distance=175
|HealFirst=True
|IdleDuty=Menader
|LeashLength=50
|MainStat=Strength
|Moveable=True
|TakeOrders=True
|TimeToRun=75
|UndeadType=Skeleton
|WolfType=Wolf
|Charm=10
|Combat=5
|Constitution=7
|Coordination=7
|HealingRate=10
|Hearing=40
|HitPoints=10
|Mana=10
|Movement=10
|Mysticism=5
|Perception=10
|Smell=20
|Stealth=5
|Strength=7
|Recovery=10
|RechargeRate=10
|Restriction=0
|Vision=400
|BuyingDiscount=0.75
|IsMerchant=False
|MoneyAmount=0
|SellingMarkup=1.25
|CharacterName=#Charactername.Scout#

**[ImageList]**<sub>9</sub>

|Property | Values | Description | Default|Use
| ------ | ----- | ----------- | ---- | ---
|Center|string|set DVariations[ i, ord( dqCenter ) ] := LoadIndexes( S )||<sub>9</sub>
|EECornerIn|string|set DVariations[ i, ord( dqIE ) ] := LoadIndexes( S )||<sub>9</sub>
|EECornerOut|string|set DVariations[ i, ord( dqOE ) ] := LoadIndexes( S )||<sub>9</sub>
|NEEdge|string|set DVariations[ i, ord( dqNE ) ] := LoadIndexes( S )||<sub>9</sub>
|NNCornerIn|string|set DVariations[ i, ord( dqIN ) ] := LoadIndexes( S )||<sub>9</sub>
|NNCornerOut|string|set DVariations[ i, ord( dqON ) ] := LoadIndexes( S )||<sub>9</sub>
|NWEdge|string|set DVariations[ i, ord( dqNW ) ] := LoadIndexes( S )||<sub>9</sub>
|WWCornerIn|string|set DVariations[ i, ord( dqIW ) ] := LoadIndexes( S )||<sub>9</sub>
|WWCornerOut|string|set DVariations[ i, ord( dqOW ) ] := LoadIndexes( S )||<sub>9</sub>
|SWEdge|string|set DVariations[ i, ord( dqSW ) ] := LoadIndexes( S )||<sub>9</sub>
|SSCornerIn|string|set DVariations[ i, ord( dqIS ) ] := LoadIndexes( S )||<sub>9</sub>
|SSCornerOut|string|set DVariations[ i, ord( dqOS ) ] := LoadIndexes( S )||<sub>9</sub>
|SEEdge|string|set DVariations[ i, ord( dqSE ) ] := LoadIndexes( S )||<sub>9</sub>




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

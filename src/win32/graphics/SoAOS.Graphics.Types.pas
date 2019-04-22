unit SoAOS.Graphics.Types;

interface

uses
  System.Types;

type
  PBITPLANE = ^BITPLANE;
  BITPLANE = record
    bitsPtr : PBYTE;
    bitsWdh : DWORD;
    bitsHgh : DWORD;
    bitsFmt : DWORD;
    bitsPitch : DWORD;
    BaseX : integer;
    BaseY : integer;
  end;

implementation

end.

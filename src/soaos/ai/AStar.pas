unit AStar;
{******************************************************************************}
{                                                                              }
{               Siege Of Avalon : Open Source Edition                          }
{               -------------------------------------                          }
{                                                                              }
{ Portions created by Digital Tome L.P. Texas USA are                          }
{ Copyright ©1999-2000 Digital Tome L.P. Texas USA                             }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Portions created by Team SOAOS are                                           }
{ Copyright (C) 2003 - Team SOAOS.                                             }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{                                                                              }
{                                                                              }
{ You may retrieve the latest version of this file at the SOAOS project page : }
{   http://www.sourceforge.com/projects/soaos                                  }
{                                                                              }
{ The contents of this file maybe used with permission, subject to             }
{ the GNU Lesser General Public License Version 2.1 (the "License"); you may   }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   DirectX Runtime libraris on Win32                                          }
{   They are available from...                                                 }
{   http://www.microsoft.com.                                                  }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   July    13 2003 - DL : Initial Upload to CVS                               }
{                                                                              }
{******************************************************************************}

interface

uses
  Classes,
  Windows,
  SysUtils,
  LogFile;

const
  MinSearch = -128;
  MaxSearch = 128;
  HeapSize = 256;

type
  TAGrid = packed record
    FromX, FromY : Smallint;
    NextX, NextY : Smallint;
    Iteration : cardinal;
    D : word;
    Closed : boolean;
    NotEdge : boolean;
  end;

type
  TInitD = packed record
    NextX, NextY : Smallint;
    Iteration : cardinal;
  end;

type
  TMoveTest = function( SrcX, SrcY, DestX, DestY : Smallint ) : boolean of object;

type
  TAStar = class( TObject )
  private
    FSrcX, FSrcY : Smallint;
    FDestX, FDestY : Smallint;
    Iteration : cardinal;
    MinD : word;
    StartD : word;
    ClosestD : word;
    ClosestX : Smallint;
    ClosestY : Smallint;
    AGrid : array[ MinSearch - 1..MaxSearch + 1, MinSearch - 1..MaxSearch + 1 ] of TAgrid;
    InitD : array[ 1..HeapSize ] of TInitD;
    procedure OpenCell( CurrentX, CurrentY, X, Y : Smallint );
  public
    Deviance : integer;
    CanMove : TMoveTest;
    constructor Create;
    function FindPath( SrcX, SrcY, DestX, DestY : Smallint; var Handle : HGLOBAL ) : word;
    function FindJaggedPath( SrcX, SrcY, DestX, DestY : Smallint; var Handle : HGLOBAL ) : word;
  end;

implementation

function iabs( I : integer ) : integer;
const
  FailName : string = 'AStar.iabs';
begin
  if ( I < 0 ) then
    result := -I
  else
    Result := I;
end;

procedure TAStar.OpenCell( CurrentX, CurrentY, X, Y : Smallint );
const
  FailName : string = 'AStar.TAStar';
var
  D, D0, D1 : word;
  dX, dY : word;
  X1, Y1 : Smallint;
  X2, Y2 : Smallint;
begin
  if AGrid[ X, Y ].NotEdge then
  begin
    if ( AGrid[ X, Y ].Iteration <> Iteration ) or not AGrid[ X, Y ].Closed then
    begin
      dX := iabs( FDestX - X );
      dY := iabs( FDestY - Y );
      if ( dX > dY ) then
        D0 := dX
      else
        D0 := dY;
      D := D0 + AGrid[ CurrentX, CurrentY ].D + 1;
      if ( D > HeapSize ) then
        exit;
      if CanMove( CurrentX, CurrentY, X, Y ) then
      begin
        if AGrid[ X, Y ].Iteration = Iteration then
        begin
          if AGrid[ CurrentX, CurrentY ].D + 1 >= AGrid[ X, Y ].D then
            exit;
          if ( dX > dY ) then
            D1 := dX + AGrid[ X, Y ].D
          else
            D1 := dY + AGrid[ X, Y ].D;
          //Remove cell from its previous position on the chain
          if ( InitD[ D1 ].NextX = X ) and ( InitD[ D1 ].NextY = Y ) then
          begin
            InitD[ D1 ].NextX := AGrid[ X, Y ].NextX;
            InitD[ D1 ].NextY := AGrid[ X, Y ].NextY;
            InitD[ D1 ].Iteration := AGrid[ InitD[ D1 ].NextX, InitD[ D1 ].NextY ].Iteration;
          end
          else
          begin
            X1 := InitD[ D1 ].NextX;
            Y1 := InitD[ D1 ].NextY;
            repeat
              X2 := X1;
              Y2 := Y1;
              X1 := AGrid[ X2, Y2 ].NextX;
              Y1 := AGrid[ X2, Y2 ].NextY;
            until ( X1 = X ) and ( Y1 = Y );
            AGrid[ X2, Y2 ].NextX := AGrid[ X, Y ].NextX;
            AGrid[ X2, Y2 ].NextY := AGrid[ X, Y ].NextY;
          end;
        end;
        if D0 < ClosestD then
        begin
          ClosestD := D0;
          ClosestX := X;
          ClosestY := Y;
        end;
        AGrid[ X, Y ].Iteration := Iteration;
        AGrid[ X, Y ].Closed := false;
        AGrid[ X, Y ].FromX := CurrentX;
        AGrid[ X, Y ].FromY := CurrentY;
        AGrid[ X, Y ].D := AGrid[ CurrentX, CurrentY ].D + 1;
        AGrid[ X, Y ].NextX := InitD[ D ].NextX;
        AGrid[ X, Y ].NextY := InitD[ D ].NextY;
        InitD[ D ].NextX := X;
        InitD[ D ].NextY := Y;
        InitD[ D ].Iteration := Iteration;
        if ( D < MinD ) then
          MinD := D;
      end;
    end;
  end;
end;

function TAStar.FindPath( SrcX, SrcY, DestX, DestY : Smallint; var Handle : HGLOBAL ) : word;
const
  FailName : string = 'TAStar.FindPath';
var
  X, Y : Smallint;
  X1, Y1 : Smallint;
  X2, Y2 : Smallint;
  D : longint;
  p : ^TPoint;
  DirX, DirY : integer;
  dX, dY : integer;
  dX2, dY2 : integer;
  h, h1, h3 : double;
  a1, a3 : double;
  LoopCount : longint;
  //  a2,h2: double; //Creates a tighter path which is not appropriate for 8 direction walk
  FavorDiagonal : boolean;
begin
  inc( Iteration );
  FSrcX := SrcX;
  FSrcY := SrcY;
  FDestX := DestX;
  FDestY := DestY;
  X := SrcX;
  Y := SrcY;
  dX := iabs( FDestX - FSrcX );
  dY := iabs( FDestY - FSrcY );
  if ( dX > dY ) then
    MinD := dX
  else
    MinD := dY;
  StartD := MinD;
  AGrid[ X, Y ].D := 0;
  AGrid[ X, Y ].Iteration := Iteration;
  AGrid[ X, Y ].FromX := X;
  AGrid[ X, Y ].FromY := Y;
  ClosestD := MinD;
  ClosestX := X;
  ClosestY := Y;

  if dX < dY then
    FavorDiagonal := ( dY - dX ) < dX
  else
    FavorDiagonal := ( dX - dY ) < dY;

  LoopCount := 0;
  repeat
    AGrid[ X, Y ].Closed := true;
    InitD[ MinD ].NextX := AGrid[ X, Y ].NextX;
    InitD[ MinD ].NextY := AGrid[ X, Y ].NextY;

    if ( X > DestX ) then
      DirX := -1
    else
      DirX := 1;
    if ( Y > DestY ) then
      DirY := -1
    else
      DirY := 1;
    X1 := X + DirX;
    Y1 := Y + DirY;
    X2 := X - DirX;
    Y2 := Y - DirY;

    dX := X - DestX;
    dY := Y - DestY;
    dX2 := dX * dX;
    dY2 := dY * dY;
    if ( dX = 0 ) and ( dY = 0 ) then
      h := 1
    else
      h := dY2 / ( dX2 + dY2 );
    if ( dx = 0 ) or ( dY = 0 ) then
      FavorDiagonal := false;

    dX := X1 - DestX;
    dX2 := dX * dX;
    if ( dX = 0 ) and ( dY = 0 ) then
      h1 := 0
    else
      h1 := dY2 / ( dX2 + dY2 );

    dY := Y1 - DestY;
    dY2 := dY * dY;
    //    if (dX=0) and (dY=0) then h2:=0
    //    else h2:=dY2/(dX2+dY2);

    dX := X - DestX;
    dX2 := dX * dX;
    if ( dX = 0 ) and ( dY = 0 ) then
      h3 := 0
    else
      h3 := dY2 / ( dX2 + dY2 );

    a1 := abs( h1 - h );
    //    a2:=abs(h2-h);
    a3 := abs( h3 - h );

    //Open cells in order of least favorite since the cells are stored LIFO
{    if ((a2<=a1) and (a2<=a3)) then begin
      OpenCell(X,Y,X2,Y2);
      if (a1<a3) then begin
        OpenCell(X,Y,X2,Y);
        OpenCell(X,Y,X,Y2);
        OpenCell(X,Y,X2,Y1);
        OpenCell(X,Y,X1,Y2);
        OpenCell(X,Y,X,Y1);
        OpenCell(X,Y,X1,Y);
      end
      else begin
        OpenCell(X,Y,X,Y2);
        OpenCell(X,Y,X2,Y);
        OpenCell(X,Y,X1,Y2);
        OpenCell(X,Y,X2,Y1);
        OpenCell(X,Y,X1,Y);
        OpenCell(X,Y,X,Y1);
      end;
      OpenCell(X,Y,X1,Y1);
    end
    else }
    if FavorDiagonal then
    begin
      if ( a1 < a3 ) then
      begin
        OpenCell( X, Y, X2, Y2 );
        OpenCell( X, Y, X2, Y1 );
        OpenCell( X, Y, X1, Y2 );
        OpenCell( X, Y, X2, Y );
        OpenCell( X, Y, X, Y2 );
        OpenCell( X, Y, X, Y1 );
        OpenCell( X, Y, X1, Y );
        OpenCell( X, Y, X1, Y1 );
      end
      else
      begin
        OpenCell( X, Y, X2, Y2 );
        OpenCell( X, Y, X1, Y2 );
        OpenCell( X, Y, X2, Y1 );
        OpenCell( X, Y, X, Y2 );
        OpenCell( X, Y, X2, Y );
        OpenCell( X, Y, X1, Y );
        OpenCell( X, Y, X, Y1 );
        OpenCell( X, Y, X1, Y1 );
      end;
    end
    else
    begin
      if ( a1 < a3 ) then
      begin
        OpenCell( X, Y, X2, Y2 );
        OpenCell( X, Y, X2, Y1 );
        OpenCell( X, Y, X1, Y2 );
        OpenCell( X, Y, X2, Y );
        OpenCell( X, Y, X, Y2 );
        OpenCell( X, Y, X1, Y1 );
        OpenCell( X, Y, X, Y1 );
        OpenCell( X, Y, X1, Y );
      end
      else
      begin
        OpenCell( X, Y, X2, Y2 );
        OpenCell( X, Y, X1, Y2 );
        OpenCell( X, Y, X2, Y1 );
        OpenCell( X, Y, X, Y2 );
        OpenCell( X, Y, X2, Y );
        OpenCell( X, Y, X1, Y1 );
        OpenCell( X, Y, X1, Y );
        OpenCell( X, Y, X, Y1 );
      end;
    end;

    D := MinD;
    repeat
      X := InitD[ D ].NextX;
      Y := InitD[ D ].NextY;
      if ( AGrid[ X, Y ].Closed ) or ( AGrid[ X, Y ].Iteration <> Iteration ) or ( InitD[ D ].Iteration <> Iteration ) then
      begin
        repeat
          inc( D );
          if ( D > HeapSize ) or ( ( D - StartD ) > Deviance ) then
          begin
            result := 0; //No path found
            X := ClosestX;
            Y := ClosestY;
            repeat
              X1 := AGrid[ X, Y ].FromX;
              Y1 := AGrid[ X, Y ].FromY;
              X := X1;
              Y := Y1;
              inc( result );
            until ( X = SrcX ) and ( Y = SrcY );

            Handle := GlobalAlloc( GMEM_MOVEABLE, result * sizeof( TPoint ) );
            p := GlobalLock( Handle );
            inc( p, result );
            X := ClosestX;
            Y := ClosestY;
            repeat
              dec( p );
              p^.X := X;
              p^.Y := Y;
              X1 := AGrid[ X, Y ].FromX;
              Y1 := AGrid[ X, Y ].FromY;
              X := X1;
              Y := Y1;
            until ( X = SrcX ) and ( Y = SrcY );
            GlobalUnlock( Handle );

            exit;
          end;
        until ( InitD[ D ].Iteration = Iteration );
        X := InitD[ D ].NextX;
        Y := InitD[ D ].NextY;
      end;

    until not ( AGrid[ X, Y ].Closed );

    MinD := D;
    inc( LoopCount );
  until ( X = DestX ) and ( Y = DestY );

  result := 0;
  repeat
    X1 := AGrid[ X, Y ].FromX;
    Y1 := AGrid[ X, Y ].FromY;
    X := X1;
    Y := Y1;
    inc( result );
  until ( X = SrcX ) and ( Y = SrcY );

  Handle := GlobalAlloc( GMEM_MOVEABLE, result * sizeof( TPoint ) );
  p := GlobalLock( Handle );
  inc( p, result );
  X := DestX;
  Y := DestY;
  repeat
    dec( p );
    p^.X := X;
    p^.Y := Y;
    X1 := AGrid[ X, Y ].FromX;
    Y1 := AGrid[ X, Y ].FromY;
    X := X1;
    Y := Y1;
  until ( X = SrcX ) and ( Y = SrcY );
  GlobalUnlock( Handle );

end;

function TAStar.FindJaggedPath( SrcX, SrcY, DestX, DestY : Smallint; var Handle : HGLOBAL ) : word;
const
  FailName : string = 'TAStar.FindJaggedPath';
var
  X, Y : Smallint;
  X1, Y1 : Smallint;
  X2, Y2 : Smallint;
  D : longint;
  p : ^TPoint;
  DirX, DirY : integer;
  dX, dY : integer;
  dX2, dY2 : integer;
  h, h1, h3 : double;
  a1, a3 : double;
  a2, h2 : double; //Creates a tighter path which is not appropriate for 8 direction walk
begin
  inc( Iteration );
  FSrcX := SrcX;
  FSrcY := SrcY;
  FDestX := DestX;
  FDestY := DestY;
  X := SrcX;
  Y := SrcY;
  dX := iabs( FDestX - FSrcX );
  dY := iabs( FDestY - FSrcY );
  if ( dX > dY ) then
    MinD := dX
  else
    MinD := dY;
  StartD := MinD;
  AGrid[ X, Y ].D := 0;
  AGrid[ X, Y ].Iteration := Iteration;
  AGrid[ X, Y ].FromX := X;
  AGrid[ X, Y ].FromY := Y;
  ClosestD := MinD;
  ClosestX := X;
  ClosestY := Y;

  repeat
    AGrid[ X, Y ].Closed := true;
    InitD[ MinD ].NextX := AGrid[ X, Y ].NextX;
    InitD[ MinD ].NextY := AGrid[ X, Y ].NextY;

    if ( X > DestX ) then
      DirX := -1
    else
      DirX := 1;
    if ( Y > DestY ) then
      DirY := -1
    else
      DirY := 1;
    X1 := X + DirX;
    Y1 := Y + DirY;
    X2 := X - DirX;
    Y2 := Y - DirY;

    dX := X - DestX;
    dY := Y - DestY;
    dX2 := dX * dX;
    dY2 := dY * dY;
    if ( dX = 0 ) and ( dY = 0 ) then
      h := 1
    else
      h := dY2 / ( dX2 + dY2 );

    dX := X1 - DestX;
    dX2 := dX * dX;
    if ( dX = 0 ) and ( dY = 0 ) then
      h1 := 0
    else
      h1 := dY2 / ( dX2 + dY2 );

    dY := Y1 - DestY;
    dY2 := dY * dY;
    if ( dX = 0 ) and ( dY = 0 ) then
      h2 := 0
    else
      h2 := dY2 / ( dX2 + dY2 );

    dX := X - DestX;
    dX2 := dX * dX;
    if ( dX = 0 ) and ( dY = 0 ) then
      h3 := 0
    else
      h3 := dY2 / ( dX2 + dY2 );

    a1 := abs( h1 - h );
    a2 := abs( h2 - h );
    a3 := abs( h3 - h );

    if ( ( a2 <= a1 ) and ( a2 <= a3 ) ) then
    begin
      OpenCell( X, Y, X2, Y2 );
      if ( a1 < a3 ) then
      begin
        OpenCell( X, Y, X2, Y );
        OpenCell( X, Y, X, Y2 );
        OpenCell( X, Y, X2, Y1 );
        OpenCell( X, Y, X1, Y2 );
        OpenCell( X, Y, X, Y1 );
        OpenCell( X, Y, X1, Y );
      end
      else
      begin
        OpenCell( X, Y, X, Y2 );
        OpenCell( X, Y, X2, Y );
        OpenCell( X, Y, X1, Y2 );
        OpenCell( X, Y, X2, Y1 );
        OpenCell( X, Y, X1, Y );
        OpenCell( X, Y, X, Y1 );
      end;
      OpenCell( X, Y, X1, Y1 );
    end
    else
    begin
      if ( a1 < a3 ) then
      begin
        OpenCell( X, Y, X2, Y2 );
        OpenCell( X, Y, X2, Y1 );
        OpenCell( X, Y, X1, Y2 );
        OpenCell( X, Y, X2, Y );
        OpenCell( X, Y, X, Y2 );
        OpenCell( X, Y, X1, Y1 );
        OpenCell( X, Y, X, Y1 );
        OpenCell( X, Y, X1, Y );
      end
      else
      begin
        OpenCell( X, Y, X2, Y2 );
        OpenCell( X, Y, X1, Y2 );
        OpenCell( X, Y, X2, Y1 );
        OpenCell( X, Y, X, Y2 );
        OpenCell( X, Y, X2, Y );
        OpenCell( X, Y, X1, Y1 );
        OpenCell( X, Y, X1, Y );
        OpenCell( X, Y, X, Y1 );
      end;
    end;

    D := MinD;
    repeat
      X := InitD[ D ].NextX;
      Y := InitD[ D ].NextY;
      if ( AGrid[ X, Y ].Closed ) or ( AGrid[ X, Y ].Iteration <> Iteration ) or ( InitD[ D ].Iteration <> Iteration ) then
      begin
        repeat
          inc( D );
          if ( D > HeapSize ) or ( ( D - StartD ) > Deviance ) then
          begin
            result := 0; //No path found
            X := ClosestX;
            Y := ClosestY;
            repeat
              X1 := AGrid[ X, Y ].FromX;
              Y1 := AGrid[ X, Y ].FromY;
              X := X1;
              Y := Y1;
              inc( result );
            until ( X = SrcX ) and ( Y = SrcY );

            Handle := GlobalAlloc( GMEM_MOVEABLE, result * sizeof( TPoint ) );
            p := GlobalLock( Handle );
            inc( p, result );
            X := ClosestX;
            Y := ClosestY;
            repeat
              dec( p );
              p^.X := X;
              p^.Y := Y;
              X1 := AGrid[ X, Y ].FromX;
              Y1 := AGrid[ X, Y ].FromY;
              X := X1;
              Y := Y1;
            until ( X = SrcX ) and ( Y = SrcY );
            GlobalUnlock( Handle );

            exit;
          end;
        until ( InitD[ D ].Iteration = Iteration );
        X := InitD[ D ].NextX;
        Y := InitD[ D ].NextY;
      end;

    until not ( AGrid[ X, Y ].Closed );

    MinD := D;
  until ( X = DestX ) and ( Y = DestY );

  result := 0;
  repeat
    X1 := AGrid[ X, Y ].FromX;
    Y1 := AGrid[ X, Y ].FromY;
    X := X1;
    Y := Y1;
    inc( result );
  until ( X = SrcX ) and ( Y = SrcY );

  Handle := GlobalAlloc( GMEM_MOVEABLE, result * sizeof( TPoint ) );
  p := GlobalLock( Handle );
  inc( p, result );
  X := DestX;
  Y := DestY;
  repeat
    dec( p );
    p^.X := X;
    p^.Y := Y;
    X1 := AGrid[ X, Y ].FromX;
    Y1 := AGrid[ X, Y ].FromY;
    X := X1;
    Y := Y1;
  until ( X = SrcX ) and ( Y = SrcY );
  GlobalUnlock( Handle );

end;

constructor TAStar.Create;
const
  FailName : string = 'TAStar.Create';
var
  i, j : integer;
begin
  inherited;

  for j := MinSearch to MaxSearch do
  begin
    for i := MinSearch to MaxSearch do
    begin
      AGrid[ i, j ].NotEdge := true;
    end;
  end;
end;

end.


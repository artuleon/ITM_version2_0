unit Ucoords;

{-------------------------------------------------------------------}
{                    Unit:    Ucoords.pas                           }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/20/22                              }
{                                                                   }
{   Delphi Pascal unit containing methods that transform            }
{   Study Area Map coordinates and determine min/max                }
{   coordinate extents.                                             }
{-------------------------------------------------------------------}

interface

uses SysUtils, Math, Uglobals, Uproject, Uutils, Uvertex;

procedure GetCoordExtents(var X1: Extended; var Y1: Extended;
                          var X2: Extended; var Y2: Extended);
procedure TransformCoords(LL1, UR1, LL2, UR2: TExtendedPoint);

implementation

var
  Xscale: Extended;
  Yscale: Extended;


procedure GetCoordExtents(var X1: Extended; var Y1: Extended;
                          var X2: Extended; var Y2: Extended);
//-----------------------------------------------------------------------------
// Finds min & max coordinates of all map objects.
//-----------------------------------------------------------------------------

  procedure AdjustExtents(var Zmin: Extended; var Zmax: Extended);
  //-------------------------------------------------------------
  // Adjusts min & max map extent in case they are the same value
  //-------------------------------------------------------------
  var
    Dz: Extended;
  begin
    if Zmin = 0 then
    begin
      Zmin := -5;
      Zmax := 5;
    end
    else
    begin
      Dz := 0.05*Abs(Zmax);
      Zmin := Zmin - Dz;
      Zmax := Zmax + Dz;
    end;
  end;

var
  Z: Extended;
  I: Integer;
  J: Integer;
begin
  X1 := -MISSING;
  X2 := MISSING;
  Y1 := -MISSING;
  Y2 := MISSING;
  for I := 0 to MAXCLASS do
  begin

    // Node locations
    if Project.IsNode(I) then
    begin
      for J := 0 to Project.Lists[I].Count - 1 do
      begin
        Z := Project.GetNode(I, J).X;
        if (Z <> MISSING) then
        begin
          X1 := Min(X1, Z);
          X2 := Max(X2, Z);
        end;
        Z := Project.GetNode(I, J).Y;
        if (z <> MISSING) then
        begin
          Y1 := Min(Y1, Z);
          Y2 := Max(Y2, Z);
        end;
      end;
    end;
  end;

  if (X1 <> -MISSING) and (X2 <> MISSING) then
  begin
    if X1 = X2 then AdjustExtents(X1, X2);
    Z := 0.05*(X2 - X1);
    X1 := X1 - Z;
    X2 := X2 + Z;
  end;
  if (Y1 <> -MISSING) and (Y2 <> MISSING) then
  begin
    if Y1 = Y2 then AdjustExtents(Y1, Y2);
    Z := 0.05*(Y2 - Y1);
    Y1 := Y1 - Z;
    Y2 := Y2 + Z;
  end;
end;


procedure TransformCoords(LL1, UR1, LL2, UR2: TExtendedPoint);
//-----------------------------------------------------------------------------
//  Transforms the coordinates of all map objects from a bounding
//  rectangle defined by coordinates LL1 (lower left) and UR1 (upper
//  right) to one defined by LL2 and UR2.
//-----------------------------------------------------------------------------

  function Xtransform(X: Extended): Extended;
  //-----------------------------------------
  //  Performs an X-coordinate transform.
  //-----------------------------------------
  begin
    if X = MISSING then Result := MISSING
    else Result := LL2.X + (X - LL1.X) * Xscale;
  end;

  function Ytransform(Y: Extended): Extended;
  //-----------------------------------------
  //  Performs a Y-coordinate transform.
  //-----------------------------------------
  begin
    if Y = MISSING then Result := MISSING
    else Result := LL2.Y + (Y - LL1.Y) * Yscale;
  end;

var
  I : Integer;
  J : Integer;
  V : PVertex;
  aNode : TNode;
  aLink : TLink;
  aLabel: TMapLabel;

begin
  if UR1.X = LL1.X then Exit;
  if UR1.Y = LL1.Y then Exit;

  // Find relative scaling between the two coordinate systems
  Xscale := (UR2.X - LL2.X) / (UR1.X - LL1.X);
  Yscale := (UR2.Y - LL2.Y) / (UR1.Y - LL1.Y);

  // Examine each class of object
  for I := 0 to MAXCLASS do
  begin
    if Project.IsNode(I) then
    begin
      for J := 0 to Project.Lists[I].Count - 1 do
      begin
        aNode := Project.GetNode(I, J);
        aNode.X := Xtransform(aNode.X);
        aNode.Y := Ytransform(aNode.Y);
      end;
    end

    else if Project.IsLink(I) then
    begin
      for J := 0 to Project.Lists[I].Count - 1 do
      begin
        aLink := Project.GetLink(I, J);
        V := aLink.Vlist.First;
        while V <> nil do
        begin
          V^.X := Xtransform(V^.X);
          V^.Y := Ytransform(V^.Y);
          V := V^.Next;
        end;
      end;
    end

    else if I = MAPLABEL then
    begin
      for J := 0 to Project.Lists[I].Count - 1 do
      begin
        aLabel := Project.GetMapLabel(J);
        aLabel.X := Xtransform(aLabel.X);
        aLabel.Y := Ytransform(aLabel.Y);
      end;
    end;
  end;
end;

end.

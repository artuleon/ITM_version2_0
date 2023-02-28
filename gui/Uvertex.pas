unit Uvertex;

{-------------------------------------------------------------------}
{                    Unit:    Uvertex.pas                           }
{                    Version: 1.0                                   }
{                    Date:    5/6/05                                }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit that defines and implements the              }
{   TVertexList class. This is a linked list of two-dimensional     }
{   points that define the vertices of a polyline or polygon shape. }
{-------------------------------------------------------------------}

interface

uses
  SysUtils, Classes, Dialogs;

type

//----------------------
// Vertex data structure
//----------------------
  PVertex = ^TVertex;
  TVertex = record
    X    : Extended;
    Y    : Extended;
    Next : PVertex;
  end;

//-------------------
// Vertex List class
//-------------------
  TVertexList = class(TObject)
    Vfirst   : PVertex;
    Vcurrent : PVertex;
    constructor Create;
    destructor  Destroy; override;
    function    Count: Integer;
    function    First: PVertex;
    function    Next:  PVertex;
    function    Find(X1,X2,Y1,Y2: Extended): PVertex;
    function    Add(X,Y: Extended): PVertex;
    function    Delete(aVertex: PVertex): PVertex;
    procedure   Clear;
    procedure   Update(X,Y: Extended);
    procedure   Reverse;
    procedure   Move(DX,DY: Extended);
    procedure   Assign(aVlist: TVertexList);
  end;

implementation

//-------------------------
// VertexList class methods
//-------------------------

constructor TVertexList.Create;
//-----------------------------------------------------------------------------
// Constructor method.
//-----------------------------------------------------------------------------
begin
  inherited Create;
  Vfirst := nil;
  Vcurrent := nil;
end;

destructor TVertexList.Destroy;
//-----------------------------------------------------------------------------
// Destructor method.
//-----------------------------------------------------------------------------
begin
  Clear;
  inherited Destroy;
end;

procedure TVertexList.Clear;
//-----------------------------------------------------------------------------
// Clears and deletes all vertices in the list.
//-----------------------------------------------------------------------------
begin
  Vcurrent := Vfirst;
  while Vcurrent <> nil do
  begin
    Vfirst := Vcurrent^.Next;
    Dispose(Vcurrent);
    Vcurrent := Vfirst;
  end;
end;

function TVertexList.Count: Integer;
//-----------------------------------------------------------------------------
// Counts the number of vertices in the list.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  Result := 0;
  V := Vfirst;
  while V <> nil do
  begin
    Inc(Result);
    V := V^.Next;
  end;
end;

function TVertexList.First: PVertex;
//-----------------------------------------------------------------------------
//  Returns the first vertex in the list.
//-----------------------------------------------------------------------------
begin
  Vcurrent := Vfirst;
  Result := Vcurrent;
end;

function TVertexList.Next: PVertex;
//-----------------------------------------------------------------------------
// Returns the next vertex in the list.
//-----------------------------------------------------------------------------
begin
  if (Vfirst = nil) or (Vcurrent^.Next = nil) then Result := nil
  else
  begin
    Vcurrent := Vcurrent^.Next;
    Result := Vcurrent;
  end;
end;

function TVertexList.Find(X1,X2,Y1,Y2: Extended): PVertex;
//-----------------------------------------------------------------------------
// Finds the vertex that lies within a rectangular region.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  V := Vfirst;
  while V <> nil do
  begin
    if (V^.X >= X1) and (V^.X <= X2)
    and (V^.Y >= Y1) and (V^.Y <= Y2) then break;
    V := V^.Next;
  end;
  if V <> nil then Vcurrent := V;
  Result := V;
end;

function TVertexList.Add(X,Y: Extended): PVertex;
//-----------------------------------------------------------------------------
// Adds a new vertex to the list.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  New(V);
  V^.X := X;
  V^.Y := Y;
  if Vcurrent = nil then Vcurrent := Vfirst;
  if Vfirst = nil then
  begin
    V^.Next := nil;
    Vfirst := V;
  end
  else
  begin
    V^.Next := Vcurrent^.Next;
    Vcurrent^.Next := V;
  end;
  Vcurrent := V;
  Result := V;
end;

function TVertexList.Delete(aVertex: PVertex): PVertex;
//-----------------------------------------------------------------------------
// Deletes a vertex form the list.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  Result := nil;
  if (aVertex = nil) then exit;
  if Vfirst = aVertex then
  begin
    Vfirst := Vfirst^.Next;
    Dispose(aVertex);
    Vcurrent := Vfirst;
  end
  else
  begin
    V := Vfirst;
    while V <> nil do
    begin
      if V^.Next = aVertex then
      begin
        V^.Next := aVertex^.Next;
        Dispose(aVertex);
        Vcurrent := V;
        break;
      end;
      V := V^.Next;
    end;
  end;
  Result := Vcurrent;
end;

procedure TVertexList.Update(X,Y: Extended);
//-----------------------------------------------------------------------------
// Updates the X,Y coordinates of the current vertex.
//-----------------------------------------------------------------------------
begin
  if Vcurrent <> nil then
  begin
    Vcurrent^.X := X;
    Vcurrent^.Y := Y;
  end;
end;

procedure TVertexList.Reverse;
//-----------------------------------------------------------------------------
// Reverses the order of the vertices in the list.
//-----------------------------------------------------------------------------
var
  V, Vprior, Vnext: PVertex;
begin
  Vprior := nil;
  V := Vfirst;
  while V <> nil do
  begin
    Vfirst := V;
    Vnext := V^.Next;
    V^.Next := Vprior;
    Vprior := V;
    V := Vnext;
  end;
end;

procedure TVertexList.Move(DX,DY: Extended);
//-----------------------------------------------------------------------------
// Moves each vertex in the list by the same amount.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  V := Vfirst;
  while V <> nil do
  begin
    V^.X := V^.X + DX;
    V^.Y := V^.Y + DY;
    V := V^.Next;
  end;
end;

procedure TVertexList.Assign(aVlist: TVertexList);
//-----------------------------------------------------------------------------
// Replaces the list's vertices with those from another list.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  Clear;
  V := aVlist.First;
  while V <> nil do
  begin
    Add(V^.X, V^.Y);
    V := aVlist.Next;
  end;
end;

end.

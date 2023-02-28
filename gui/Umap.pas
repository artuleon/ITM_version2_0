unit Umap;

{-------------------------------------------------------------------}
{                    Unit:    Umap.pas                              }
{                    Project: ITM                                   }
{                    Version: 1.5                                   }
{                    Date:    10/20/22                              }
{                                                                   }
{   Delphi Pascal unit that defines the TMap object. This object    }
{   contains drawing methods for rendering the Study Area Map on a  }
{   memory bitmap. It also draws a backdrop image, draws the map    }
{   legends, identifies the bounding rectangle for an object, and   }
{   handles map re-scaling.                                         }
{-------------------------------------------------------------------}

interface

uses
  Windows, Graphics, SysUtils, Dialogs, Forms, Classes,
  Controls, Math, Jpeg, System.Types, System.UITypes,
  Uglobals, Uproject, Uutils, Uvertex;

const
  MAX_INT_COORD = 32767;
  MAX_POINTS    = 1000;

type

  // These types are used for watermarking the backdrop image
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[WORD] of TRGBTriple;

  // Map physical dimensions
  TMapDimensions = record
    LowerLeft    : TExtendedPoint; //Lower left corner of map
    UpperRight   : TExtendedPoint; //Upper right corner of map
    XperDeg      : Extended;       //Meters per degree longitude
    YperDeg      : Extended;       //Meters per degree latitude
    LengthUCF    : Extended;       //Length units conversion factor
    AreaUCF      : Extended;       //Area units conversion factor
    Units        : TMapUnits;      //Length units
    Digits       : Integer;        //Decimal digits in XY-coords.
  end;

  // Map display window scaling
  TMapWindow = record
    WPPx       : Extended;         //World X coord. per pixel
    WPPy       : Extended;         //World Y coord. per pixel
    Woffset    : TExtendedPoint;   //World coord. offset
    Poffset    : TPoint;           //Pixel coord. offset
    Pwidth     : Integer;          //Full window width
    Pheight    : Integer;          //Full window height
    MapRect    : TRect;            //Current window rect
  end;

  // Map zoom-in state
  TMapZoomState = record
    ZoomFactor : Extended;
    Xcenter    : Extended;
    Ycenter    : Extended;
  end;

  // Map backdrop image
  TMapBackDrop = record
    Filename   : String;           //File containing backdrop image
    LowerLeft  : TExtendedPoint;   //World coords. of lower-left of image
    UpperRight : TExtendedPoint;   //World coords. of upper-right of image
    Watermark  : Boolean;          //True if backdrop drawn as watermark
    Visible    : Boolean;          //True if backdrop is visible
  end;

  // Map display options
  TMapOptions   = record
    ShowGageIDs       : Boolean;

    ShowSubcatchIDs   : Boolean;
    ShowSubcatchValues: Boolean;
    ShowSubcatchLinks : Boolean;
    SubcatchFillStyle : Integer;
    SubcatchLineSize  : Integer;
    SubcatchSnapTol   : Integer;
    SubcatchSize      : Integer;

    ShowNodeIDs       : Boolean;
    ShowNodeValues    : Boolean;
    ShowNodesBySize   : Boolean;
    ShowNodeBorder    : Boolean;
    NodeSize          : Integer;

    ShowLinkIDs       : Boolean;
    ShowLinkValues    : Boolean;
    ShowLinksBySize   : Boolean;
    ShowLinkBorder    : Boolean;
    LinkSize          : Integer;

    ShowGages         : Boolean;
    ShowSubcatchs     : Boolean;
    ShowNodes         : Boolean;
    ShowLinks         : Boolean;
    ShowNodeSymbols   : Boolean;
    ShowLinkSymbols   : Boolean;

    ShowLabels        : Boolean;
    LabelsTranspar    : Boolean;
    NotationTranspar  : Boolean;
    NotationSize      : Integer;

    ArrowStyle        : TArrowStyle;
    ArrowSize         : Integer;
    ColorIndex        : Integer;

    NotationZoom      : Integer;
    LabelZoom         : Integer;
    SymbolZoom        : Integer;
    ArrowZoom         : Integer;
  end;

const

  DefMapDimensions: TMapDimensions =
    (LowerLeft : (X:0.00; Y:0.00);
     UpperRight: (X:10000.00; Y:10000.00);
     XperDeg   : 111195; //Meters per degree of longitude on spherical earth
     YperDeg   : 111195;
     LengthUCF : 1.0;
     AreaUCF   : 2.2956e-5;  // acres per sq. foot
     Units     : muNone;
     Digits    : 3);

  DefMapBackdrop: TMapBackdrop =
    (Filename  : '';
     LowerLeft : (X:0.00; Y:0.00);
     UpperRight: (X:0.00; Y:0.00);
     Watermark : False;
     Visible   : False);

  DefMapOptions: TMapOptions =
    (ShowGageIDs        : False;
     ShowSubcatchIDs    : False;
     ShowSubcatchValues : False;
     ShowSubcatchLinks  : True;
     SubcatchFillStyle  : Ord(bsBDiagonal);
     SubcatchLineSize   : 1;
     SubcatchSnapTol    : 0;
     SubcatchSize       : 5;
     ShowNodeIDs        : False;
     ShowNodeValues     : False;
     ShowNodesBySize    : False;
     ShowNodeBorder     : True;
     NodeSize           : 3;
     ShowLinkIDs        : False;
     ShowLinkValues     : False;
     ShowLinksBySize    : False;
     ShowLinkBorder     : False;
     LinkSize           : 1;
     ShowGages          : True;
     ShowSubcatchs      : True;
     ShowNodes          : True;
     ShowLinks          : True;
     ShowNodeSymbols    : True;
     ShowLinkSymbols    : True;
     ShowLabels         : True;
     LabelsTranspar     : True;
     NotationTranspar   : False;
     NotationSize       : 7;
     ArrowStyle         : asNone;
     ArrowSize          : 2;
     ColorIndex         : 1;
     NotationZoom       : 100;
     LabelZoom          : 100;
     SymbolZoom         : 100;
     ArrowZoom          : 100);

var
  Points  : array[0..MAX_POINTS] of TPoint;

type

  TLinkSymbol = (lsArrow, lsPump, lsValve, lsCheckValve);

  TMap = class(TObject)          // Map object
    Canvas    : TCanvas;         // Display canvas
    Bitmap    : TBitmap;         // Bitmap containing the drawn map
    BackBM    : TBitmap;         // Bitmap containing backdrop image
    GageBM    : TBitmap;         // Bitmap containing raingage image
    Window    : TMapWindow;      // Display window sizing info
    Dimensions: TMapDimensions;  // Physical map dimensions
    Options   : TMapOptions;     // Display options
    Backdrop  : TMapBackdrop;    // Backdrop image info
    WPPx0     : Extended;        // World X per pixel scaling at 0 zoom
    WPPy0     : Extended;        // World Y per pixel scaling at 0 zoom
    ZoomState : array [0..10] of TMapZoomState;
    ZoomIndex : Integer;         // Current zoom-in level
    ZoomRatio : Integer;
    constructor Create;
    destructor  Destroy; override;

    procedure BrightenBackdrop;
    procedure ClearMap;
    procedure ConvertMapUnits(var Dx, Dy: Extended);
    procedure DrawArrow(L: TLink; P1: TPoint; P2: TPoint);
    procedure DrawArrowHead(const X: Integer; const Y: Integer;
              const Style: TArrowStyle; const Size: Integer;
              const Aratio: Single; const Asin: Extended;
              const Acos: Extended);
    function  DrawBackdrop(BDCanvas: TCanvas): Boolean;
    procedure DrawGate(const X: Integer; const Y: Integer; const Size: Integer);
    procedure DrawForeground;
    procedure DrawInflowSymbol(const X: Integer; const Y: Integer; const Size: Integer);
    procedure DrawLabels;
    procedure DrawLink(const P1: TPoint; const P2: TPoint; L: TLink);
    procedure DrawLinkIDLabel(const ObjType: Integer; const Index: Integer;
              const P1: TPoint; const P2: TPoint; const Size: Integer);
    procedure DrawLinks;
    procedure DrawLinkSymbol(const P1: TPoint; const P2: TPoint;
              const Symbol: TLinkSymbol);
    procedure DrawLinkValueLabel(const ObjType: Integer; const Index: Integer;
              const P1: TPoint; const P2: TPoint; const Size: Integer;
              const S: String);
    procedure DrawMap;
    procedure DrawNode(const X: Integer; const Y: Integer;
              const Size: Integer);
    procedure DrawNodeIDLabel(const ObjType: Integer; const Index: Integer;
              const P: TPoint; const Size: Integer);
    procedure DrawNodes;
    procedure DrawNodeValueLabel(const ObjType: Integer; const Index: Integer;
              const P: TPoint; const Size: Integer; const S: String);
    procedure DrawObject(const ObjType: Integer; const Index: Integer);
    procedure DrawBoundary(const X: Integer; const Y: Integer;
              const Size: Integer);
    procedure DrawOutline(const LineWidth: Integer; const LineColor: TColor);
    procedure DrawPumpSymbol(const X: Integer; const Y: Integer;
              const Size: Integer; const Direction: Integer;
              const Asin: Extended; const Acos: Extended);
    procedure DrawStorage(const X: Integer; const Y: Integer;
              const Size: Integer);
    procedure DrawWeir(const X: Integer; const Y: Integer;
              const Size: Integer);

    function  GetAdjacencyRect(const ObjType: Integer; const Index: Integer;
              const DeleteLinks: Boolean): TRect;
    function  GetArea(Points: array of TPoint; N: Integer): Extended;
    procedure GetBackdropBounds(const PicWidth: Integer;
              const PicHeight: Integer);
    function  GetBoundingRect(const ObjType: Integer;
              const Index: Integer): TRect;
    function  GetDistance(Points: array of TPoint; N: Integer): Extended;
    function  GetLabelRect(L: TMapLabel): TRect;
    function  GetLinkLengthStr(const LinkType: Integer;
              const Index: Integer): String;
    function  GetLinkMidpoint(L: TLink; var P: TPoint): Boolean;
    procedure GetLinkMidsegment(L: TLink; var Pa, Pb: TPoint);
    function  GetLinkRect(L: TLink): TRect;
    function  GetNodePixPos(N: TNode; var P: TPoint): Boolean;
    function  GetNodePoint(N: TNode): TPoint;
    function  GetNodeRect(N: TNode): TRect;
    function  GetX(const X: Integer): Extended;
    function  GetY(const Y: Integer): Extended;
    function  GetXpix(const X: Extended): Integer;
    function  GetYpix(const Y: Extended): Integer;

    procedure GoDrawNode(const ObjType: Integer; const Index: Integer);
    procedure GoDrawLink(const ObjType: Integer; const Index: Integer);

{    function  IsBounded(const A, B, C: Extended): Boolean;  }
    function  RedrawBackdrop: Boolean;
    procedure Rescale;
    procedure Resize(const Rect: TRect);

    procedure ResizeBitmap(var Bmap: TBitmap; const W,H: Integer);
    procedure ResizeWindow(const Rect: TRect);

    procedure SetLinkColor(const ObjType: Integer; const Index: Integer);
    function  SetLinkSize: Integer;
    procedure SetNodeColor(const ObjType: Integer; const Index: Integer);
    function  SetNodeSize: Integer;
    procedure UpdateBounds(var R: TRect; const P: TPoint);

  end;

implementation

uses
  Uoutput;

var
  P1, P2: TPoint;
  LastColorIndex: Integer;
  CharHeight: Integer;
  BackColor: TColor;
  ForeColor: TColor;
  GrayColor: TColor;


//=============================================================================
//                         TMap Constructor & Destructor
//=============================================================================

constructor TMap.Create;
begin
  inherited Create;
  Bitmap := TBitmap.Create;
  if Bitmap <> nil then Canvas := Bitmap.Canvas;
  BackBM := TBitmap.Create;
  Bitmap.PixelFormat := pf24Bit;
  BackBM.PixelFormat := pf24Bit;
  GageBM := TBitmap.Create;
  GageBM.Transparent := False;
  Dimensions := DefMapDimensions;
  Options := DefMapOptions;
  Backdrop := DefMapBackdrop;
  ZoomIndex := 0;
  ZoomState[0].ZoomFactor := 1.0;
end;

destructor TMap.Destroy;
begin
  Bitmap.Free;
  BackBM.Free;
  GageBM.Free;
  inherited Destroy;
end;


//=============================================================================
//                         Backdrop Drawing Methods
//=============================================================================

function TMap.RedrawBackdrop: Boolean;
//-----------------------------------------------------------------------------
//  Redraws backdrop image on the Map object's backdrop bitmap.
//-----------------------------------------------------------------------------
begin
  if Backdrop.Visible
  then Result := DrawBackdrop(BackBM.Canvas)
  else Result := True;
end;


procedure TMap.GetBackdropBounds(const PicWidth: Integer;
  const PicHeight: Integer);
//-----------------------------------------------------------------------------
//  Finds bounding rectangle of backdrop that fills map extent
//  while preserving its aspect ratio.
//-----------------------------------------------------------------------------
var
  Wpic   : Extended;
  Hpic   : Extended;
  Wwin   : Extended;
  Hwin   : Extended;
  R      : Extended;
begin
  // Compute picture & window width & height in world coords.
  Wpic := PicWidth * WPPx0;
  Hpic := PicHeight * WPPy0;
  Wwin := Window.Pwidth * WPPx0;
  Hwin := Window.Pheight * WPPy0;

  // Re-scale if picture is wider or taller than map window
  if Wpic > Wwin then
  begin
    R := Wwin / Wpic;
    Wpic := R * Wpic;
    Hpic := R * Hpic;
  end;
  if Hpic > Hwin then
  begin
    R := Hwin / Hpic;
    Wpic := R * Wpic;
    Hpic := R * Hpic;
  end;

  // Set bottom left & top right coords. of backdrop
  with Backdrop do
  begin
    LowerLeft.X  := Dimensions.LowerLeft.X;
    UpperRight.X := LowerLeft.X + Wpic;
    UpperRight.Y := Dimensions.UpperRight.Y;
    LowerLeft.Y  := UpperRight.Y - Hpic;
  end;
end;


procedure TMap.BrightenBackdrop;
//-----------------------------------------------------------------------------
//  Brightens backdrop bitmap to create a watermark effect.
//-----------------------------------------------------------------------------
const
  BRIGHTNESS = 70;           // relative brightness setting
var
  Row : PRGBTripleArray;     // ptr. to array of R,G,B color values
  V   : Integer;             // new R, G, or B color value
  X, Y: Integer;             // pixel row & column
  newBitmap: TBitmap;        // copy of backdrop's bitmap
begin
  // Create a copy of the backdrop bitmap
  newBitmap := TBitmap.Create;
  try
    newBitmap.PixelFormat := pf24Bit;  // need this format for ScanLine function
    newBitmap.Assign(BackBM);

    // Inrease brightness of each color of each pixel of newBitmap
    with newBitmap do
    begin
      for Y := 0 to Height-1 do
      begin
        Row := ScanLine[Y];            // fast access to rows of pixels
        for X := 0 to Width-1 do
        begin
          with Row[X] do
          begin
            V := rgbtRed;
            V := V + (BRIGHTNESS*(255 - V) div 100);
            rgbtRed := Min(255, V);
            V := rgbtBlue;
            V := V + (BRIGHTNESS*(255 - V) div 100);
            rgbtBlue := Min(255, V);
            V := rgbtGreen;
            V := V + (BRIGHTNESS*(255 - V) div 100);
            rgbtGreen := Min(255, V);
          end;
        end;
      end;
    end;

    // Transfer brightened bitmap to backdrop bitmap
    BackBM.Canvas.Draw(0, 0, newBitmap);

  // Free the bitmap copy
  finally
    newBitmap.Free;
  end;
end;


function TMap.DrawBackdrop(BDCanvas: TCanvas): Boolean;
//-----------------------------------------------------------------------------
//  Draws map's backdrop image on specified Canvas.
//-----------------------------------------------------------------------------
var
  Picture     : TPicture;
  R           : TRect;
begin
  // Create a TPicture object to hold the backdrop image
  Result := False;
  Picture := TPicture.Create;

  if FileExists(Backdrop.FileName) then
  try

    // Retrieve the backdrop picture
    Picture.LoadFromFile(Backdrop.FileName);

    // Find coords. of bounding rectangle if none exist
    with Backdrop do
    begin
      if LowerLeft.X = UpperRight.X
      then GetBackdropBounds(Picture.Width, Picture.Height);
      R := Rect(GetXpix(LowerLeft.X),  GetYpix(UpperRight.Y),
                GetXpix(UpperRight.X), GetYpix(LowerLeft.Y));
    end;

    // Draw the picture in the canvas
    Uutils.Cls(BDCanvas, Window.MapRect, MapBackColor[Options.ColorIndex]);
    BDCanvas.StretchDraw(R, Picture.Graphic);
    if Backdrop.Watermark then BrightenBackdrop;
    Result := True;

  // Free the Picture
  finally
    Picture.Free;
  end;
end;


//=============================================================================
//                          Map Drawing Methods
//=============================================================================

procedure TMap.ClearMap;
//-----------------------------------------------------------------------------
//  Clears map to background color.
//-----------------------------------------------------------------------------
begin
  Uutils.Cls(Canvas,Window.MapRect,MapBackColor[Options.ColorIndex]);
end;


procedure TMap.DrawMap;
//-----------------------------------------------------------------------------
//  Draws entire map (backdrop + foreground) on current canvas.
//-----------------------------------------------------------------------------
begin
  ClearMap;
  if Backdrop.Visible
  then Canvas.CopyRect(Window.MapRect, BackBM.Canvas, Window.MapRect);
  DrawForeground;
end;


procedure TMap.DrawForeground;
//-----------------------------------------------------------------------------
//  Draws foreground of map (i.e., SWMM's visual objects) on current canvas.
//-----------------------------------------------------------------------------
begin
  // Assign values to global colors
  ForeColor := MapForeColor[Options.ColorIndex];
  BackColor := MapBackColor[Options.ColorIndex];
  GrayColor := MapGrayColor[Options.ColorIndex];

  // Setup pen & brush to draw on map's Canvas
  Canvas.Pen.Color := ForeColor;
  Canvas.Brush.Color := ForeColor;
  Canvas.Brush.Style := bsSolid;

  // Set Canvas's font for ID/Value labeling
  Canvas.Font.Name := 'Arial';
  Canvas.Font.Size := Options.NotationSize;
  Canvas.Font.Style := [];
  CharHeight := Canvas.TextHeight('[');

  // Draw links, nodes, & labels
  DrawLinks;
  DrawNodes;
  DrawLabels;
  LastColorIndex := -999;
end;


procedure TMap.DrawLinks;
//-----------------------------------------------------------------------------
//  Draws all conduits & pumps.
//-----------------------------------------------------------------------------
var
  I, J : Integer;
begin
  if Options.NotationTranspar then SetBkMode(Canvas.Handle, TRANSPARENT);
  LastColorIndex := -999;
  Canvas.Pen.Width := Options.LinkSize;
  if Options.ShowLinks then
  begin
    I := CONDUIT;
    for J := 0 to Project.Lists[I].Count - 1 do DrawObject(I, J);
  end;
  Canvas.Pen.Width := 1;
  SetBkMode(Canvas.Handle, OPAQUE);
end;


procedure TMap.DrawNodes;
//-----------------------------------------------------------------------------
//  Draws all junctions, boundaries, gates, outlets & storage units.
//-----------------------------------------------------------------------------
var
  I, J : Integer;
begin
  if Options.NotationTranspar then SetBkMode(Canvas.Handle, TRANSPARENT);
  LastColorIndex := -999;
  if Options.ShowNodes then for I := JUNCTION to STORAGE do
  begin
    for J := 0 to Project.Lists[I].Count - 1 do DrawObject(I, J);
  end;
  SetBkMode(Canvas.Handle, OPAQUE);
end;


procedure TMap.DrawLabels;
//-----------------------------------------------------------------------------
//  Draws all map labels.
//-----------------------------------------------------------------------------
var
  J: Integer;
  ZoomFlag: Boolean;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := BackColor;
  Canvas.Pen.Color := ForeColor;
  Canvas.Font.Color := ForeColor;
  if (Options.ShowLabels) then
  begin
    ZoomFlag := (ZoomRatio >= Options.LabelZoom);
    if Options.LabelsTranspar then SetBkMode(Canvas.Handle, TRANSPARENT);
    for J := 0 to Project.Lists[MAPLABEL].Count - 1 do
      if ZoomFlag then
      begin
        if Project.GetMapLabel(J).X <> MISSING then DrawObject(MAPLABEL, J);
      end;
    SetBkMode(Canvas.Handle, OPAQUE);
  end;
end;


procedure TMap.DrawOutline(const LineWidth: Integer; const LineColor: TColor);
//-----------------------------------------------------------------------------
//  Draws map outline for display of the Overview map.
//-----------------------------------------------------------------------------
var
  I, J    : Integer;
  L       : TLink;
begin
  // Setup canvas's pen
  LastColorIndex := -999;
  Canvas.Pen.Color := LineColor;
  Canvas.Pen.Width := 1;

  // Draw all link objects
  for I := 0 to MAXCLASS do
  begin
    if not Project.IsLink(I) then continue;
    for J := 0 to Project.Lists[I].Count - 1 do
    begin
      L := Project.GetLink(I, J);
      if not GetNodePixPos(L.Node1, P1) then Continue;
      if not GetNodePixPos(L.Node2, P2) then Continue;
      if not PtInRect(Window.MapRect, P1)
      and not PtInRect(Window.MapRect, P2) then Continue;
      DrawLink(P1, P2, L);
    end;
  end;
  Canvas.Pen.Width := 1;
end;


//=============================================================================
//                        Object Drawing Methods
//=============================================================================

procedure TMap.DrawObject(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Draws the object identified by ObjType and Index on map.
//-----------------------------------------------------------------------------
var
  R : TRect;
begin
  // Check if object falls within current display window
  // (GetBoundingRect also gets global variables P1 & P2
  // that define the object's bounding rectangle).
  R := GetBoundingRect(ObjType, Index);
  if not IntersectRect(R, R, Window.MapRect) then Exit;

  // Object is a Node
  if Project.IsNode(ObjType) then GoDrawNode(ObjType, Index)

  // Object is a Link
  else if Project.IsLink(ObjType) then GoDrawLink(ObjType, Index)

  // Object is a Label
  else if (ObjType = MAPLABEL) then
  begin
    with Project.GetMapLabel(Index), Canvas do
    begin
      Font.Name := FontName;
      Font.Size := FontSize;
      Font.Style := [];
      if FontBold then Font.Style := Font.Style + [fsBold];
      if FontItalic then Font.Style := Font.Style + [fsItalic];
      Canvas.TextOut(P1.X, P1.Y, Text);
    end;
  end;
end;


procedure TMap.GoDrawNode(const ObjType: Integer; const Index: Integer);
var
  Size  : Integer;
begin
  // Determine color & size
  SetNodeColor(ObjType, Index);
  Size := SetNodeSize;

  // Draw the node
  if (Options.ShowNodeSymbols) and (ZoomRatio >= Options.SymbolZoom) then
  begin
    if ObjType = STORAGE then DrawStorage(P1.X, P1.Y, Size)
    else if ObjType = GATE then DrawGate(P1.X, P1.Y, Size)
    else if ObjType = BOUNDARY then DrawBoundary(P1.X, P1.Y, Size)
    else if ObjType = WEIR then DrawWeir(P1.X, P1.Y, Size)
    else DrawNode(P1.X, P1.Y, Size);
    if Length(Project.GetNode(ObjType, Index).ExInflow.FlowType) > 0 then
      DrawInflowSymbol(P1.X, P1.Y, Size);
  end
  else
    DrawNode(P1.X, P1.Y, Size);

  // Add notation if called for
  if (ZoomRatio >= Options.NotationZoom) then
  begin
    if (Options.ShowNodeIDs) then DrawNodeIDLabel(ObjType, Index, P1, Size);
    if (Options.ShowNodeValues) and (CurrentNodeVar > NOVIEW)
    then DrawNodeValueLabel(ObjType, Index, P1, Size,
      Uoutput.GetNodeValStr(CurrentNodeVar, CurrentPeriod, ObjType, Index));
  end;
end;


procedure TMap.GoDrawLink(const ObjType: Integer; const Index: Integer);
var
  L : TLink;
  Size  : Integer;
  Offset: Integer;
  Color: TColor;
begin
  // Determine color & size and draw the link
  L := Project.GetLink(ObjType, Index);
  SetLinkColor(ObjType, Index);
  Size := SetLinkSize;

  // Draw border if called for
  // (Line size adjusted to insure that border gets displayed)
  if Options.ShowLinkBorder then
  begin
    Color := Canvas.Pen.Color;
    if Color <> ForeColor then
    begin
      Canvas.Pen.Color := ForeColor;
      if Size = 4 then Size := 3
      else if Size = 5 then Size := 6;
      Canvas.Pen.Width := Size + 2;
      DrawLink(P1, P2, L);
      Canvas.Pen.Color := Color;
    end;
  end;

  // Draw link in the proper color
  Offset := Size;
  Canvas.Pen.Width := Size;
  DrawLink(P1, P2, L);
  Canvas.Pen.Width := 1;
  GetLinkMidSegment(L, P1, P2);
  Canvas.Pen.Color := ForeColor;

  // Draw object's symbol & flow direction arrow if called for
  if ObjType = CONDUIT then
  begin
    if (Options.ShowLinkSymbols) and (ZoomRatio >= Options.SymbolZoom) and
      (L.HasPump = True) then
    begin
      DrawLinkSymbol(P1, P2, lsPump);
      Offset := Offset + SYMBOLSIZE;
    end
    else if (Options.ArrowStyle <> asNone) then
    begin
      DrawArrow(L, P1, P2);
      Offset := Offset + Options.ArrowSize div 2;
    end;

    // Add link notation if called for
    if (ZoomRatio >= Options.NotationZoom) then
    begin
      if (Options.ShowLinkIDs) then
        DrawLinkIDLabel(ObjType, Index, P1, P2, Offset);
      if (Options.ShowLinkValues) and (CurrentLinkVar > NOVIEW) then
         DrawLinkValueLabel(ObjType, Index, P1, P2, Offset,
           Uoutput.GetLinkValStr(CurrentLinkVar, CurrentPeriod, ObjType, Index));
    end;
  end;
end;


procedure TMap.DrawNode(const X: Integer; const Y: Integer;
  const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws a node at location X,Y with size Size.
//-----------------------------------------------------------------------------
begin
  Canvas.Ellipse(X-Size, Y-Size, X+Size+1, Y+Size+1);
end;

procedure TMap.DrawInflowSymbol(const X: Integer; const Y: Integer;
  const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws an Inflow symbol next to a node at location X,Y with size Size.
//-----------------------------------------------------------------------------
var
  Fac: Integer;
  X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer;
  Ymid: Integer;
begin
  Fac := 5;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clBlack;
  X1 := X+Size+2;
  Y1 := Y+Size+2;
  X2 := X+Fac*Size;
  Y2 := Y1;
  X3 := X2;
  Y3 := Y+Fac*Size;
  X4 := X1;
  Y4 := Y3;
  Ymid := Trunc(Fac*Size / 2);

  Canvas.MoveTo(X1, Y1);
  Canvas.LineTo(X2, Y2);
  Canvas.MoveTo(X2, Y2);
  Canvas.LineTo(X3, Y3);
  Canvas.MoveTo(X3, Y3);
  Canvas.LineTo(X4, Y4);
  Canvas.MoveTo(X4, Y4);
  Canvas.LineTo(X1, Y1);

  Canvas.MoveTo(X1+3, Y1+2);
  Canvas.LineTo(X1+3, Y3-1);
  Canvas.MoveTo(X1+3, Y1+Ymid-2);
  Canvas.LineTo(X2-3, Y1+Ymid-2);
  Canvas.MoveTo(X2-3, Y1+2);
  Canvas.LineTO(X2-3, Y3-1);
end;

procedure TMap.DrawBoundary(const X: Integer; const Y: Integer;
  const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws a Boundary node at location X,Y with size Size.
//-----------------------------------------------------------------------------
var
  Poly: array[0..2] of TPoint;
  W: Integer;
begin
  W := Size;
  Poly[0] := Point(X-W, Y-W);
  Poly[1] := Point(X, Y+W);
  Poly[2] := Point(X+W, Y-W);
  Canvas.Polygon(Poly);
end;


procedure TMap.DrawGate(const X: Integer; const Y: Integer; const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws a Gate node at location X,Y with size Size.
//-----------------------------------------------------------------------------
var
  Poly: array[0..3] of TPoint;
  W: Integer;
begin
  W := Size + 1;
  Poly[0] := Point(X, Y-W);
  Poly[1] := Point(X+W, Y);
  Poly[2] := Point(X, Y+W);
  Poly[3] := Point(X-W, Y);
  Canvas.Polygon(Poly);
end;


procedure TMap.DrawWeir(const X: Integer; const Y: Integer;  const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws a Gate node at location X,Y with size Size.
//-----------------------------------------------------------------------------
var
  Poly: array[0..4] of TPoint;
  W: Integer;
begin
  W := Size + 1;
  Poly[0] := Point(X-W, Y+W);
  Poly[1] := Point(X-W, Y-W);
  Poly[2] := Point(X, Y);
  Poly[3] := Point(X+W, Y-W);
  Poly[4] := Point(X+W, Y+W);
  Canvas.Polygon(Poly);
end;


procedure TMap.DrawStorage(const X: Integer; const Y: Integer;
                           const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws a Storage node at location X,Y with size Size.
//-----------------------------------------------------------------------------
var
  Poly: array[0..3] of TPoint;
  W: Integer;
begin
  if (Options.ShowNodeSymbols) and (ZoomRatio >= Options.SymbolZoom) then
  begin
    W := 2*Size;
    Poly[0] := Point(X-W, Y-W);
    Poly[1] := Point(X-W, Y+Size+1);
    Poly[2] := Point(X+W+1, Y+Size+1);
    Poly[3] := Point(X+W+1, Y-W-1);
    Canvas.PolyLine(Poly);
    Canvas.Rectangle(X-W, Y-Size, X+W+2, Y+Size+2);
  end
  else
    Canvas.Rectangle(X-Size, Y-Size, X+Size+1, Y+Size+1);
end;


procedure TMap.DrawLink(const P1: TPoint; const P2: TPoint; L: TLink);
//-----------------------------------------------------------------------------
//  Draws a link between points P1 and P2.
//-----------------------------------------------------------------------------
var
  V: PVertex;
begin
  Canvas.MoveTo(P1.X, P1.Y);
  if L <> nil then
  begin
    V := L.Vlist.First;
    while V <> nil do
    begin
      Canvas.LineTo(GetXPix(V^.X), GetYpix(V^.Y));
      V := V^.Next;
    end;
  end;
  Canvas.LineTo(P2.X, P2.Y);
end;


//=============================================================================
//                       Symbol Drawing Methods
//=============================================================================

procedure TMap.DrawArrow(L: TLink; P1: TPoint; P2: TPoint);
//-----------------------------------------------------------------------------
//  Draws flow direction arrow on link between points P1 and P2.
//-----------------------------------------------------------------------------
var
  K: Integer;
  Ptmp: TPoint;
begin
  // Check that zoom in scale is high enough to show arrows
  if (ZoomRatio < Options.ArrowZoom) then exit;

  // If results of a simulation run exist, reverse P1 and P2 for negative
  // flow direction
  if RunFlag then
  begin
    K := L.Zindex;
    if (K >= 0) then
    case FlowDir^[K] of

    NONE:
      Exit;

    MINUS:
      begin
        Ptmp := P1;
        P1 := P2;
        P2 := Ptmp;
      end;

    end;
  end;

  // Draw the arrow symbol on the link
  DrawLinkSymbol(P1, P2, lsArrow);
end;


procedure TMap.DrawLinkSymbol(const P1: TPoint; const P2: TPoint;
  const Symbol: TLinkSymbol);
//-----------------------------------------------------------------------------
//  Draws a symbol on the link between points P1 and P2.
//-----------------------------------------------------------------------------
var
  Asin,
  Acos   : Extended;
  Dx, Dy : Extended;
  X, Y   : Integer;
  Size   : Integer;
  Width  : Integer;
begin
  // Determine angle of inclination of symbol
  Dy := P2.Y - P1.Y;
  Dx := P2.X - P1.X;
  SinCos(arctan2(Dy, Dx), Asin, Acos);

  // Determine location & size of symbol
  X := (P1.X + P2.X) div 2;
  Y := (P1.Y + P2.Y) div 2;
  Width := Canvas.Pen.Width;
  Size := SYMBOLSIZE + Width;

  // Call symbol-specific drawing procedure
  case Symbol of

  lsArrow:
    begin
      with Options do
        DrawArrowHead(X, Y, ArrowStyle, ArrowSize, 0.5, Asin, Acos);
    end;

  lsPump:
    begin
      if (Dx >= 0)
      then DrawPumpSymbol(X, Y, Size, 1, Asin, Acos)
      else DrawPumpSymbol(X, Y, Size, -1, Asin, Acos);
    end;

  end;
  Canvas.Pen.Width := Width;
end;


procedure TMap.DrawArrowHead(const X: Integer; const Y: Integer;
  const Style: TArrowStyle; const Size: Integer; const Aratio: Single;
  const Asin: Extended; const Acos: Extended);
//-----------------------------------------------------------------------------
//  Draws arrowhead symbol starting from position X,Y.
//  Style = arrowhead style,
//  Size  = arrowhead style,
//  Aratio = ratio of arrowhead width to length
//  Asin, Acos = sine & cosine of angle of inclination of arrowhead.
//-----------------------------------------------------------------------------
var
  X1, X2: Integer;
  Y1, Y2: Integer;
  Poly  : array[0..3] of TPoint;
begin
  X1 := X + Round((-Acos + Aratio*Asin)*Size);
  Y1 := Y - Round((Asin + Aratio*Acos)*Size);
  X2 := X + Round((-Acos - Aratio*Asin)*Size);
  Y2 := Y - Round((Asin - Aratio*Acos)*Size);
  case Style of

  asOpen:
    begin
      Poly[0] := Point(X1, Y1);
      Poly[1] := Point(X, Y);
      Poly[2] := Point(X2, Y2);
      Canvas.PolyLine(Slice(Poly, 3));
    end;

  asFilled:
    begin
      Poly[0] := Point(X, Y);
      Poly[1] := Point(X1, Y1);
      Poly[2] := Point(X2, Y2);
      Canvas.Pen.Width := 1;
      Canvas.Polygon(Slice(Poly, 3));
    end;

  asFancy:
    begin
      Poly[0] := Point(X, Y);
      Poly[1] := Point(X1, Y1);
      X1 := X + Round((0.67*Acos)*Size);
      Y1 := Y - Round((-0.67*Asin)*Size);
      Poly[2] := Point(X1, Y1);
      Poly[3] := Point(X2, Y2);
      Canvas.Pen.Width := 1;
      Canvas.Polygon(Slice(Poly, 4));
    end;

  end;
end;


procedure TMap.DrawPumpSymbol(const X: Integer; const Y: Integer;
  const Size: Integer; const Direction: Integer; const Asin: Extended;
  const Acos: Extended);
//-----------------------------------------------------------------------------
//  Draws pump symbol centered at position X,Y.
//  Size  = arrowhead style,
//  Direction = orientation of pump
//  Asin, Acos = sine & cosine of angle of inclination of pump.
//-----------------------------------------------------------------------------
var
  Xi, Yi, R : Integer;
  Acolor    : TColor;
  Poly      : array[0..3] of TPoint;
begin
  Acolor := Canvas.Pen.Color;
  if Options.ShowNodeBorder then Canvas.Pen.Color := ForeColor;
  R := 2*Size;
  Poly[0] := Point(X, Y);
  Xi := X + Round(Acos*R);
  Yi := Y + Round(Asin*R);
  Poly[1] := Point(Xi, Yi);
  Xi := X + Round((Acos + 0.5*Asin*Direction)*R);
  Yi := Y + Round((Asin - 0.5*Acos*Direction)*R);
  Poly[2] := Point(Xi, Yi);
  Xi := X + Round(+0.5*Asin*R*Direction);
  Yi := Y + Round(-0.5*Acos*R*Direction);
  Poly[3] := Point(Xi, Yi);
  Canvas.Polygon(Slice(Poly, 4));
  Canvas.Ellipse(X-Size, Y-Size, X+Size, Y+Size);
  Canvas.Pen.Color := Acolor;
end;


//=============================================================================
//                        Label Drawing Methods
//=============================================================================


procedure TMap.DrawNodeIDLabel(const ObjType: Integer; const Index: Integer;
  const P: TPoint; const Size: Integer);
//-----------------------------------------------------------------------------
// Draws text of node ID label next to node.
//-----------------------------------------------------------------------------
var
  Acolor : TColor;
  Offset: Integer;
begin
  // Increase offset distance from node for tanks & reservoirs
  Offset := Size + CharHeight + 1;
  if (ObjType = STORAGE)
  and (Options.ShowNodeSymbols)
  and (ZoomRatio >= Options.SymbolZoom)
  then Offset := Offset + Size + 1;
  with Canvas do
  begin
    Acolor := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := GrayColor;
    if Options.NotationTranspar then SetBkMode(Handle, TRANSPARENT);
    TextOut(P.X, P.Y-Offset, Project.GetID(ObjType, Index));
    Brush.Color := Acolor;
  end;
end;


procedure TMap.DrawNodeValueLabel(const ObjType: Integer; const Index: Integer;
  const P: TPoint; const Size: Integer; const S: String);
//-----------------------------------------------------------------------------
//  Draws text of value of current node view variable next to node.
//-----------------------------------------------------------------------------
var
  Acolor : TColor;
  Offset : Integer;
begin
  if (Length(S) = 0) then Exit;
  Offset := Size + 2;
  if (ObjType = STORAGE)
  and (Options.ShowNodeSymbols)
  and (ZoomRatio >= Options.SymbolZoom)
  then Offset := Offset + Size;
  with Canvas do
  begin
    Acolor := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := Pen.Color;
    if Options.NotationTranspar then SetBkMode(Handle,TRANSPARENT);
    TextOut(P.X, P.Y+Offset, S);
    Brush.Color := Acolor;
  end;
end;


procedure TMap.DrawLinkIDLabel(const ObjType: Integer; const Index: Integer;
  const P1: TPoint; const P2: TPoint; const Size: Integer);
//-----------------------------------------------------------------------------
//  Draws text of link ID label at midpoint of link.
//-----------------------------------------------------------------------------
var
  Acolor : TColor;
  S      : String;
  X, Y   : Integer;
begin
  S := Project.GetID(ObjType, Index);
  X := (P1.X + P2.X) div 2;
  Y := (P1.Y + P2.Y) div 2;
  if (Abs(P2.Y - P1.Y) < Size) then
  begin
    X := X - (Canvas.TextWidth(S) div 2);
    Y := Y - Size - CharHeight;
  end
  else
  begin
    X := X - Canvas.TextWidth(S) - Size;
    if ((P1.Y < P2.Y) and (P1.X < P2.X))
    then Y := Y + Size
    else Y := Y - Size - CharHeight;
  end;
  with Canvas do
  begin
    Acolor := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := GrayColor;
    if Options.NotationTranspar then SetBkMode(Handle, TRANSPARENT);
    TextOut(X, Y, S);
    Brush.Color := Acolor;
  end;
end;


procedure TMap.DrawLinkValueLabel(const ObjType: Integer; const Index: Integer;
  const P1: TPoint; const P2: TPoint; const Size: Integer; const S: String);
//-----------------------------------------------------------------------------
//  Draws text of value of current link view variable at link midpoint.
//-----------------------------------------------------------------------------
var
  Acolor : TColor;
  X, Y   : Integer;
begin
  if (Length(S) = 0) then Exit;
  X := (P1.X + P2.X) div 2;
  Y := (P1.Y + P2.Y) div 2;
  if (Abs(P2.Y - P1.Y) < Size) then
  begin
    X := X - (Canvas.TextWidth(S) div 2);
    Y := Y + Size;
  end
  else
  begin
    X := X + Size;
    if ((P1.Y < P2.Y) and (P1.X < P2.X))
    then Y := Y - Size - CharHeight
    else Y := Y + Size;
  end;
  with Canvas do
  begin
    Acolor := Brush.Color;
    Brush.Color := BackColor;
    Font.Color := Pen.Color;
    if Options.NotationTranspar then SetBkMode(Handle, TRANSPARENT);
    TextOut(X, Y, S);
    Brush.Color := Acolor;
  end;
end;


//=============================================================================
//                    Object Sizing & Coloring Methods
//=============================================================================

procedure TMap.SetLinkColor(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Sets the color to be used for drawing a link.
//-----------------------------------------------------------------------------
var
  K      : Integer;
  Color : TColor;
begin
  // Retrieve the color stored for the link in the project database
  K := -1;
  if (CurrentLinkVar = NOVIEW)
  then Color := MapGrayColor[Options.ColorIndex]
  else begin
    K := Project.GetLink(ObjType, Index).ColorIndex;
    if K < 0 then Color := MapGrayColor[Options.ColorIndex]
    else if QueryFlag then Color := QueryColor
    else Color := MapLinkColor[K];
  end;

  // Update the color used to draw with.
  Canvas.Pen.Color := Color;
  Canvas.Brush.Color := Color;
  LastColorIndex := K;
end;


procedure TMap.SetNodeColor(const ObjType: Integer; const Index: Integer);
//-----------------------------------------------------------------------------
//  Sets the color to be used for drawing a node.
//-----------------------------------------------------------------------------
var
  K      : Integer;
  Color : TColor;
begin
  // Retrieve the color stored for the node in the project database
  K := -1;
  if (CurrentNodeVar = NOVIEW)
  then Color := MapGrayColor[Options.ColorIndex]
  else begin
    K := Project.GetNode(ObjType, Index).ColorIndex;
    if K < 0 then Color := MapGrayColor[Options.ColorIndex]
    else if QueryFlag then Color := QueryColor
    else Color := MapNodeColor[K];
  end;

  // If this color is different than the last one used to draw with
  // then update the color used to draw with.
  if K <> LastColorIndex then
  begin
    if Options.ShowNodeBorder then Canvas.Pen.Color := ForeColor
    else Canvas.Pen.Color := Color;
    Canvas.Brush.Color := Color;
    LastColorIndex := K;
  end;
end;


function TMap.SetLinkSize: Integer;
//-----------------------------------------------------------------------------
//  Sets the size used to draw a link with.
//-----------------------------------------------------------------------------
begin
  with Options do
    if ShowLinksBySize
    then Result := LinkSize + 2*LastColorIndex
    else Result := LinkSize;
end;


function TMap.SetNodeSize: Integer;
//-----------------------------------------------------------------------------
//  Sets the size used to draw a node with.
//-----------------------------------------------------------------------------
begin
  with Options do
    if ShowNodesBySize
    then Result := NodeSize + LastColorIndex
    else Result := NodeSize;
end;


//=============================================================================
//                    Object Bounding & Location Methods
//=============================================================================

procedure TMap.UpdateBounds(var R: TRect; const P: TPoint);
//-----------------------------------------------------------------------------
//  Updates the size of rectangle R to include point P.
//-----------------------------------------------------------------------------
begin
  if P.X < R.Left   then R.Left := P.X;
  if P.X > R.Right  then R.Right := P.X;
  if P.Y < R.Top    then R.Top := P.Y;
  if P.Y > R.Bottom then R.Bottom := P.Y;
end;


function TMap.GetBoundingRect(const ObjType: Integer;
  const Index: Integer): TRect;
//-----------------------------------------------------------------------------
//  Returns the bounding rectangle for item Index of type ObjType.
//  Also saves the pixel locations of the top-left & bottom-right
//  points in global variables P1 and P2.
//-----------------------------------------------------------------------------
begin
  if Project.IsNode(ObjType)
  then Result := GetNodeRect(Project.GetNode(ObjType, Index))
  else if Project.IsLink(ObjType)
  then Result := GetLinkRect(Project.GetLink(ObjType, Index))
  else if ObjType = MAPLABEL
  then Result := GetLabelRect(Project.GetMapLabel(Index))
  else Result := NORECT;
end;


function TMap.GetNodeRect(N: TNode): TRect;
//-----------------------------------------------------------------------------
//  Gets the bounding rectangle for a node. The rectangle is centered
//  at node's pixel position and extends a distance on all sides
//  equal to the node size option plus the height of notation
//  labels (CharHeight).
//-----------------------------------------------------------------------------
var
  Pbuf : Integer;
begin
   Pbuf := Options.NodeSize + CharHeight;
   P1 := GetNodePoint(N);
   Result := Rect(P1.X-Pbuf, P1.Y-Pbuf, P1.X+Pbuf+1, P1.Y+Pbuf+1);
end;


function TMap.GetLinkRect(L: TLink): TRect;
//-----------------------------------------------------------------------------
//  Gets the bounding rectangle for a link. The rectangle equals the
//  pixel extent of all nodes & vertices that comprise the link and is
//  then enlarged by the size of link symbols (or flow arrows)
//  plus the link's width.
//-----------------------------------------------------------------------------
var
  Pbuf : Integer;
  P    : TPoint;
  R    : TRect;
  V    : PVertex;
begin
  Pbuf := MaxIntValue([SYMBOLSIZE,Options.ArrowSize]) + Options.LinkSize;
  Result := NORECT;
  if not GetNodePixPos(L.Node1, P1) then Exit;
  if not GetNodePixPos(L.Node2, P2) then Exit;
  R := Rect(P1.x, P1.y, P1.x+1, P1.y+1);
  V := L.Vlist.First;
  while V <> nil do
  begin
    P := Point(GetXPix(V^.X), GetYpix(V^.Y));
    UpdateBounds(R, P);
    V := V^.Next;
  end;
  UpdateBounds(R,P2);
  OffsetRect(R, -Pbuf, -Pbuf);
  InflateRect(R, 2*Pbuf, 2*Pbuf);
  Result := R;
end;


function TMap.GetLabelRect(L: TMapLabel): TRect;
//-----------------------------------------------------------------------------
//  Gets the bounding rectangle for a label.
//-----------------------------------------------------------------------------
var
  H, W      : Integer;
  Xa, Ya    : Single;
  F         : TFont;
  S         : TSize;
begin
    with L do
    begin

      // Find world coordinates of label's anchor node
      // (if no anchor node then these are same coordinates as label)
      if Anchor = nil then
      begin
        Xa := X;
        Ya := Y;
      end
      else
      begin
        Xa := Anchor.X;
        Ya := Anchor.Y;
      end;

      // Find pixel coordinates of upper left of label's bounding rectangle
      with Window do
      begin
        if ZoomIndex = 0 then
        begin
          P1.X := GetXpix(X);
          P1.Y := GetYpix(Y);
        end
        else
        begin
          P1.X := GetXpix(Xa) + Round((X-Xa)/WPPx0);
          P1.Y := GetYpix(Ya) + Round((Ya-Y)/WPPy0);
        end;
      end;
    end;

    // Create a Font object to determine label's width & height
    F := TFont.Create;
    try
      F.Assign(Canvas.Font);
      L.GetFont(Canvas.Font);
      S := Canvas.TextExtent(L.Text);
      W := S.cx;
      H := S.cy;
      Canvas.Font.Assign(F);
    finally
      F.Free;
    end;

    // Construct the label's bounding rectangle
    Result := Rect(P1.X, P1.Y, P1.X + W, P1.Y + H);
end;


procedure TMap.GetLinkMidSegment(L: TLink; var Pa, Pb: TPoint);
//-----------------------------------------------------------------------------
//  Gets the pixel coordinates of the vertices that define the
//  midpoint segment of the link (aLink).
//-----------------------------------------------------------------------------
var
  M, N   : Integer;
  Va, Vb : PVertex;
begin
  N := L.Vlist.Count;
  if N > 0 then
  begin
    N := (N div 2) + 1;
    Va := nil;
    Vb := L.Vlist.First;
    for M := 2 to N do
    begin
      Va := Vb;
      Vb := Vb^.Next;
    end;
    if Va <> nil then
      Pa := Point(GetXPix(Va^.X), GetYpix(Va^.Y));
    if Vb <> nil then
      Pb := Point(GetXPix(Vb^.X), GetYpix(Vb^.Y));
  end;
end;


function TMap.GetLinkMidpoint(L: TLink; var P: TPoint): Boolean;
//-----------------------------------------------------------------------------
//  Gets the pixel coordinates (P) of the midpoint of a link (aLink).
//-----------------------------------------------------------------------------
var
  Pa, Pb: TPoint;
begin
  Result := False;
  if not GetNodePixPos(L.Node1, Pa)
  or not GetNodePixPos(L.Node2, Pb) then Exit;
  GetLinkMidSegment(L, Pa, Pb);
  P.X := (Pa.X + Pb.X) div 2;
  P.Y := (Pa.Y + Pb.Y) div 2;
  Result := True;
end;


function TMap.GetAdjacencyRect(const ObjType: Integer; const Index: Integer;
  const DeleteLinks: Boolean): TRect;
//-----------------------------------------------------------------------------
//  Finds the rectangle encompassing all nodes adjacent to a given node.
//  If DeleteLinks = True, then the connecting links are deleted (as
//  when the node is being deleted from the project).
//-----------------------------------------------------------------------------
var
  I, J   : Integer;
  R      : TRect;
  Links  : TStringList;
  N      : TNode;
begin
  Result := GetBoundingRect(ObjType, Index);
  N := Project.GetNode(ObjType, Index);
  I := CONDUIT;
  begin
    Links := Project.Lists[I];
    for J := Links.Count-1 downto 0 do
    begin
      if (Project.GetLink(I, J).Node1 = N)
      or (Project.GetLink(I, J).Node2 = N)
      then begin
        R := GetBoundingRect(I, J);
        if IntersectRect(R, R, Window.MapRect)
        then UnionRect(Result, Result, R);
        if DeleteLinks then
        begin
          Project.DeleteItem(I, J);
        end;
      end;
    end;
  end;
end;


function TMap.GetXpix(const X: Extended):Integer;
//-----------------------------------------------------------------------------
// Converts world coordinate X to a screen pixel value.
//-----------------------------------------------------------------------------
var
  P: Extended;
begin
  with Window do
  try
    P := (X-Woffset.X) / WPPx;
    Result := Round(P) + Poffset.X;
  except
    On EInvalidOp do Result := MAX_INT_COORD;
  end;
end;


function TMap.GetYpix(const Y: Extended):Integer;
//-----------------------------------------------------------------------------
// Converts world coordinate Y to a screen pixel value.
//-----------------------------------------------------------------------------
var
  P: Extended;
begin
  with Window do
  try
    P := (Y-Woffset.Y) / WPPy;
    Result := Poffset.Y - Round(P);
  except
    On EInvalidOp do Result := MAX_INT_COORD;
  end;
end;


function  TMap.GetX(const X: Integer): Extended;
//-----------------------------------------------------------------------------
// Converts a screen pixel location to an X world coordinate value.
//-----------------------------------------------------------------------------
begin
  with Window do
    Result := (X - Poffset.X) * WPPx + Woffset.X;
end;


function  TMap.GetY(const Y: Integer): Extended;
//-----------------------------------------------------------------------------
// Converts a screen pixel location to a Y world coordinate value.
//-----------------------------------------------------------------------------
begin
  with Window do
    Result := (Poffset.Y - Y) * WPPy + Woffset.Y;
end;


function TMap.GetNodePoint(N: TNode): TPoint;
//-----------------------------------------------------------------------------
//  Returns the X,Y pixel location of a node object.
//-----------------------------------------------------------------------------
var
  X,Y : Extended;
begin
  X := N.X;
  Y := N.Y;
  if (X = MISSING) or (Y = MISSING)
  then Result := NOPOINT
  else Result := Point(GetXpix(X),GetYpix(Y));
end;


function TMap.GetNodePixPos(N: TNode; var P: TPoint): Boolean;
//-----------------------------------------------------------------------------
//  Finds the pixel coordinates (P) of a node (N).
//  Returns False if the node's coordinates are missing.
//-----------------------------------------------------------------------------
begin
  P := NOPOINT;
  Result := False;
  if N = nil then Exit;
  if (N.X = MISSING) or (N.Y = MISSING) then Exit;
  P := Point(GetXpix(N.X), GetYpix(N.Y));
  Result := True;
end;

{
function TMap.IsBounded(const A, B, C: Extended): Boolean;
//-----------------------------------------------------------------------------
//  Determines if C is between A and B
//-----------------------------------------------------------------------------
begin
  if (C < A) or (C > B) then Result := False
  else Result := True;
end;
}

//=============================================================================
//                Map Resizing & Re-scaling Methods
//=============================================================================

procedure TMap.Resize(const Rect: TRect);
//-----------------------------------------------------------------------------
//  Resizes the map's display window & its bitmaps.
//-----------------------------------------------------------------------------
begin
  ResizeWindow(Rect);
  ResizeBitmap(Bitmap, Window.Pwidth, Window.Pheight);
  ResizeBitmap(BackBM, Window.Pwidth, Window.Pheight);
end;


procedure TMap.ResizeWindow(const Rect: TRect);
//-----------------------------------------------------------------------------
//  Resizes the map's display window.
//-----------------------------------------------------------------------------
begin
  with Window do
  begin
    MapRect := Rect;
    Pwidth := MapRect.Right - MapRect.Left;
    Pheight := MapRect.Bottom - MapRect.Top;
    Poffset.X := MapRect.Left;
    Poffset.Y := MapRect.Bottom;
  end;
end;


procedure TMap.ResizeBitmap(var Bmap: TBitmap; const W,H: Integer);
//-----------------------------------------------------------------------------
//  Resizes the map's bitmap.
//-----------------------------------------------------------------------------
begin
  if Bmap <> nil then
  begin
    Bmap.Width := W;
    Bmap.Height := H;
  end;
end;


procedure TMap.Rescale;
//-----------------------------------------------------------------------------
//  Resets the map's dimensions and scale factors.
//-----------------------------------------------------------------------------
var
  Dx, Dy   : Extended;
begin
  // Compute world distance units per pixel in the X & Y directions
  Dx := Dimensions.UpperRight.X - Dimensions.LowerLeft.X;
  Dy := Dimensions.UpperRight.Y - Dimensions.LowerLeft.Y;
  WPPx0 := Dx/Window.Pwidth;
  WPPy0 := Dy/Window.Pheight;

  // Adjust scaling to maintain a 1:1 aspect ratio
  if WPPy0 > WPPx0 then WPPx0 := WPPy0
  else WPPy0 := WPPx0;

  // Compute the location of the map center at full scale
  ZoomState[0].Xcenter := Dimensions.LowerLeft.X + Dx/2.0;
  ZoomState[0].Ycenter := Dimensions.LowerLeft.Y + Dy/2.0;

  // Compute a scaling for the current zoom state
  with Window do
  begin
    WPPx := WPPx0 * ZoomState[ZoomIndex].ZoomFactor;
    WPPy := WPPy0 * ZoomState[ZoomIndex].ZoomFactor;
    Woffset.X := ZoomState[ZoomIndex].Xcenter - WPPx * (Pwidth/2.0);
    Woffset.Y := ZoomState[ZoomIndex].Ycenter - WPPy * (Pheight/2.0);
  end;
end;


procedure TMap.ConvertMapUnits(var Dx, Dy: Extended);
begin
  with Dimensions do
    if Units = muDegrees then
    begin
      Dx := Dx*XperDeg;
      Dy := Dy*YperDeg;
    end;
end;


function TMap.GetLinkLengthStr(const LinkType: Integer;
  const Index: Integer): String;
//-----------------------------------------------------------------------------
// Returns a string with the length of a particular link.
//-----------------------------------------------------------------------------
var
  L      : TLink;
  V      : PVertex;
  Length : Extended;
  X1, X2 : Extended;
  Y1, Y2 : Extended;
  Dx, Dy : Extended;
begin
  L := Project.GetLink(LinkType, Index);
  Length := 0;
  X1 := L.Node1.X;
  Y1 := L.Node1.Y;
  V  := L.Vlist.First;
  while V <> nil do
  begin
    X2 := V^.X;
    Y2 := V^.Y;
    Dx := X2 - X1;
    Dy := Y2 - Y1;
    ConvertMapUnits(Dx, Dy);
    Length := Length + Sqrt(Sqr(Dx) + Sqr(Dy));
    X1 := X2;
    Y1 := Y2;
    V  := V^.Next;
  end;
  X2 := L.Node2.X;
  Y2 := L.Node2.Y;
  Dx := X2 - X1;
  Dy := Y2 - Y1;
  ConvertMapUnits(Dx, Dy);
  Length := Length + Sqrt(Sqr(Dx) + Sqr(Dy));
  Length := Dimensions.LengthUCF*Length;
  Result := Format('%.2f',[Length]);
end;


function TMap.GetDistance(Points: array of TPoint; N: Integer): Extended;
//-----------------------------------------------------------------------------
// Finds the distance in project units of a polyline given in pixel units.
//-----------------------------------------------------------------------------
var
  I: Integer;
  X, Y, X1, Y1: Extended;
  D, Dx, Dy: Extended;
begin
  D := 0.0;
  if N >= 2 then
  begin
    X1 := GetX(Points[0].X);
    Y1 := GetY(Points[0].Y);
    for I := 1 to N-1 do
    begin
      X := GetX(Points[I].X);
      Y := GetY(Points[I].Y);
      Dx := X - X1;
      Dy := Y - Y1;
      ConvertMapUnits(Dx, Dy);
      D := D + Sqrt(Sqr(Dx) + Sqr(Dy));
      X1 := X;
      Y1 := Y;
    end;
  end;
  Result := D * Dimensions.LengthUCF;
end;


function TMap.GetArea(Points: array of TPoint; N: Integer): Extended;
//-----------------------------------------------------------------------------
//  Computes the area in project units of a polygon region whose
//  coordinates are in pixels.
//-----------------------------------------------------------------------------
var
  I:  Integer;
  X:  Extended;
  Y:  Extended;
  A:  Extended;
  X1, Y1: Extended;
  Xmin, Ymin: Extended;
begin
  // There must be at least 3 points to compute an area
  A := 0.0;
  if N >= 3 then
  begin

    // Find min. X & Y of all points
    X := GetX(Points[0].X);
    Y := GetY(Points[0].Y);
    Xmin := X;
    Ymin := Y;
    for I := 1 to N-1 do
    begin
      X := GetX(Points[I].X);
      Y := GetY(Points[I].Y);
      if X < Xmin then Xmin := X;
      if Y < Ymin then Ymin := Y;
    end;

    // Compute terms of the area formula using scaled X & Y values
    X := GetX(Points[0].X) - Xmin;
    Y := GetY(Points[0].Y) - Ymin;
    for I := 1 to N-1 do
    begin
      X1 := GetX(Points[I].X) - Xmin;
      Y1 := GetY(Points[I].Y) - Ymin;
      A := A + X*Y1 - Y*X1;
      X := X1;
      Y := Y1;
    end;
    A := Abs(A)/2.0;

    // Convert from map units to project units
    with Dimensions do
    begin
      if Units = muDegrees then A := A*XperDeg*YperDeg;
      A := A * AreaUCF;
    end;
  end;
  Result := A;
end;

end.

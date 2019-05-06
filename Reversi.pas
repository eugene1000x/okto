unit reversi;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Grids, StdCtrls, ExtCtrls, ImgList, ComCtrls, ShellCtrls;
const Side=8;
type
  TPlayerID=1..2;TState=0..2;TNextMove=0..2;
  TSide=1..Side;
  TField=array[TSide,TSide] of TState;
  TMove=record i,j:TSide;end;
  TPlayer=record Name:string;GetMove:Procedure(var Move:TMove;Field:TField;Piece:TPlayerID);end;
  TPlayers=array [1..2] of TPlayer;
  TNumberOfSquares=0..Side*Side;
  TBranchPosition=record Move:TMove;Field:TField;end;
  TBranchPositions=array[TNumberOfSquares]of TBranchPosition;
  TPosition=record Field:TField;
                   NextMove:TNextMove;
                   NumberOfMoves:TNumberOfSquares;
                   BranchPositions:TBranchPositions;
            end;
  TValue=record WinLossDraw,LoE,MoE:boolean;Pieces:integer;end;
  TTransPos=array [1..2,1..15] of record
                {MoveID,Depth}    NumberOfPos:word;
                                  Positions:array of record
                                                     Field:TField;
                                                     Value:TValue;
                                                     end;
                                  end;
  TProtocol=record Fields:array[TNumberOfSquares] of record Field:TField;
                                                            PlayerID:TPlayerID;
                                                     end;
                   NumberOfMoves,Current:TNumberOfSquares;
            end;
  TForm1 = class(TForm)
    DrawGrid1: TDrawGrid;
    MainMenu1: TMainMenu;
    New: TMenuItem;
    Player2: TImage;
    Player1: TImage;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    StatusBar1: TStatusBar;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    ProgressBar1: TProgressBar;
    Order: TMenuItem;
    PP: TMenuItem;
    PC: TMenuItem;
    CP: TMenuItem;
    CC: TMenuItem;
    Image1: TImage;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    Image2: TImage;
    N1: TMenuItem;
    N11: TMenuItem;
    N21: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N5: TMenuItem;
    N4: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    Green: TImage;
    Blue: TImage;
    Yellow: TImage;
    Red: TImage;
    N10: TMenuItem;
    BSetup: TMenuItem;
    Continue: TMenuItem;
    UpDown3: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure NewClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MoveClick(Sender: TObject);
    procedure Color1Click(Sender: TObject);
    procedure Color2Click(Sender: TObject);
    procedure BSetupClick(Sender: TObject);
    procedure ContinueClick(Sender: TObject);
    procedure DrawGrid1Click(Sender: TObject);
    procedure UpDown3Click(Sender: TObject; Button: TUDBtnType);
  private
    { Private declarations }
  public
   Field:TField;
   Positions:array[1..2]of TPosition;
   Players:TPlayers;
   Values: record last,best:TMove;V:array [1..8,1..8] of string;end;
   Protocol:TProtocol;
   setup,break:boolean;
       { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
procedure clear(var field:TField);
var i,j:byte;
begin
for i:=1 to 8 do
 for j:=1 to 8 do
  field[i,j]:=0;
end;

procedure setfield(var field:TField);
begin
field[4,4]:=2;field[5,4]:=1;field[4,5]:=1;field[5,5]:=2;
end;

procedure GetPosition(Field:TField;NextMove:TNextMove;var Position:TPosition);
var i,j:TSide;revcount:TNumberOfSquares;
    k,l:byte;
    vx,vy:-1..1;
begin
Position.NumberOfMoves:=0;Position.Field:=Field;Position.NextMove:=0;
for i:=1 to 8 do
 for j:=1 to 8 do
  begin
   revcount:=0;
   Position.BranchPositions[Position.NumberOfMoves+1].Field:=Field;
   for vx:=-1 to 1 do
    for vy:=-1 to 1 do
     if (Field[i,j]=0)and(not((vx=0)and(vy=0))) then
      if (i+vx in [1..8])and(j+vy in [1..8])and(Field[i+vx,j+vy]=3-NextMove) then
       begin
        k:=2;
        while (i+k*vx in [1..8])and(j+k*vy in [1..8])and(Field[i+k*vx,j+k*vy]=3-NextMove) do
         inc(k);
        if (i+k*vx in [1..8])and(j+k*vy in [1..8])and(Field[i+k*vx,j+k*vy]=NextMove) then
         begin
          for l:=0 to k-1 do
           Position.BranchPositions[Position.NumberOfMoves+1].field[i+l*vx,j+l*vy]:=NextMove;
          inc(revcount,k-1);
         end;
       end;
   if revcount>0 then
    begin inc(Position.NumberOfMoves);Position.BranchPositions[Position.NumberOfMoves].Move.i:=i;
            Position.BranchPositions[Position.NumberOfMoves].Move.j:=j;Position.NextMove:=NextMove;end;
  end;
end;

function Pieces(Field:tfield;piece:byte):byte;
var i,j:TSide;
begin
result:=0;
for i:=1 to Side do
 for j:=1 to Side do
  if field[i,j]=piece then
   inc(result);
end;

function midgame(field:tfield):boolean;
begin
result:=pieces(field,1)+pieces(field,2)<56;
end;

function StaticValue(Field:TField;Piece:TPlayerId):TValue;
const g:array[1..4] of array[1..2] of 1..8=((1,1),(1,8),(8,1),(8,8));
const k:array[1..4] of array[1..2] of 1..8=((2,2),(2,7),(7,2),(7,7));
var i:byte;Positions:array[1..2]of TPosition;
begin
result.Pieces:=0;result.WinLossDraw:=false;result.LoE:=false;result.MoE:=false;
GetPosition(Field,Piece,Positions[piece]);
GetPosition(Field,3-Piece,Positions[3-piece]);

if (Positions[piece].NumberOfMoves=0)and(Positions[3-piece].NumberOfMoves=0) then
 begin result.WinLossDraw:=true;result.Pieces:=pieces(Field,piece)-pieces(Field,3-piece);exit;end;

if midgame(Field) then
 result.Pieces:=pieces(Field,3-piece)-pieces(Field,piece)
else
 result.Pieces:=pieces(Field,piece)-pieces(Field,3-piece);

result.Pieces:=result.Pieces+Positions[piece].NumberOfMoves;
result.Pieces:=result.Pieces-Positions[3-piece].NumberOfMoves;

for i:=1 to 4 do
 if (Field[g[i][1],g[i][2]]=piece)or((Field[k[i][1],k[i][2]]=3-piece)and(Field[g[i][1],g[i][2]]=0)) then
  result.Pieces:=result.Pieces+100
 else
  if (Field[g[i][1],g[i][2]]=3-piece)or((Field[k[i][1],k[i][2]]=piece)and(Field[g[i][1],g[i][2]]=0)) then
   result.Pieces:=result.Pieces-100;
end;

function BetterOrEqual(Value1,Value2:TValue;Equal:boolean):boolean;
begin
if Value1.WinLossDraw=Value2.WinLossDraw then
 BetterOrEqual:=(value1.pieces>Value2.pieces)or((value1.Pieces=value2.Pieces) and (Equal or ((Value1.MoE) and (Value2.MoE=false))))
else
 if Value1.WinLossDraw then
  BetterOrEqual:=Value1.pieces>=0
 else
  BetterOrEqual:=value2.pieces<0;
end;

function StrValue1(Value:TValue):string;
begin
StrValue1:=inttostr(abs(Value.Pieces));
if Value.WinLossDraw then
 if Value.Pieces>0 then StrValue1:='Win by '+Result
 else
  if Value.Pieces<0 then StrValue1:='Loss by '+Result
 else StrValue1:='Draw'
else StrValue1:=inttostr(Value.Pieces);
end;

function StrValue2(Value:TValue):string;
begin
StrValue2:=inttostr(Value.Pieces);
if Value.WinLossDraw then
 StrValue2:=Result+'p';
if Value.LoE then
 StrValue2:='<='+Result
else if Value.MoE then StrValue2:='>='+Result;
end;

function Opposite(Value:TValue):TValue;
begin
Opposite:=Value;
Opposite.Pieces:=-Value.Pieces;
Opposite.MoE:=Value.LoE;
Opposite.LoE:=Value.MoE;
end;

procedure CompMove(var Move:TMove;Field:TField;Piece:TPlayerID);
var i,depth,maxdepth:TNumberOfSquares;
    max,res,MinValue,MaxValue:TValue;
    CompPosition,Pi:TPosition;
    Sorting:Boolean;
label done;

function Value(Position:TPosition;Alpha,Beta:TValue):TValue;
var i:TNumberOfSquares;
    Pi:TPosition;
    Max:TValue;
label done;
begin application.processmessages;inc(Depth);
if Depth=MaxDepth then
 begin Max:=StaticValue(Position.Field,Position.NextMove);goto done;end;
Max:=MinValue;
for i:=1 to Position.NumberOfMoves do
 begin GetPosition(Position.BranchPositions[i].Field,3-Position.NextMove,Pi);if Pi.NumberOfMoves>0 then
   Result:=Opposite(Value(Pi,Opposite(Beta),Opposite(Alpha)))     {* Alpha>=Max => -Alpha<=-Max => Beta<=-Max }
  else begin GetPosition(Position.BranchPositions[i].Field,Position.NextMove,Pi);
   if Pi.NumberOfMoves>0 then
    Result:=Value(Pi,Alpha,Beta)
   else
    Result:=StaticValue(Position.BranchPositions[i].Field,Position.NextMove);end;
  if BetterOrEqual(Max,Alpha,true)then Alpha:=Max;
  if BetterOrEqual(result,max,false) then
   begin
    max:=result;
    if(not Max.LoE)and(Max.MoE or BetterOrEqual(Max,Beta,true))then
     begin if Position.NumberOfMoves>1 then begin Max.LoE:=false;max.MoE:=true;end;goto done;end;
   end;
 end;
done:Result:=Max;if BetterOrEqual(Result,MaxValue,true) then Result.MoE:=false;
dec(depth);
end;

begin
{}form1.statusbar1.panels[0].Text:='Thinking...';
{}form1.ProgressBar1.Max:=form1.positions[piece].NumberOfMoves;form1.ProgressBar1.Position:=0;
{}Move.i:=1;Move.j:=1;{for depth:=1 to 15 do TransPos[depth].NumberOfPos:=0; }
{}for depth:=1 to 8 do for maxdepth:=1 to 8 do form1.values.V[depth,maxdepth]:='';
{}if 64-pieces(field,1)-pieces(field,2)<=strtoint(form1.labelededit2.text) then
{} maxdepth:=strtoint(form1.labelededit2.text) else maxdepth:=strtoint(form1.labelededit1.text);
{}form1.statusbar1.panels[3].text:='Depth='+inttostr(maxdepth);
{}res.WinLossDraw:=false;form1.bsetup.Enabled:=false;
{}form1.updown1.Enabled:=false;form1.UpDown2.Enabled:=false;
{}form1.DrawGrid1.Repaint;depth:=0;

GetPosition(field,piece,CompPosition);
if compPosition.NumberOfMoves=1 then begin Move:=compPosition.BranchPositions[1].Move;goto done;end;

MinValue.WinLossDraw:=true;MinValue.LoE:=false;MinValue.MoE:=false;MinValue.Pieces:=-64;
MaxValue.WinLossDraw:=true;MaxValue.LoE:=false;MaxValue.MoE:=false;MaxValue.Pieces:=64;
Max:=MinValue;
for i:=1 to compPosition.NumberOfMoves do
 begin GetPosition(CompPosition.BranchPositions[i].Field,3-Piece,Pi);
  if Pi.NumberOfMoves>0 then
   if i<CompPosition.NumberOfMoves then
    Res:=Opposite(Value(Pi,Opposite(MaxValue),Opposite(max)))
   else
    Res:=Opposite(Value(Pi,Opposite(Max),Opposite(Max)))
  else
   begin GetPosition(CompPosition.BranchPositions[i].Field,Piece,Pi);
    if Pi.NumberOfMoves>0 then
     if i<compPosition.NumberOfMoves then
      Res:=Value(Pi,Max,MaxValue)
     else
      Res:=Value(Pi,Max,Max)
    else
     Res:=StaticValue(CompPosition.BranchPositions[i].Field,Piece);
   end;
  if (not Res.LoE)and(Res.MoE or BetterOrEqual(Res,Max,true)) then
   begin Move:=compPosition.BranchPositions[i].Move;max:=res;
    {}form1.values.best:=Move;
    {}form1.statusbar1.panels[1].Text:=StrValue1(res);
   end;
  {}form1.Values.V[compPosition.BranchPositions[i].Move.i,compPosition.BranchPositions[i].Move.j]:=StrValue2(res);
  {}form1.DrawGrid1.Repaint;
  {}form1.ProgressBar1.Position:=form1.ProgressBar1.Position+1;
  if BetterOrEqual(max,MaxValue,true) then
   goto done;
 end;
done:{}form1.statusbar1.panels[0].Text:='Ready';form1.bsetup.Enabled:=true;
{}form1.updown1.Enabled:=true;form1.UpDown2.Enabled:=true;
end;

function legalmove(i,j:TSide;Position:TPosition;var number:byte):boolean;
var k:byte;
begin
result:=false;
for k:=1 to Position.NumberOfMoves do
 if (Position.BranchPositions[k].Move.i=i)and(Position.BranchPositions[k].Move.j=j) then
  begin result:=true;number:=k;break;end;
end;

procedure PlayerMove(var Move:TMove;Field:TField;Piece:TPlayerID);
var c:byte;
begin
repeat
application.ProcessMessages;
if form1.break then exit;
until legalmove(form1.drawgrid1.Col+1,form1.drawgrid1.Row+1,form1.Positions[piece],c);
Move.i:=form1.drawgrid1.col+1;
Move.j:=form1.drawgrid1.row+1;
end;

procedure TForm1.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var text:string;
begin
drawgrid1.Canvas.Brush.Color:=clwhite;
drawgrid1.Canvas.FillRect(rect);
case field[acol+1,arow+1] of
1:drawgrid1.Canvas.StretchDraw(rect,Player1.Picture.Bitmap);
2:drawgrid1.Canvas.StretchDraw(rect,Player2.Picture.Bitmap);
end;
if (Values.Last.i=acol+1)and(Values.Last.j=arow+1)and(field[acol+1,arow+1]>0)then
 begin
  DrawGrid1.Canvas.Pen.Color:=clred;
  DrawGrid1.Canvas.MoveTo(Rect.Left,Rect.Top);
  DrawGrid1.Canvas.LineTo(Rect.Right,Rect.Top);
  DrawGrid1.Canvas.LineTo(Rect.Right,Rect.Bottom);
  DrawGrid1.Canvas.LineTo(Rect.Left,Rect.Bottom);
  DrawGrid1.Canvas.LineTo(Rect.Left,Rect.Top);
 end;
if (acol+1=values.best.i)and(arow+1=values.best.j)then
 DrawGrid1.Canvas.Font.Color:=clred
else
 DrawGrid1.Canvas.Font.Color:=clblack;
text:=values.v[acol+1,arow+1];
if copy(text,1,2)='<=' then
 with drawgrid1.Canvas do
 begin
  delete(text,1,2);
  pen.Color:=clblack;
  draw(rect.Left,rect.Top,image1.Picture.Graphic);
  TextOut(Rect.Left+8,Rect.Top,text);
 end
else
 if copy(text,1,2)='>=' then
  with drawgrid1.Canvas do
   begin
    delete(text,1,2);
    pen.Color:=clblack;
    draw(rect.Left,rect.Top,image2.Picture.Graphic);
    TextOut(Rect.Left+8,Rect.Top,text);
   end
 else
  DrawGrid1.Canvas.TextOut(Rect.Left,Rect.Top,text);
end;

procedure game(Players:TPlayers);
var Move:TMove;
    c1,c2:byte;
    moveid:byte;
gameend:boolean;
begin form1.BSetup.Enabled:=true;
Form1.Values.Last.i:=1;Form1.Values.Last.j:=1;
Form1.Edit1.Text:=inttostr(pieces(form1.field,1));
Form1.Edit2.Text:=inttostr(pieces(form1.field,2));
GetPosition(Form1.Field,1,Form1.Positions[1]);
GetPosition(Form1.Field,2,Form1.Positions[2]);
for c1:=1 to 8 do
 for c2:=1 to 8 do
  form1.Values.V[c1,c2]:='';
form1.DrawGrid1.Repaint;
form1.StatusBar1.Panels[2].Text:=Form1.Player1.Hint+' '+inttostr(form1.Positions[1].NumberOfMoves)+' moves  '
 +Form1.Player2.Hint+' '+inttostr(Form1.Positions[2].NumberOfMoves)+' moves';
gameend:=false;
moveid:=1;
if form1.Positions[1].NumberOfMoves=0 then
 if form1.Positions[2].NumberOfMoves=0 then
 gameend:=true;
while not gameend do
begin if form1.break then exit;form1.drawgrid1.Selection:=TGridRect(form1.drawgrid1.cellrect(-1,-1));
GetPosition(Form1.Field,1,Form1.Positions[1]);
GetPosition(Form1.Field,2,Form1.Positions[2]);
if form1.Positions[moveid].NumberOfMoves=0 then
 if form1.Positions[3-moveid].NumberOfMoves=0 then
  gameend:=true
 else
  begin
   if Players[MoveID].Name='human' then
    showmessage('pass move');
   moveID:=3-MoveID;
  end;
if not gameend then
 Players[MoveID].GetMove(Move,form1.field,MoveID);
if legalmove(Move.i,Move.j,form1.Positions[moveid],c1) then
 begin
  form1.field:=form1.Positions[moveid].BranchPositions[c1].field;
  Form1.Values.Last:=Move;
  form1.Protocol.NumberOfMoves:=form1.Protocol.NumberOfMoves+1;
  form1.Protocol.Current:=form1.Protocol.Current+1;
  Form1.UpDown3.Position:=Form1.protocol.current;
  form1.Protocol.Fields[form1.Protocol.NumberOfMoves].Field:=Form1.Field;
  form1.Protocol.Fields[form1.Protocol.NumberOfMoves].PlayerID:=MoveID;
  form1.DrawGrid1.Repaint;
 end;
moveID:=3-MoveID;
GetPosition(Form1.Field,1,Form1.Positions[1]);
GetPosition(Form1.Field,2,Form1.Positions[2]);
form1.StatusBar1.Panels[2].Text:=Form1.Player1.Hint+' '+inttostr(form1.Positions[1].NumberOfMoves)+' moves  '
 +Form1.Player2.Hint+' '+inttostr(form1.Positions[2].NumberOfMoves)+' moves';
Form1.Edit1.Text:=inttostr(pieces(form1.field,1));
Form1.Edit2.Text:=inttostr(pieces(form1.field,2));
end;
c1:=pieces(form1.field,1);
c2:=pieces(form1.field,2);
if c1>c2 then
 showmessage('Winner is '+players[1].name)
else
 if c2>c1 then
  showmessage('Winner is '+players[2].name)
 else
  showmessage('Draw');
form1.BSetup.Enabled:=true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
PC.Checked:=true;
CC.Checked:=false;
CP.Checked:=false;
PP.Checked:=false;
if n2.Checked then begin Player1.Picture:=Blue.Picture;Player1.Hint:='Blue';end;
if n3.Checked then begin Player1.Picture:=Green.Picture;Player1.Hint:='Green';end;
if n4.Checked then begin Player1.Picture:=Red.Picture;Player1.Hint:='Red';end;
if n5.Checked then begin Player1.Picture:=Yellow.Picture;Player1.Hint:='Yellow';end;
if n6.Checked then begin Player2.Picture:=Blue.Picture;Player2.Hint:='Blue';end;
if n7.Checked then begin Player2.Picture:=Green.Picture;Player2.Hint:='Green';end;
if n8.Checked then begin Player2.Picture:=Red.Picture;Player2.Hint:='Red';end;
if n9.Checked then begin Player2.Picture:=Yellow.Picture;Player2.Hint:='Yellow';end;
break:=false;setup:=false;
label3.Top:=0;
label4.Left:=5;label5.Left:=5;label6.Left:=5;label7.Left:=5;label8.Left:=5;label9.Left:=5;label10.Left:=5;label11.Left:=5;
drawgrid1.Height:=drawgrid1.GridLineWidth+drawgrid1.rowcount*(drawgrid1.DefaultRowHeight+drawgrid1.GridLineWidth)+2;
drawgrid1.Width:=drawgrid1.GridLineWidth+drawgrid1.colcount*(drawgrid1.DefaultColWidth+drawgrid1.GridLineWidth)+2;
drawgrid1.top:=label3.Height+label3.Top;
drawgrid1.Left:=label4.Left+label4.Width+5;
Label3.Left:=Drawgrid1.Left;
label4.Top:=Drawgrid1.Top+1+15-6;label5.Top:=label4.Top+31;label6.Top:=label5.Top+31;label7.Top:=label6.Top+31;label8.Top:=label7.Top+31;
label9.Top:=label8.Top+31;label10.Top:=label9.Top+31;label11.Top:=label10.Top+31;
Player1.Left:=DrawGrid1.Left+DrawGrid1.Width+60;Player1.Top:=10;
Player1.Width:=Player1.Picture.Width;Player1.Height:=Player1.Picture.Height;
Player2.Left:=Player1.Left;Player2.Top:=Player1.Top+Player1.Height+20;
Player2.Width:=Player1.Picture.Width;Player2.Height:=Player1.Picture.Height;
Edit1.Width:=20;  Edit2.Width:=Edit1.Width;
Edit1.Height:=15;  Edit2.Height:=Edit1.Height;
Edit1.Left:=Player1.Left-Edit1.Width-35;  Edit1.Top:=Player1.Top+Player1.Height-Edit1.Height;
Edit2.Left:=Edit1.Left;  Edit2.Top:=Player2.Top+Player2.Height-Edit2.Height;
Label1.Left:=Edit1.Left-3;Label1.Top:=Player1.top;
Label2.Left:=Edit2.Left-3;Label2.Top:=Player2.Top;
form1.Clientheight:=Drawgrid1.Height+statusbar1.Height+label3.Height+UpDown3.Height;
form1.ClientWidth:=drawgrid1.Width+label4.Width+110+5+5;
progressbar1.Left:=drawgrid1.Left+drawgrid1.Width+5;
progressbar1.Width:=form1.ClientWidth-drawgrid1.Width-label4.Width-20;
labelededit1.Left:=ProgressBar1.Left;
labelededit2.Left:=labelededit1.left;
Updown3.Top:=drawgrid1.Top+drawgrid1.Height;
form1.Show;
//newclick(sender);
end;

procedure TForm1.NewClick(Sender: TObject);
begin
clear(field);
setfield(field);
clear(form1.protocol.fields[1].field);
setfield(form1.protocol.fields[1].field);
Protocol.NumberOfMoves:=1;
Protocol.Current:=1;UpDown3.Position:=protocol.current;
drawgrid1.DefaultDrawing:=false;
form1.statusbar1.panels[0].Text:='';
form1.statusbar1.panels[1].Text:='';
if CC.Checked or cp.Checked then
 begin Players[1].GetMove:=CompMove;Players[1].Name:='CPU';end
else
 begin Players[1].GetMove:=PlayerMove;Players[1].Name:='human';end;
if pp.Checked or cp.Checked then
 begin Players[2].GetMove:=PlayerMove;Players[2].Name:='human';end
else
 begin Players[2].GetMove:=CompMove;Players[2].Name:='CPU';end;
label1.caption:=Players[1].Name;label2.caption:=Players[2].Name;
game(players);
drawgrid1.Repaint;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
halt;
end;

procedure TForm1.MoveClick(Sender: TObject);
begin
PC.Checked:=false;
CC.Checked:=false;
CP.Checked:=false;
PP.Checked:=false;
(sender as TMenuItem).Checked:=true;
if CC.Checked or cp.Checked then
 begin Players[1].GetMove:=CompMove;Players[1].Name:='CPU';end
else
 begin Players[1].GetMove:=PlayerMove;Players[1].Name:='human';end;
if pp.Checked or cp.Checked then
 begin Players[2].GetMove:=PlayerMove;Players[2].Name:='human';end
else
 begin Players[2].GetMove:=CompMove;Players[2].Name:='CPU';end;
label1.caption:=Players[1].Name;label2.caption:=Players[2].Name;
end;

procedure TForm1.Color1Click(Sender: TObject);
begin
N2.Checked:=false;
N3.Checked:=false;
N4.Checked:=false;
N5.Checked:=false;
(Sender as TMenuItem).Checked:=true;
if n2.Checked then begin Player1.Picture:=Blue.Picture;Player1.Hint:='Blue';end;
if n3.Checked then begin Player1.Picture:=Green.Picture;Player1.Hint:='Green';end;
if n4.Checked then begin Player1.Picture:=Red.Picture;Player1.Hint:='Red';end;
if n5.Checked then begin Player1.Picture:=Yellow.Picture;Player1.Hint:='Yellow';end;
form1.StatusBar1.Panels[2].Text:=Form1.Player1.Hint+' '+inttostr(form1.Positions[1].NumberOfMoves)+' moves  '
 +Form1.Player2.Hint+' '+inttostr(Form1.Positions[2].NumberOfMoves)+' moves';
drawgrid1.Repaint;
end;

procedure TForm1.Color2Click(Sender: TObject);
begin
N6.Checked:=false;
N7.Checked:=false;
N8.Checked:=false;
N9.Checked:=false;
(Sender as TMenuItem).Checked:=true;
if n6.Checked then begin Player2.Picture:=Blue.Picture;Player2.Hint:='Blue';end;
if n7.Checked then begin Player2.Picture:=Green.Picture;Player2.Hint:='Green';end;
if n8.Checked then begin Player2.Picture:=Red.Picture;Player2.Hint:='Red';end;
if n9.Checked then begin Player2.Picture:=Yellow.Picture;Player2.Hint:='Yellow';end;
form1.StatusBar1.Panels[2].Text:=Form1.Player1.Hint+' '+inttostr(form1.Positions[1].NumberOfMoves)+' moves  '
 +Form1.Player2.Hint+' '+inttostr(Form1.Positions[2].NumberOfMoves)+' moves';
drawgrid1.Repaint;
end;

procedure TForm1.BSetupClick(Sender: TObject);
begin
bsetup.Enabled:=false;
continue.Enabled:=true;
break:=true;setup:=true;
end;

procedure TForm1.ContinueClick(Sender: TObject);
begin
bsetup.Enabled:=true;
continue.Enabled:=false;
break:=false;setup:=false;
drawgrid1.Selection:=TGridRect(drawgrid1.cellrect(-1,-1));
protocol.fields[1].field:=field;
Protocol.NumberOfMoves:=1;
Protocol.Current:=1;UpDown3.Position:=protocol.current;
game(players);
end;

procedure TForm1.DrawGrid1Click(Sender: TObject);
begin
if not setup then
 exit;
repeat field[drawgrid1.Col+1,drawgrid1.Row+1]:=(field[drawgrid1.Col+1,drawgrid1.Row+1]+1)mod 3;
until not((field[drawgrid1.col+1,drawgrid1.row+1]=0)and(drawgrid1.col+1 in [4,5])and(drawgrid1.row+1 in [4,5]));
drawgrid1.Repaint;
end;

procedure TForm1.UpDown3Click(Sender: TObject; Button: TUDBtnType);
begin
if button=btnext then
 if protocol.current+1<=protocol.NumberOfMoves then
  inc(protocol.current);
if button=btprev then
 if protocol.current-1>=1 then
  dec(protocol.current);
UpDown3.Position:=protocol.current;
field:=protocol.fields[protocol.current].Field;
drawgrid1.Repaint;
end;

end.

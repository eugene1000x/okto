
unit Reversi;


interface


uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Menus, Grids, StdCtrls, ExtCtrls, ImgList, ComCtrls, ShellCtrls;

const
	Side = 8;

type
	TPlayerId = 1..2;
	TState = 0..2;
	TNextMove = 0..2;
	TSide = 1..Side;
	TField = array [TSide, TSide] of TState;
	
	TMove = record
		I, J: TSide;
	end;
	
	TPlayer = record
		Name: string;
		GetMove: procedure(var Move: TMove; Field: TField; Piece: TPlayerId);
	end;
	
	TPlayers = array [1..2] of TPlayer;
	TNumberOfSquares = 0 .. Side * Side;
	
	TBranchPosition = record
		Move: TMove;
		Field: TField;
	end;
	
	TBranchPositions = array [TNumberOfSquares] of TBranchPosition;
	
	TPosition = record
		Field: TField;
		NextMove: TNextMove;
		NumberOfMoves: TNumberOfSquares;
		BranchPositions: TBranchPositions;
	end;
	
	TValue = record
		WinLossDraw, LOE, MOE: Boolean;
		Pieces: Integer;
	end;
	
	TTransPos = array [1..2, 1..15] of record
		NumberOfPos: Word;		//MoveId, Depth
		Positions: array of record
			Field: TField;
			Value: TValue;
		end;
	end;
	
	TProtocol = record
		Fields: array [TNumberOfSquares] of record
			Field: TField;
			PlayerId: TPlayerId;
		end;
		NumberOfMoves, Current: TNumberOfSquares;
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
		Statusbar1: TStatusBar;
		Label3: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		Label6: TLabel;
		Label7: TLabel;
		Label8: TLabel;
		Label9: TLabel;
		Label10: TLabel;
		Label11: TLabel;
		Progressbar1: TProgressBar;
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
		procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
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
		// nothing
	public
		Field: TField;
		Positions: array [1..2] of TPosition;
		Players: TPlayers;
		
		Values: record
			Last, Best: TMove;
			V: array [1..8, 1..8] of string;
		end;
		
		Protocol: TProtocol;
		Setup, Break: Boolean;
	end;

var
	Form1: TForm1;


implementation
{$R *.dfm}


procedure Clear(var Field: TField);
var
	I, J: Byte;
begin
	for I := 1 to 8 do
		for J := 1 to 8 do
			Field[I, J] := 0;
end;

procedure SetField(var Field: TField);
begin
	Field[4, 4] := 2;
	Field[5, 4] := 1;
	Field[4, 5] := 1;
	Field[5, 5] := 2;
end;

procedure GetPosition(Field: TField; NextMove: TNextMove; var Position: TPosition);
var
	I, J: TSide;
	RevCount: TNumberOfSquares;
	K, L: Byte;
	Vx, Vy: -1..1;
begin
	Position.NumberOfMoves := 0;
	Position.Field := Field;
	Position.NextMove := 0;
	
	for I := 1 to 8 do
		for J := 1 to 8 do
		begin
			RevCount := 0;
			Position.BranchPositions[Position.NumberOfMoves + 1].Field := Field;
			
			for Vx := -1 to 1 do
				for Vy := -1 to 1 do
					if (Field[I, J] = 0) and (not ((Vx = 0) and (Vy = 0))) then
						if (I + Vx in [1..8]) and (J + Vy in [1..8]) and (Field[I + Vx, J + Vy] = 3 - NextMove) then
						begin
							K := 2;
							
							while (I + K * Vx in [1..8]) and (J + K * Vy in [1..8]) and (Field[I + K * Vx, J + K * Vy] = 3 - NextMove) do
								Inc(K);
								
							if (I + K * Vx in [1..8]) and (J + K * Vy in [1..8]) and (Field[I + K * Vx, J + K * Vy] = NextMove) then
							begin
								for L := 0 to K - 1 do
									Position.BranchPositions[Position.NumberOfMoves + 1].Field[I + L * Vx, J + L * Vy] := NextMove;
									
								Inc(RevCount, K - 1);
							end;
						end;
			if RevCount > 0 then
			begin
				Inc(Position.NumberOfMoves);
				Position.BranchPositions[Position.NumberOfMoves].Move.I := I;
				Position.BranchPositions[Position.NumberOfMoves].Move.J := J;
				Position.NextMove := NextMove;
			end;
		end;
end;

function Pieces(Field: TField; Piece: Byte): Byte;
var
	I, J: TSide;
begin
	Result := 0;
	for I := 1 to Side do
		for J := 1 to Side do
			if Field[I, J] = Piece then
				Inc(Result);
end;

function Midgame(Field: TField): Boolean;
begin
	Result := Pieces(Field, 1) + Pieces(Field, 2) < 56;
end;

function StaticValue(Field: TField; Piece: TPlayerId): TValue;
const
	G: array [1..4] of array [1..2] of 1..8 = ((1, 1), (1, 8), (8, 1), (8, 8));
const
	K: array [1..4] of array [1..2] of 1..8 = ((2, 2), (2, 7), (7, 2), (7, 7));
var
	I: Byte;
	Positions: array [1..2] of TPosition;
begin
	Result.Pieces := 0;
	Result.WinLossDraw := False;
	Result.LOE := False;
	Result.MOE := False;
	
	GetPosition(Field, Piece, Positions[Piece]);
	GetPosition(Field, 3 - Piece, Positions[3 - Piece]);
	
	if (Positions[Piece].NumberOfMoves = 0) and (Positions[3 - Piece].NumberOfMoves = 0) then
	begin
		Result.WinLossDraw := True;
		Result.Pieces := Pieces(Field, Piece) - Pieces(Field, 3 - Piece);
		Exit;
	end;
	
	if Midgame(Field) then
		Result.Pieces := Pieces(Field, 3 - Piece) - Pieces(Field, Piece)
	else
		Result.Pieces := Pieces(Field, Piece) - Pieces(Field, 3 - Piece);
	
	Result.Pieces := Result.Pieces + Positions[Piece].NumberOfMoves;
	Result.Pieces := Result.Pieces - Positions[3 - Piece].NumberOfMoves;
	
	for I := 1 to 4 do
		if (Field[G[I][1], G[I][2]] = Piece) or ((Field[K[I][1], K[I][2]] = 3 - Piece) and (Field[G[I][1], G[I][2]] = 0)) then
			Result.Pieces := Result.Pieces + 100
		else
			if (Field[G[I][1], G[I][2]] = 3 - Piece) or ((Field[K[I][1], K[I][2]] = Piece) and (Field[G[I][1], G[I][2]] = 0)) then
				Result.Pieces := Result.Pieces - 100;
end;

function BetterOrEqual(Value1, Value2: TValue; Equal: Boolean): Boolean;
begin
	if Value1.WinLossDraw = Value2.WinLossDraw then
		BetterOrEqual := (Value1.Pieces > Value2.Pieces) or ((Value1.Pieces = Value2.Pieces) and (Equal or ((Value1.MOE) and (Value2.MOE = False))))
	else
		if Value1.WinLossDraw then
			BetterOrEqual := Value1.Pieces >= 0
		else
			BetterOrEqual := Value2.Pieces < 0;
end;

function StrValue1(Value: TValue): string;
begin
	StrValue1 := IntToStr(Abs(Value.Pieces));
	
	if Value.WinLossDraw then
		if Value.Pieces > 0 then
			StrValue1 := 'Win by '+ Result
		else if Value.Pieces < 0 then
			StrValue1 := 'Loss by '+ Result
		else
			StrValue1 := 'Draw'
	else
		StrValue1 := IntToStr(Value.Pieces);
end;

function StrValue2(Value: TValue): string;
begin
	StrValue2 := IntToStr(Value.Pieces);
	
	if Value.WinLossDraw then
		StrValue2 := Result +'p';
		
	if Value.LOE then
		StrValue2 := '<='+ Result
	else if Value.MOE then
		StrValue2 := '>='+ Result;
end;

function Opposite(Value: TValue): TValue;
begin
	Opposite := Value;
	Opposite.Pieces := -Value.Pieces;
	Opposite.MOE := Value.LOE;
	Opposite.LOE := Value.MOE;
end;

procedure CompMove(var Move: TMove; Field: TField; Piece: TPlayerID);
var
	I, Depth, MaxDepth: TNumberOfSquares;
	Max, Res, MinValue, MaxValue: TValue;
	CompPosition, Pi: TPosition;
	Sorting: Boolean;
label
	done;

	function Value(Position: TPosition; Alpha, Beta: TValue): TValue;
	var
		I: TNumberOfSquares;
		Pi: TPosition;
		Max: TValue;
	label
		done;
	begin
		Application.ProcessMessages;
		Inc(Depth);
		
		if Depth = MaxDepth then
		begin
			Max := StaticValue(Position.Field, Position.NextMove);
			goto done;
		end;
		
		Max := MinValue;
		
		for I := 1 to Position.NumberOfMoves do
		begin
			GetPosition(Position.BranchPositions[I].Field, 3 - Position.NextMove, Pi);
		
			if Pi.NumberOfMoves > 0 then
				Result := Opposite(Value(Pi, Opposite(Beta), Opposite(Alpha)))		// (Alpha >= Max) => (-Alpha <= -Max) => (Beta <= -Max)
			else
			begin
				GetPosition(Position.BranchPositions[I].Field, Position.NextMove, Pi);
				
				if Pi.NumberOfMoves > 0 then
					Result := Value(Pi, Alpha, Beta)
				else
					Result := StaticValue(Position.BranchPositions[I].Field, Position.NextMove);
			end;
			
			if BetterOrEqual(Max, Alpha, True) then
				Alpha := Max;
			
			if BetterOrEqual(Result, Max, False) then
			begin
				Max := Result;
				
				if (not Max.LOE) and (Max.MOE or BetterOrEqual(Max, Beta, True)) then
				begin
					if Position.NumberOfMoves > 1 then
					begin
						Max.LOE := False;
						Max.MOE := True;
					end;
					
					goto done;
				end;
			end;
		end;
		
		done:
		
		Result := Max;
		
		if BetterOrEqual(Result, MaxValue, True) then
			Result.MOE := False;
			
		Dec(Depth);
	end;

begin
	Form1.Statusbar1.Panels[0].Text := 'Thinking...';
	
	Form1.Progressbar1.Max := Form1.Positions[Piece].NumberOfMoves;
	Form1.Progressbar1.Position := 0;
	
	Move.I := 1;
	Move.J := 1;
	
	//for Depth := 1 to 15 do
	//	TransPos[Depth].NumberOfPos := 0;
	
	for Depth := 1 to 8 do
		for MaxDepth := 1 to 8 do
			Form1.Values.V[Depth, MaxDepth] := '';
	
	if 64 - Pieces(Field, 1) - Pieces(Field, 2) <= StrToInt(Form1.LabeledEdit2.Text) then
		MaxDepth := StrToInt(Form1.LabeledEdit2.Text)
	else
		MaxDepth := StrToInt(Form1.LabeledEdit1.Text);
	
	Form1.Statusbar1.Panels[3].Text := 'Depth='+ IntToStr(MaxDepth);
	
	Res.WinLossDraw := False;
	Form1.BSetup.Enabled := False;
	
	Form1.UpDown1.Enabled := False;
	Form1.UpDown2.Enabled := False;
	
	Form1.DrawGrid1.Repaint;
	Depth := 0;
	
	GetPosition(Field, Piece, CompPosition);
	
	if CompPosition.NumberOfMoves = 1 then
	begin
		Move := CompPosition.BranchPositions[1].Move;
		goto done;
	end;
	
	MinValue.WinLossDraw := True;
	MinValue.LOE := False;
	MinValue.MOE := False;
	MinValue.Pieces := -64;
	
	MaxValue.WinLossDraw := True;
	MaxValue.LOE := False;
	MaxValue.MOE := False;
	MaxValue.Pieces := 64;
	
	Max := MinValue;
	
	for I := 1 to CompPosition.NumberOfMoves do
	begin
		GetPosition(CompPosition.BranchPositions[I].Field, 3 - Piece, Pi);
		
		if Pi.NumberOfMoves > 0 then
			if I < CompPosition.NumberOfMoves then
				Res := Opposite(Value(Pi, Opposite(MaxValue), Opposite(Max)))
			else
				Res := Opposite(Value(Pi, Opposite(Max), Opposite(Max)))
		else
		begin
			GetPosition(CompPosition.BranchPositions[I].Field, Piece, Pi);
			
			if Pi.NumberOfMoves > 0 then
				if I < CompPosition.NumberOfMoves then
					Res := Value(Pi, Max, MaxValue)
				else
					Res := Value(Pi, Max, Max)
			else
				Res := StaticValue(CompPosition.BranchPositions[I].Field, Piece);
		end;
		
		if (not Res.LOE) and (Res.MOE or BetterOrEqual(Res, Max, True)) then
		begin
			Move := CompPosition.BranchPositions[I].Move;
			Max := Res;
			Form1.Values.Best := Move;
			Form1.Statusbar1.Panels[1].Text := StrValue1(Res);
		end;
		
		Form1.Values.V[CompPosition.BranchPositions[I].Move.I, CompPosition.BranchPositions[I].Move.J] := StrValue2(Res);
		Form1.DrawGrid1.Repaint;
		Form1.Progressbar1.Position := Form1.Progressbar1.Position + 1;
		
		if BetterOrEqual(Max, MaxValue, True) then
			goto done;
	end;
	
	done:
	
	Form1.Statusbar1.Panels[0].Text := 'Ready';
	Form1.BSetup.Enabled := True;
	
	Form1.UpDown1.Enabled := True;
	Form1.UpDown2.Enabled := True;
end;

function LegalMove(I, J: TSide; Position: TPosition; var Number: Byte): Boolean;
var
	K: Byte;
begin
	Result := False;
	
	for K := 1 to Position.NumberOfMoves do
		if (Position.BranchPositions[K].Move.I = I) and (Position.BranchPositions[K].Move.J = J) then
		begin
			Result := True;
			Number := K;
			Break;
		end;
end;

procedure PlayerMove(var Move: TMove; Field: TField; Piece: TPlayerID);
var
	C: Byte;
begin
	repeat
		Application.ProcessMessages;
		
		if Form1.Break then
			Exit;
	until LegalMove(Form1.DrawGrid1.Col + 1, Form1.DrawGrid1.Row + 1, Form1.Positions[Piece], C);
	
	Move.I := Form1.DrawGrid1.Col + 1;
	Move.J := Form1.DrawGrid1.Row + 1;
end;

procedure TForm1.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
	Text: string;
begin
	DrawGrid1.Canvas.Brush.Color := clWhite;
	DrawGrid1.Canvas.FillRect(Rect);
	
	case Field[ACol + 1, ARow + 1] of
		1:
			DrawGrid1.Canvas.StretchDraw(Rect, Player1.Picture.Bitmap);
		2:
			DrawGrid1.Canvas.StretchDraw(Rect, Player2.Picture.Bitmap);
	end;
	
	if (Values.Last.I = ACol + 1) and (Values.Last.J = ARow + 1) and (Field[ACol + 1, ARow + 1] > 0) then
	begin
		DrawGrid1.Canvas.Pen.Color := clRed;
		DrawGrid1.Canvas.MoveTo(Rect.Left, Rect.Top);
		DrawGrid1.Canvas.LineTo(Rect.Right, Rect.Top);
		DrawGrid1.Canvas.LineTo(Rect.Right, Rect.Bottom);
		DrawGrid1.Canvas.LineTo(Rect.Left, Rect.Bottom);
		DrawGrid1.Canvas.LineTo(Rect.Left, Rect.Top);
	end;
	
	if (ACol + 1 = Values.Best.I) and (ARow + 1 = Values.Best.J) then
		DrawGrid1.Canvas.Font.Color := clRed
	else
		DrawGrid1.Canvas.Font.Color := clBlack;
		
	Text := Values.V[ACol + 1, ARow + 1];
	
	if Copy(Text, 1, 2) = '<=' then
		with DrawGrid1.Canvas do
		begin
			Delete(Text, 1, 2);
			Pen.Color := clBlack;
			Draw(Rect.Left, Rect.Top, Image1.Picture.Graphic);
			TextOut(Rect.Left + 8, Rect.Top, Text);
		end
	else
		if Copy(Text, 1, 2) = '>=' then
			with DrawGrid1.Canvas do
			begin
				Delete(Text, 1, 2);
				Pen.Color := clBlack;
				Draw(Rect.Left, Rect.Top, Image2.Picture.Graphic);
				TextOut(Rect.Left + 8, Rect.Top, Text);
			end
		else
			DrawGrid1.Canvas.TextOut(Rect.Left, Rect.Top, Text);
end;

procedure Game(Players: TPlayers);
var
	Move: TMove;
	C1, C2: Byte;
	MoveId: Byte;
	GameEnd: Boolean;
begin
	Form1.BSetup.Enabled := True;
	
	Form1.Values.Last.I := 1;
	Form1.Values.Last.J := 1;
	
	Form1.Edit1.Text := IntToStr(Pieces(Form1.Field, 1));
	Form1.Edit2.Text := IntToStr(Pieces(Form1.Field, 2));
	
	GetPosition(Form1.Field, 1, Form1.Positions[1]);
	GetPosition(Form1.Field, 2, Form1.Positions[2]);
	
	for C1 := 1 to 8 do
		for C2 := 1 to 8 do
			Form1.Values.V[C1, C2] := '';
			
	Form1.DrawGrid1.Repaint;
	
	Form1.Statusbar1.Panels[2].Text := Form1.Player1.Hint +' '+ IntToStr(Form1.Positions[1].NumberOfMoves) +' moves  '+
			Form1.Player2.Hint +' '+ IntToStr(Form1.Positions[2].NumberOfMoves) +' moves';
		
	GameEnd := False;
	MoveId := 1;
	
	if Form1.Positions[1].NumberOfMoves = 0 then
		if Form1.Positions[2].NumberOfMoves = 0 then
			GameEnd := True;
		
	while not GameEnd do
	begin
		if Form1.Break then
			Exit;
		
		Form1.DrawGrid1.Selection := TGridRect(Form1.DrawGrid1.CellRect(-1, -1));
		
		GetPosition(Form1.Field, 1, Form1.Positions[1]);
		GetPosition(Form1.Field, 2, Form1.Positions[2]);
		
		if Form1.Positions[MoveId].NumberOfMoves = 0 then
			if Form1.Positions[3 - MoveId].NumberOfMoves = 0 then
				GameEnd := True
			else
			begin
				if Players[MoveId].Name = 'human' then
					ShowMessage('pass move');
				MoveId := 3 - MoveId;
			end;
			
		if not GameEnd then
			Players[MoveId].GetMove(Move, Form1.Field, MoveId);
			
		if LegalMove(Move.I, Move.J, Form1.Positions[MoveId], C1) then
		begin
			Form1.Field := Form1.Positions[MoveId].BranchPositions[C1].Field;
			Form1.Values.Last := Move;
			Form1.Protocol.NumberOfMoves := Form1.Protocol.NumberOfMoves + 1;
			Form1.Protocol.Current := Form1.Protocol.Current + 1;
			Form1.UpDown3.Position := Form1.Protocol.Current;
			Form1.Protocol.Fields[Form1.Protocol.NumberOfMoves].Field := Form1.Field;
			Form1.Protocol.Fields[Form1.Protocol.NumberOfMoves].PlayerID := MoveId;
			Form1.DrawGrid1.Repaint;
		end;
		
		MoveId := 3 - MoveId;
		
		GetPosition(Form1.Field, 1, Form1.Positions[1]);
		GetPosition(Form1.Field, 2, Form1.Positions[2]);
		
		Form1.Statusbar1.Panels[2].Text := Form1.Player1.Hint +' '+ IntToStr(Form1.Positions[1].NumberOfMoves) +' moves  '+
				Form1.Player2.Hint +' '+ IntToStr(Form1.Positions[2].NumberOfMoves) +' moves';
		
		Form1.Edit1.Text := IntToStr(Pieces(Form1.Field, 1));
		Form1.Edit2.Text := IntToStr(Pieces(Form1.Field, 2));
	end;
	
	C1 := Pieces(Form1.Field, 1);
	C2 := Pieces(Form1.Field, 2);
	
	if C1 > C2 then
		ShowMessage('Winner is '+ Players[1].name)
	else if C2 > C1 then
		ShowMessage('Winner is '+ Players[2].name)
	else
		ShowMessage('Draw');
			
	Form1.BSetup.Enabled := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
	PC.Checked := True;
	CC.Checked := False;
	CP.Checked := False;
	PP.Checked := False;
	
	if N2.Checked then
	begin
		Player1.Picture := Blue.Picture;
		Player1.Hint := 'Blue';
	end;
	
	if N3.Checked then
	begin
		Player1.Picture := Green.Picture;
		Player1.Hint := 'Green';
	end;
	
	if N4.Checked then
	begin
		Player1.Picture := Red.Picture;
		Player1.Hint := 'Red';
	end;
	
	if N5.Checked then
	begin
		Player1.Picture := Yellow.Picture;
		Player1.Hint := 'Yellow';
	end;
	
	if N6.Checked then
	begin
		Player2.Picture := Blue.Picture;
		Player2.Hint := 'Blue';
	end;
	
	if N7.Checked then
	begin
		Player2.Picture := Green.Picture;
		Player2.Hint := 'Green';
	end;
	
	if N8.Checked then
	begin
		Player2.Picture := Red.Picture;
		Player2.Hint := 'Red';
	end;
	
	if N9.Checked then
	begin
		Player2.Picture := Yellow.Picture;
		Player2.Hint := 'Yellow';
	end;
	
	Break := False;
	Setup := False;
	
	Label3.Top := 0;
	Label4.Left := 5;
	Label5.Left := 5;
	Label6.Left := 5;
	Label7.Left := 5;
	Label8.Left := 5;
	Label9.Left := 5;
	Label10.Left := 5;
	Label11.Left := 5;
	
	DrawGrid1.Height := DrawGrid1.GridLineWidth + DrawGrid1.RowCount * (DrawGrid1.DefaultRowHeight + DrawGrid1.GridLineWidth) + 2;
	DrawGrid1.Width := DrawGrid1.GridLineWidth + DrawGrid1.ColCount * (DrawGrid1.DefaultColWidth + DrawGrid1.GridLineWidth) + 2;
	DrawGrid1.Top := Label3.Height + Label3.Top;
	DrawGrid1.Left := Label4.Left + Label4.Width + 5;
	
	Label3.Left := DrawGrid1.Left;
	Label4.Top := DrawGrid1.Top + 1 + 15 - 6;
	Label5.Top := Label4.Top + 31;
	Label6.Top := Label5.Top + 31;
	Label7.Top := Label6.Top + 31;
	Label8.Top := Label7.Top + 31;
	Label9.Top := Label8.Top + 31;
	Label10.Top := Label9.Top + 31;
	Label11.Top := Label10.Top + 31;
	
	Player1.Left := DrawGrid1.Left + DrawGrid1.Width + 60;
	Player1.Top := 10;
	Player1.Width := Player1.Picture.Width;
	Player1.Height := Player1.Picture.Height;
	Player2.Left := Player1.Left;
	Player2.Top := Player1.Top + Player1.Height + 20;
	Player2.Width := Player1.Picture.Width;
	Player2.Height := Player1.Picture.Height;
	
	Edit1.Width := 20;
	Edit2.Width := Edit1.Width;
	Edit1.Height := 15;
	Edit2.Height := Edit1.Height;
	Edit1.Left := Player1.Left - Edit1.Width - 35;
	Edit1.Top := Player1.Top + Player1.Height - Edit1.Height;
	Edit2.Left := Edit1.Left;
	Edit2.Top := Player2.Top + Player2.Height - Edit2.Height;
	
	Label1.Left := Edit1.Left - 3;
	Label1.Top := Player1.Top;
	Label2.Left := Edit2.Left - 3;
	Label2.Top := Player2.Top;
	
	Form1.ClientHeight := DrawGrid1.Height + Statusbar1.Height + Label3.Height + UpDown3.Height;
	Form1.ClientWidth := DrawGrid1.Width + Label4.Width + 110 + 5 + 5;
	
	Progressbar1.Left := DrawGrid1.Left + DrawGrid1.Width + 5;
	Progressbar1.Width := Form1.ClientWidth - DrawGrid1.Width - Label4.Width - 20;
	
	LabeledEdit1.Left := Progressbar1.Left;
	LabeledEdit2.Left := LabeledEdit1.left;
	
	UpDown3.Top := DrawGrid1.Top + DrawGrid1.Height;
	
	Form1.Show;
	//NewClick(Sender);
end;

procedure TForm1.NewClick(Sender: TObject);
begin
	Clear(Field);
	SetField(Field);
	Clear(Form1.Protocol.Fields[1].Field);
	SetField(Form1.Protocol.Fields[1].Field);
	
	Protocol.NumberOfMoves := 1;
	Protocol.Current := 1;
	
	UpDown3.Position := Protocol.Current;
	
	DrawGrid1.DefaultDrawing := False;
	
	Form1.Statusbar1.Panels[0].Text := '';
	Form1.Statusbar1.Panels[1].Text := '';
	
	if CC.Checked or CP.Checked then
	begin
		Players[1].GetMove := CompMove;
		Players[1].Name := 'CPU';
	end
	else
	begin
		Players[1].GetMove := PlayerMove;
		Players[1].Name := 'human';
	end;
	
	if PP.Checked or CP.Checked then
	begin
		Players[2].GetMove := PlayerMove;
		Players[2].Name := 'human';
	end
	else
	begin
		Players[2].GetMove := CompMove;
		Players[2].Name := 'CPU';
	end;
	
	Label1.Caption := Players[1].Name;
	Label2.Caption := Players[2].Name;
	Game(Players);
	DrawGrid1.Repaint;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Halt;
end;

procedure TForm1.MoveClick(Sender: TObject);
begin
	PC.Checked := False;
	CC.Checked := False;
	CP.Checked := False;
	PP.Checked := False;
	
	(Sender as TMenuItem).Checked := True;
	
	if CC.Checked or CP.Checked then
	begin
		Players[1].GetMove := CompMove;
		Players[1].Name := 'CPU';
	end
	else
	begin
		Players[1].GetMove := PlayerMove;
		Players[1].Name := 'human';
	end;
	
	if PP.Checked or CP.Checked then
	begin
		Players[2].GetMove := PlayerMove;
		Players[2].Name := 'human';
	end
	else
	begin
		Players[2].GetMove := CompMove;
		Players[2].Name := 'CPU';
	end;
	
	Label1.Caption := Players[1].Name;
	Label2.Caption := Players[2].Name;
end;
	
procedure TForm1.Color1Click(Sender: TObject);
begin
	N2.Checked := False;
	N3.Checked := False;
	N4.Checked := False;
	N5.Checked := False;
	
	(Sender as TMenuItem).Checked := True;
	
	if N2.Checked then
	begin
		Player1.Picture := Blue.Picture;
		Player1.Hint := 'Blue';
	end;
	
	if N3.Checked then
	begin
		Player1.Picture := Green.Picture;
		Player1.Hint := 'Green';
	end;
	
	if N4.Checked then
	begin
		Player1.Picture := Red.Picture;
		Player1.Hint := 'Red';
	end;
	
	if N5.Checked then
	begin
		Player1.Picture := Yellow.Picture;
		Player1.Hint := 'Yellow';
	end;
	
	Form1.Statusbar1.Panels[2].Text := Form1.Player1.Hint +' '+ IntToStr(Form1.Positions[1].NumberOfMoves) +' moves  '+
			Form1.Player2.Hint +' '+ IntToStr(Form1.Positions[2].NumberOfMoves) +' moves';
	
	DrawGrid1.Repaint;
end;

procedure TForm1.Color2Click(Sender: TObject);
begin
	N6.Checked := False;
	N7.Checked := False;
	N8.Checked := False;
	N9.Checked := False;
	
	(Sender as TMenuItem).Checked := True;
	
	if N6.Checked then
	begin
		Player2.Picture := Blue.Picture;
		Player2.Hint := 'Blue';
	end;
	
	if N7.Checked then
	begin
		Player2.Picture := Green.Picture;
		Player2.Hint := 'Green';
	end;
	
	if N8.Checked then
	begin
		Player2.Picture := Red.Picture;
		Player2.Hint := 'Red';
	end;
	
	if N9.Checked then
	begin
		Player2.Picture := Yellow.Picture;
		Player2.Hint := 'Yellow';
	end;
	
	Form1.Statusbar1.Panels[2].Text := Form1.Player1.Hint +' '+ IntToStr(Form1.Positions[1].NumberOfMoves) +' moves  '+
			Form1.Player2.Hint +' '+ IntToStr(Form1.Positions[2].NumberOfMoves) +' moves';
	
	DrawGrid1.Repaint;
end;

procedure TForm1.BSetupClick(Sender: TObject);
begin
	BSetup.Enabled := False;
	Continue.Enabled := True;
	Break := True;
	Setup := True;
end;

procedure TForm1.ContinueClick(Sender: TObject);
begin
	BSetup.Enabled := True;
	Continue.Enabled := False;
	Break := False;
	Setup := False;
	DrawGrid1.Selection := TGridRect(DrawGrid1.CellRect(-1, -1));
	Protocol.Fields[1].Field := Field;
	Protocol.NumberOfMoves := 1;
	Protocol.Current := 1;
	UpDown3.Position := Protocol.Current;
	Game(Players);
end;

procedure TForm1.DrawGrid1Click(Sender: TObject);
begin
	if not Setup then
		Exit;
		
	repeat
		Field[DrawGrid1.Col + 1, DrawGrid1.Row + 1] := (Field[DrawGrid1.Col + 1, DrawGrid1.Row + 1] + 1) mod 3;
	until not ((Field[DrawGrid1.Col + 1, DrawGrid1.Row + 1] = 0) and (DrawGrid1.Col + 1 in [4, 5]) and (DrawGrid1.Row + 1 in [4, 5]));
	
	DrawGrid1.Repaint;
end;

procedure TForm1.UpDown3Click(Sender: TObject; Button: TUDBtnType);
begin
	if Button = btNext then
		if Protocol.Current + 1 <= Protocol.NumberOfMoves then
			Inc(Protocol.Current);
	
	if Button = btPrev then
		if Protocol.Current - 1 >= 1 then
			Dec(Protocol.Current);
	
	UpDown3.Position := Protocol.Current;
	Field := Protocol.Fields[Protocol.Current].Field;
	
	DrawGrid1.Repaint;
end;

end.

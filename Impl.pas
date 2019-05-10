
unit Impl;


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
	
	TMainWindow = class(TForm)
		m__DrawGrid: TDrawGrid;
		m__Statusbar: TStatusBar;
		m__Progressbar: TProgressBar;
		m__BackForwardButtons: TUpDown;

		m__MainMenu: TMainMenu;
		m__MenuItem__new_game: TMenuItem;
		m__MenuItem_position: TMenuItem;
		m__MenuItem_position_modify: TMenuItem;
		m__MenuItem_position_continue: TMenuItem;
		m__MenuItem_players: TMenuItem;
		m__MenuItem__players__2_players: TMenuItem;
		m__MenuItem__players__human_vs_cpu: TMenuItem;
		m__MenuItem__players__cpu_vs_human: TMenuItem;
		m__MenuItem__players__cpu_vs_cpu: TMenuItem;
		m__MenuItem_colour: TMenuItem;
		m__MenuItem__colour__1st_player: TMenuItem;
		m__MenuItem__colour__1st_player__blue: TMenuItem;
		m__MenuItem__colour__1st_player__green: TMenuItem;
		m__MenuItem__colour__1st_player__red: TMenuItem;
		m__MenuItem__colour__1st_player__yellow: TMenuItem;
		m__MenuItem__colour__2nd_player: TMenuItem;
		m__MenuItem__colour__2nd_player__blue: TMenuItem;
		m__MenuItem__colour__2nd_player__green: TMenuItem;
		m__MenuItem__colour__2nd_player__red: TMenuItem;
		m__MenuItem__colour__2nd_player__yellow: TMenuItem;

		m__Player1Piece: TImage;
		m__Player2Piece: TImage;
		m__Player1PieceCountEdit: TEdit;
		m__Player2PieceCountEdit: TEdit;
		m__Player1Label: TLabel;
		m__Player2Label: TLabel;

		m__ColumnLabel: TLabel;
		m__Row1Label: TLabel;
		m__Row2Label: TLabel;
		m__Row3Label: TLabel;
		m__Row4Label: TLabel;
		m__Row5Label: TLabel;
		m__Row6Label: TLabel;
		m__Row7Label: TLabel;
		m__Row8Label: TLabel;

		m__LessOrEqual: TImage;
		m__GreaterOrEqual: TImage;

		m__MidgameDepthUpDown: TUpDown;
		m__EndgameDepthUpDown: TUpDown;
		m__MidgameDepthLabeledEdit: TLabeledEdit;
		m__EndgameDepthLabeledEdit: TLabeledEdit;

		m__GreenPiece: TImage;
		m__BluePiece: TImage;
		m__YellowPiece: TImage;
		m__RedPiece: TImage;
		
		
		procedure OnCreate_MainWindow(Sender: TObject);
		procedure OnClose_MainWindow(Sender: TObject; var Action: TCloseAction);
		procedure OnDrawCell_DrawGrid(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
		procedure OnClick_DrawGrid(Sender: TObject);
		procedure OnClick_BackForwardButtons(Sender: TObject; Button: TUDBtnType);

		procedure OnClick_MenuItem__new_game(Sender: TObject);
		procedure OnClick_MenuItem_position_modify(Sender: TObject);
		procedure OnClick_MenuItem_position_continue(Sender: TObject);
		procedure OnClick_MenuItem__players__any_submenu(Sender: TObject);
		procedure OnClick_MenuItem__colour__1st_player__any_submenu(Sender: TObject);
		procedure OnClick_MenuItem__colour__2nd_player__any_submenu(Sender: TObject);
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
	MainWindow: TMainWindow;


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
		Application.ProcessMessages;		//without this window becomes unresponsive
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
	MainWindow.m__Statusbar.Panels[0].Text := 'Thinking...';
	
	MainWindow.m__Progressbar.Max := MainWindow.Positions[Piece].NumberOfMoves;
	MainWindow.m__Progressbar.Position := 0;
	
	Move.I := 1;
	Move.J := 1;
	
	//for Depth := 1 to 15 do
	//	TransPos[Depth].NumberOfPos := 0;
	
	for Depth := 1 to 8 do
		for MaxDepth := 1 to 8 do
			MainWindow.Values.V[Depth, MaxDepth] := '';
	
	if 64 - Pieces(Field, 1) - Pieces(Field, 2) <= StrToInt(MainWindow.m__EndgameDepthLabeledEdit.Text) then
		MaxDepth := StrToInt(MainWindow.m__EndgameDepthLabeledEdit.Text)
	else
		MaxDepth := StrToInt(MainWindow.m__MidgameDepthLabeledEdit.Text);
	
	MainWindow.m__Statusbar.Panels[3].Text := 'Depth='+ IntToStr(MaxDepth);
	
	Res.WinLossDraw := False;
	MainWindow.m__MenuItem_position_modify.Enabled := False;
	
	MainWindow.m__MidgameDepthUpDown.Enabled := False;
	MainWindow.m__EndgameDepthUpDown.Enabled := False;
	
	MainWindow.m__DrawGrid.Repaint;
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
			MainWindow.Values.Best := Move;
			MainWindow.m__Statusbar.Panels[1].Text := StrValue1(Res);
		end;
		
		MainWindow.Values.V[CompPosition.BranchPositions[I].Move.I, CompPosition.BranchPositions[I].Move.J] := StrValue2(Res);
		MainWindow.m__DrawGrid.Repaint;
		MainWindow.m__Progressbar.Position := MainWindow.m__Progressbar.Position + 1;
		
		if BetterOrEqual(Max, MaxValue, True) then
			goto done;
	end;
	
	done:
	
	MainWindow.m__Statusbar.Panels[0].Text := 'Ready';
	MainWindow.m__MenuItem_position_modify.Enabled := True;
	
	MainWindow.m__MidgameDepthUpDown.Enabled := True;
	MainWindow.m__EndgameDepthUpDown.Enabled := True;
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
		Application.ProcessMessages;		//without this window becomes unresponsive
		
		if MainWindow.Break then
			Exit;
	until LegalMove(MainWindow.m__DrawGrid.Col + 1, MainWindow.m__DrawGrid.Row + 1, MainWindow.Positions[Piece], C);
	
	Move.I := MainWindow.m__DrawGrid.Col + 1;
	Move.J := MainWindow.m__DrawGrid.Row + 1;
end;

procedure Game(Players: TPlayers);
var
	Move: TMove;
	C1, C2: Byte;
	MoveId: Byte;
	GameEnd: Boolean;
begin
	MainWindow.m__MenuItem_position_modify.Enabled := True;
	
	MainWindow.Values.Last.I := 1;
	MainWindow.Values.Last.J := 1;
	
	MainWindow.m__Player1PieceCountEdit.Text := IntToStr(Pieces(MainWindow.Field, 1));
	MainWindow.m__Player2PieceCountEdit.Text := IntToStr(Pieces(MainWindow.Field, 2));
	
	GetPosition(MainWindow.Field, 1, MainWindow.Positions[1]);
	GetPosition(MainWindow.Field, 2, MainWindow.Positions[2]);
	
	for C1 := 1 to 8 do
		for C2 := 1 to 8 do
			MainWindow.Values.V[C1, C2] := '';
			
	MainWindow.m__DrawGrid.Repaint;
	
	MainWindow.m__Statusbar.Panels[2].Text := MainWindow.m__Player1Piece.Hint +' '+ IntToStr(MainWindow.Positions[1].NumberOfMoves) +' moves  '+
			MainWindow.m__Player2Piece.Hint +' '+ IntToStr(MainWindow.Positions[2].NumberOfMoves) +' moves';
		
	GameEnd := False;
	MoveId := 1;
	
	if MainWindow.Positions[1].NumberOfMoves = 0 then
		if MainWindow.Positions[2].NumberOfMoves = 0 then
			GameEnd := True;
		
	while not GameEnd do
	begin
		if MainWindow.Break then
			Exit;
		
		MainWindow.m__DrawGrid.Selection := TGridRect(MainWindow.m__DrawGrid.CellRect(-1, -1));
		
		GetPosition(MainWindow.Field, 1, MainWindow.Positions[1]);
		GetPosition(MainWindow.Field, 2, MainWindow.Positions[2]);
		
		if MainWindow.Positions[MoveId].NumberOfMoves = 0 then
			if MainWindow.Positions[3 - MoveId].NumberOfMoves = 0 then
				GameEnd := True
			else
			begin
				if Players[MoveId].Name = 'human' then
					ShowMessage('pass move');
				MoveId := 3 - MoveId;
			end;
			
		if not GameEnd then
			Players[MoveId].GetMove(Move, MainWindow.Field, MoveId);
			
		if LegalMove(Move.I, Move.J, MainWindow.Positions[MoveId], C1) then
		begin
			MainWindow.Field := MainWindow.Positions[MoveId].BranchPositions[C1].Field;
			MainWindow.Values.Last := Move;
			MainWindow.Protocol.NumberOfMoves := MainWindow.Protocol.NumberOfMoves + 1;
			MainWindow.Protocol.Current := MainWindow.Protocol.Current + 1;
			MainWindow.m__BackForwardButtons.Position := MainWindow.Protocol.Current;
			MainWindow.Protocol.Fields[MainWindow.Protocol.NumberOfMoves].Field := MainWindow.Field;
			MainWindow.Protocol.Fields[MainWindow.Protocol.NumberOfMoves].PlayerID := MoveId;
			MainWindow.m__DrawGrid.Repaint;
		end;
		
		MoveId := 3 - MoveId;
		
		GetPosition(MainWindow.Field, 1, MainWindow.Positions[1]);
		GetPosition(MainWindow.Field, 2, MainWindow.Positions[2]);
		
		MainWindow.m__Statusbar.Panels[2].Text := MainWindow.m__Player1Piece.Hint +' '+ IntToStr(MainWindow.Positions[1].NumberOfMoves) +' moves  '+
				MainWindow.m__Player2Piece.Hint +' '+ IntToStr(MainWindow.Positions[2].NumberOfMoves) +' moves';
		
		MainWindow.m__Player1PieceCountEdit.Text := IntToStr(Pieces(MainWindow.Field, 1));
		MainWindow.m__Player2PieceCountEdit.Text := IntToStr(Pieces(MainWindow.Field, 2));
	end;
	
	C1 := Pieces(MainWindow.Field, 1);
	C2 := Pieces(MainWindow.Field, 2);
	
	if C1 > C2 then
		ShowMessage('Winner is '+ Players[1].name)
	else if C2 > C1 then
		ShowMessage('Winner is '+ Players[2].name)
	else
		ShowMessage('Draw');
			
	MainWindow.m__MenuItem_position_modify.Enabled := True;
end;

procedure TMainWindow.OnCreate_MainWindow(Sender: TObject);
begin
	Self.m__MenuItem__players__human_vs_cpu.Checked := True;
	Self.m__MenuItem__players__cpu_vs_cpu.Checked := False;
	Self.m__MenuItem__players__cpu_vs_human.Checked := False;
	Self.m__MenuItem__players__2_players.Checked := False;
	
	if Self.m__MenuItem__colour__1st_player__blue.Checked then
	begin
		Self.m__Player1Piece.Picture := Self.m__BluePiece.Picture;
		Self.m__Player1Piece.Hint := 'Blue';
	end;
	
	if Self.m__MenuItem__colour__1st_player__green.Checked then
	begin
		Self.m__Player1Piece.Picture := Self.m__GreenPiece.Picture;
		Self.m__Player1Piece.Hint := 'Green';
	end;
	
	if Self.m__MenuItem__colour__1st_player__red.Checked then
	begin
		Self.m__Player1Piece.Picture := Self.m__RedPiece.Picture;
		Self.m__Player1Piece.Hint := 'Red';
	end;
	
	if Self.m__MenuItem__colour__1st_player__yellow.Checked then
	begin
		Self.m__Player1Piece.Picture := Self.m__YellowPiece.Picture;
		Self.m__Player1Piece.Hint := 'Yellow';
	end;
	
	if Self.m__MenuItem__colour__2nd_player__blue.Checked then
	begin
		Self.m__Player2Piece.Picture := Self.m__BluePiece.Picture;
		Self.m__Player2Piece.Hint := 'Blue';
	end;
	
	if Self.m__MenuItem__colour__2nd_player__green.Checked then
	begin
		Self.m__Player2Piece.Picture := Self.m__GreenPiece.Picture;
		Self.m__Player2Piece.Hint := 'Green';
	end;
	
	if Self.m__MenuItem__colour__2nd_player__red.Checked then
	begin
		Self.m__Player2Piece.Picture := Self.m__RedPiece.Picture;
		Self.m__Player2Piece.Hint := 'Red';
	end;
	
	if Self.m__MenuItem__colour__2nd_player__yellow.Checked then
	begin
		Self.m__Player2Piece.Picture := Self.m__YellowPiece.Picture;
		Self.m__Player2Piece.Hint := 'Yellow';
	end;
	
	Self.Break := False;
	Self.Setup := False;
	
	Self.m__ColumnLabel.Top := 0;
	Self.m__Row1Label.Left := 5;
	Self.m__Row2Label.Left := 5;
	Self.m__Row3Label.Left := 5;
	Self.m__Row4Label.Left := 5;
	Self.m__Row5Label.Left := 5;
	Self.m__Row6Label.Left := 5;
	Self.m__Row7Label.Left := 5;
	Self.m__Row8Label.Left := 5;
	
	Self.m__DrawGrid.Height := Self.m__DrawGrid.GridLineWidth + Self.m__DrawGrid.RowCount * (Self.m__DrawGrid.DefaultRowHeight + Self.m__DrawGrid.GridLineWidth) + 2;
	Self.m__DrawGrid.Width := Self.m__DrawGrid.GridLineWidth + Self.m__DrawGrid.ColCount * (Self.m__DrawGrid.DefaultColWidth + Self.m__DrawGrid.GridLineWidth) + 2;
	Self.m__DrawGrid.Top := Self.m__ColumnLabel.Height + Self.m__ColumnLabel.Top;
	Self.m__DrawGrid.Left := Self.m__Row1Label.Left + Self.m__Row1Label.Width + 5;
	
	Self.m__ColumnLabel.Left := Self.m__DrawGrid.Left;
	Self.m__Row1Label.Top := Self.m__DrawGrid.Top + 1 + 15 - 6;
	Self.m__Row2Label.Top := Self.m__Row1Label.Top + 31;
	Self.m__Row3Label.Top := Self.m__Row2Label.Top + 31;
	Self.m__Row4Label.Top := Self.m__Row3Label.Top + 31;
	Self.m__Row5Label.Top := Self.m__Row4Label.Top + 31;
	Self.m__Row6Label.Top := Self.m__Row5Label.Top + 31;
	Self.m__Row7Label.Top := Self.m__Row6Label.Top + 31;
	Self.m__Row8Label.Top := Self.m__Row7Label.Top + 31;
	
	Self.m__Player1Piece.Left := Self.m__DrawGrid.Left + Self.m__DrawGrid.Width + 60;
	Self.m__Player1Piece.Top := 10;
	Self.m__Player1Piece.Width := Self.m__Player1Piece.Picture.Width;
	Self.m__Player1Piece.Height := Self.m__Player1Piece.Picture.Height;
	Self.m__Player2Piece.Left := Self.m__Player1Piece.Left;
	Self.m__Player2Piece.Top := Self.m__Player1Piece.Top + Self.m__Player1Piece.Height + 20;
	Self.m__Player2Piece.Width := Self.m__Player1Piece.Picture.Width;
	Self.m__Player2Piece.Height := Self.m__Player1Piece.Picture.Height;
	
	Self.m__Player1PieceCountEdit.Width := 20;
	Self.m__Player2PieceCountEdit.Width := Self.m__Player1PieceCountEdit.Width;
	Self.m__Player1PieceCountEdit.Height := 15;
	Self.m__Player2PieceCountEdit.Height := Self.m__Player1PieceCountEdit.Height;
	Self.m__Player1PieceCountEdit.Left := Self.m__Player1Piece.Left - Self.m__Player1PieceCountEdit.Width - 35;
	Self.m__Player1PieceCountEdit.Top := Self.m__Player1Piece.Top + Self.m__Player1Piece.Height - Self.m__Player1PieceCountEdit.Height;
	Self.m__Player2PieceCountEdit.Left := Self.m__Player1PieceCountEdit.Left;
	Self.m__Player2PieceCountEdit.Top := Self.m__Player2Piece.Top + Self.m__Player2Piece.Height - Self.m__Player2PieceCountEdit.Height;
	
	Self.m__Player1Label.Left := Self.m__Player1PieceCountEdit.Left - 3;
	Self.m__Player1Label.Top := Self.m__Player1Piece.Top;
	Self.m__Player2Label.Left := Self.m__Player2PieceCountEdit.Left - 3;
	Self.m__Player2Label.Top := Self.m__Player2Piece.Top;
	
	Self.ClientHeight := Self.m__DrawGrid.Height + Self.m__Statusbar.Height + Self.m__ColumnLabel.Height + Self.m__BackForwardButtons.Height;
	Self.ClientWidth := Self.m__DrawGrid.Width + Self.m__Row1Label.Width + 110 + 5 + 5;
	
	Self.m__Progressbar.Left := Self.m__DrawGrid.Left + Self.m__DrawGrid.Width + 5;
	Self.m__Progressbar.Width := Self.ClientWidth - Self.m__DrawGrid.Width - Self.m__Row1Label.Width - 20;
	
	Self.m__MidgameDepthLabeledEdit.Left := Self.m__Progressbar.Left;
	Self.m__EndgameDepthLabeledEdit.Left := Self.m__MidgameDepthLabeledEdit.left;
	
	Self.m__BackForwardButtons.Top := Self.m__DrawGrid.Top + Self.m__DrawGrid.Height;
	
	Self.Show;
	//NewClick(Sender);
end;

procedure TMainWindow.OnClose_MainWindow(Sender: TObject; var Action: TCloseAction);
begin
	Halt;
end;

procedure TMainWindow.OnDrawCell_DrawGrid(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
	Text: string;
begin
	Self.m__DrawGrid.Canvas.Brush.Color := clWhite;
	Self.m__DrawGrid.Canvas.FillRect(Rect);
	
	case Self.Field[ACol + 1, ARow + 1] of
		1:
			Self.m__DrawGrid.Canvas.StretchDraw(Rect, Self.m__Player1Piece.Picture.Bitmap);
		2:
			Self.m__DrawGrid.Canvas.StretchDraw(Rect, Self.m__Player2Piece.Picture.Bitmap);
	end;
	
	if (Self.Values.Last.I = ACol + 1) and (Self.Values.Last.J = ARow + 1) and (Self.Field[ACol + 1, ARow + 1] > 0) then
	begin
		Self.m__DrawGrid.Canvas.Pen.Color := clRed;
		Self.m__DrawGrid.Canvas.MoveTo(Rect.Left, Rect.Top);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Right, Rect.Top);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Right, Rect.Bottom);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Left, Rect.Bottom);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Left, Rect.Top);
	end;
	
	if (ACol + 1 = Values.Best.I) and (ARow + 1 = Values.Best.J) then
		Self.m__DrawGrid.Canvas.Font.Color := clRed
	else
		Self.m__DrawGrid.Canvas.Font.Color := clBlack;
		
	Text := Self.Values.V[ACol + 1, ARow + 1];
	
	if Copy(Text, 1, 2) = '<=' then
		with Self.m__DrawGrid.Canvas do
		begin
			Delete(Text, 1, 2);
			Pen.Color := clBlack;
			Draw(Rect.Left, Rect.Top, Self.m__LessOrEqual.Picture.Graphic);
			TextOut(Rect.Left + 8, Rect.Top, Text);
		end
	else
		if Copy(Text, 1, 2) = '>=' then
			with Self.m__DrawGrid.Canvas do
			begin
				Delete(Text, 1, 2);
				Pen.Color := clBlack;
				Draw(Rect.Left, Rect.Top, Self.m__GreaterOrEqual.Picture.Graphic);
				TextOut(Rect.Left + 8, Rect.Top, Text);
			end
		else
			Self.m__DrawGrid.Canvas.TextOut(Rect.Left, Rect.Top, Text);
end;

procedure TMainWindow.OnClick_DrawGrid(Sender: TObject);
begin
	if not Self.Setup then
		Exit;
		
	repeat
		Self.Field[Self.m__DrawGrid.Col + 1, Self.m__DrawGrid.Row + 1] := (Self.Field[Self.m__DrawGrid.Col + 1, Self.m__DrawGrid.Row + 1] + 1) mod 3;
	until not ((Self.Field[Self.m__DrawGrid.Col + 1, Self.m__DrawGrid.Row + 1] = 0) and (Self.m__DrawGrid.Col + 1 in [4, 5]) and (Self.m__DrawGrid.Row + 1 in [4, 5]));
	
	Self.m__DrawGrid.Repaint;
end;

procedure TMainWindow.OnClick_BackForwardButtons(Sender: TObject; Button: TUDBtnType);
begin
	if Button = btNext then
		if Self.Protocol.Current + 1 <= Self.Protocol.NumberOfMoves then
			Inc(Self.Protocol.Current);
	
	if Button = btPrev then
		if Self.Protocol.Current - 1 >= 1 then
			Dec(Self.Protocol.Current);
	
	Self.m__BackForwardButtons.Position := Self.Protocol.Current;
	Self.Field := Self.Protocol.Fields[Self.Protocol.Current].Field;
	
	Self.m__DrawGrid.Repaint;
end;

procedure TMainWindow.OnClick_MenuItem__new_game(Sender: TObject);
begin
	Clear(Self.Field);
	SetField(Self.Field);
	Clear(Self.Protocol.Fields[1].Field);
	SetField(Self.Protocol.Fields[1].Field);
	
	Self.Protocol.NumberOfMoves := 1;
	Self.Protocol.Current := 1;
	
	Self.m__BackForwardButtons.Position := Self.Protocol.Current;
	
	Self.m__DrawGrid.DefaultDrawing := False;
	
	Self.m__Statusbar.Panels[0].Text := '';
	Self.m__Statusbar.Panels[1].Text := '';
	
	if Self.m__MenuItem__players__cpu_vs_cpu.Checked or Self.m__MenuItem__players__cpu_vs_human.Checked then
	begin
		Self.Players[1].GetMove := CompMove;
		Self.Players[1].Name := 'CPU';
	end
	else
	begin
		Self.Players[1].GetMove := PlayerMove;
		Self.Players[1].Name := 'human';
	end;
	
	if Self.m__MenuItem__players__2_players.Checked or Self.m__MenuItem__players__cpu_vs_human.Checked then
	begin
		Self.Players[2].GetMove := PlayerMove;
		Self.Players[2].Name := 'human';
	end
	else
	begin
		Self.Players[2].GetMove := CompMove;
		Self.Players[2].Name := 'CPU';
	end;
	
	Self.m__Player1Label.Caption := Self.Players[1].Name;
	Self.m__Player2Label.Caption := Self.Players[2].Name;
	Game(Self.Players);
	Self.m__DrawGrid.Repaint;
end;

procedure TMainWindow.OnClick_MenuItem_position_modify(Sender: TObject);
begin
	Self.m__MenuItem_position_modify.Enabled := False;
	Self.m__MenuItem_position_continue.Enabled := True;
	Self.Break := True;
	Self.Setup := True;
end;

procedure TMainWindow.OnClick_MenuItem_position_continue(Sender: TObject);
begin
	Self.m__MenuItem_position_modify.Enabled := True;
	Self.m__MenuItem_position_continue.Enabled := False;
	Self.Break := False;
	Self.Setup := False;
	Self.m__DrawGrid.Selection := TGridRect(Self.m__DrawGrid.CellRect(-1, -1));
	Self.Protocol.Fields[1].Field := Self.Field;
	Self.Protocol.NumberOfMoves := 1;
	Self.Protocol.Current := 1;
	Self.m__BackForwardButtons.Position := Self.Protocol.Current;
	Game(Self.Players);
end;

procedure TMainWindow.OnClick_MenuItem__players__any_submenu(Sender: TObject);
begin
	Self.m__MenuItem__players__human_vs_cpu.Checked := False;
	Self.m__MenuItem__players__cpu_vs_cpu.Checked := False;
	Self.m__MenuItem__players__cpu_vs_human.Checked := False;
	Self.m__MenuItem__players__2_players.Checked := False;
	
	(Sender as TMenuItem).Checked := True;
	
	if Self.m__MenuItem__players__cpu_vs_cpu.Checked or Self.m__MenuItem__players__cpu_vs_human.Checked then
	begin
		Self.Players[1].GetMove := CompMove;
		Self.Players[1].Name := 'CPU';
	end
	else
	begin
		Self.Players[1].GetMove := PlayerMove;
		Self.Players[1].Name := 'human';
	end;
	
	if Self.m__MenuItem__players__2_players.Checked or Self.m__MenuItem__players__cpu_vs_human.Checked then
	begin
		Self.Players[2].GetMove := PlayerMove;
		Self.Players[2].Name := 'human';
	end
	else
	begin
		Self.Players[2].GetMove := CompMove;
		Self.Players[2].Name := 'CPU';
	end;
	
	Self.m__Player1Label.Caption := Self.Players[1].Name;
	Self.m__Player2Label.Caption := Self.Players[2].Name;
end;
	
procedure TMainWindow.OnClick_MenuItem__colour__1st_player__any_submenu(Sender: TObject);
begin
	Self.m__MenuItem__colour__1st_player__blue.Checked := False;
	Self.m__MenuItem__colour__1st_player__green.Checked := False;
	Self.m__MenuItem__colour__1st_player__red.Checked := False;
	Self.m__MenuItem__colour__1st_player__yellow.Checked := False;
	
	(Sender as TMenuItem).Checked := True;
	
	if Self.m__MenuItem__colour__1st_player__blue.Checked then
	begin
		Self.m__Player1Piece.Picture := Self.m__BluePiece.Picture;
		Self.m__Player1Piece.Hint := 'Blue';
	end;
	
	if Self.m__MenuItem__colour__1st_player__green.Checked then
	begin
		Self.m__Player1Piece.Picture := Self.m__GreenPiece.Picture;
		Self.m__Player1Piece.Hint := 'Green';
	end;
	
	if Self.m__MenuItem__colour__1st_player__red.Checked then
	begin
		Self.m__Player1Piece.Picture := Self.m__RedPiece.Picture;
		Self.m__Player1Piece.Hint := 'Red';
	end;
	
	if Self.m__MenuItem__colour__1st_player__yellow.Checked then
	begin
		Self.m__Player1Piece.Picture := Self.m__YellowPiece.Picture;
		Self.m__Player1Piece.Hint := 'Yellow';
	end;
	
	Self.m__Statusbar.Panels[2].Text := Self.m__Player1Piece.Hint +' '+ IntToStr(Self.Positions[1].NumberOfMoves) +' moves  '+
			Self.m__Player2Piece.Hint +' '+ IntToStr(Self.Positions[2].NumberOfMoves) +' moves';
	
	Self.m__DrawGrid.Repaint;
end;

procedure TMainWindow.OnClick_MenuItem__colour__2nd_player__any_submenu(Sender: TObject);
begin
	Self.m__MenuItem__colour__2nd_player__blue.Checked := False;
	Self.m__MenuItem__colour__2nd_player__green.Checked := False;
	Self.m__MenuItem__colour__2nd_player__red.Checked := False;
	Self.m__MenuItem__colour__2nd_player__yellow.Checked := False;
	
	(Sender as TMenuItem).Checked := True;
	
	if Self.m__MenuItem__colour__2nd_player__blue.Checked then
	begin
		Self.m__Player2Piece.Picture := Self.m__BluePiece.Picture;
		Self.m__Player2Piece.Hint := 'Blue';
	end;
	
	if Self.m__MenuItem__colour__2nd_player__green.Checked then
	begin
		Self.m__Player2Piece.Picture := Self.m__GreenPiece.Picture;
		Self.m__Player2Piece.Hint := 'Green';
	end;
	
	if Self.m__MenuItem__colour__2nd_player__red.Checked then
	begin
		Self.m__Player2Piece.Picture := Self.m__RedPiece.Picture;
		Self.m__Player2Piece.Hint := 'Red';
	end;
	
	if Self.m__MenuItem__colour__2nd_player__yellow.Checked then
	begin
		Self.m__Player2Piece.Picture := Self.m__YellowPiece.Picture;
		Self.m__Player2Piece.Hint := 'Yellow';
	end;
	
	Self.m__Statusbar.Panels[2].Text := Self.m__Player1Piece.Hint +' '+ IntToStr(Self.Positions[1].NumberOfMoves) +' moves  '+
			Self.m__Player2Piece.Hint +' '+ IntToStr(Self.Positions[2].NumberOfMoves) +' moves';
	
	Self.m__DrawGrid.Repaint;
end;

end.

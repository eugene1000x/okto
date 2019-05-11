
unit Impl;


interface


uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Menus, Grids, StdCtrls, ExtCtrls, ImgList, ComCtrls, ShellCtrls;

const
	BOARD_DIMENSION = 8;

type
	TPlayerNumber = 1..2;
	TCellState = 0..2;
	TWhoseTurn = 0..2;
	TCellNumber = 1..BOARD_DIMENSION;
	TBoardState = array [TCellNumber, TCellNumber] of TCellState;
	
	TCellAddress = record
		Column, Row: TCellNumber;
	end;
	
	TPlayer = record
		Name: string;
		GetMove: procedure(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TPlayerNumber);
	end;
	
	TPlayerArray = array [1..2] of TPlayer;
	TNumberOfSquares = 0 .. BOARD_DIMENSION * BOARD_DIMENSION;
	
	TChildPosition = record
		Move: TCellAddress;
		BoardState: TBoardState;
	end;
	
	TChildPositionArray = array [TNumberOfSquares] of TChildPosition;
	
	TPosition = record
		BoardState: TBoardState;
		i__WhoseTurn: TWhoseTurn;
		PossibleMoveCount: TNumberOfSquares;
		ChildPositions: TChildPositionArray;
	end;
	
	TEvaluation = record
		IsHeuristical, IsLessOrEqual, IsGreaterOrEqual: Boolean;
		PieceCount: Integer;
	end;
	
	//unfinished
	{TTranspositionTable = array [1..2, 1..15] of record
		PositionCount: Word;		//i__WhoseTurn, Depth
		Positions: array of record
			BoardState: TBoardState;
			Evaluation: TEvaluation;
		end;
	end;}
	
	TGameHistory = record
		Positions: array [TNumberOfSquares] of record
			BoardState: TBoardState;
			i__WhoseTurn: TPlayerNumber;
		end;
		MoveCount, CurrentMoveNumber: TNumberOfSquares;
	end;
	
	TMainWindow = class(TForm)
		published m__DrawGrid: TDrawGrid;
		published m__Statusbar: TStatusBar;
		published m__Progressbar: TProgressBar;
		published m__BackForwardButtons: TUpDown;

		published m__MainMenu: TMainMenu;
		published m__MenuItem__new_game: TMenuItem;
		published m__MenuItem_position: TMenuItem;
		published m__MenuItem_position_modify: TMenuItem;
		published m__MenuItem_position_continue: TMenuItem;
		published m__MenuItem_players: TMenuItem;
		published m__MenuItem__players__2_players: TMenuItem;
		published m__MenuItem__players__human_vs_cpu: TMenuItem;
		published m__MenuItem__players__cpu_vs_human: TMenuItem;
		published m__MenuItem__players__cpu_vs_cpu: TMenuItem;
		published m__MenuItem_colour: TMenuItem;
		published m__MenuItem__colour__1st_player: TMenuItem;
		published m__MenuItem__colour__1st_player__blue: TMenuItem;
		published m__MenuItem__colour__1st_player__green: TMenuItem;
		published m__MenuItem__colour__1st_player__red: TMenuItem;
		published m__MenuItem__colour__1st_player__yellow: TMenuItem;
		published m__MenuItem__colour__2nd_player: TMenuItem;
		published m__MenuItem__colour__2nd_player__blue: TMenuItem;
		published m__MenuItem__colour__2nd_player__green: TMenuItem;
		published m__MenuItem__colour__2nd_player__red: TMenuItem;
		published m__MenuItem__colour__2nd_player__yellow: TMenuItem;

		published m__Player1Piece: TImage;
		published m__Player2Piece: TImage;
		published m__Player1PieceCountEdit: TEdit;
		published m__Player2PieceCountEdit: TEdit;
		published m__Player1Label: TLabel;
		published m__Player2Label: TLabel;

		published m__ColumnLabel: TLabel;
		published m__Row1Label: TLabel;
		published m__Row2Label: TLabel;
		published m__Row3Label: TLabel;
		published m__Row4Label: TLabel;
		published m__Row5Label: TLabel;
		published m__Row6Label: TLabel;
		published m__Row7Label: TLabel;
		published m__Row8Label: TLabel;

		published m__LessOrEqual: TImage;
		published m__GreaterOrEqual: TImage;

		published m__MidgameDepthUpDown: TUpDown;
		published m__EndgameDepthUpDown: TUpDown;
		published m__MidgameDepthLabeledEdit: TLabeledEdit;
		published m__EndgameDepthLabeledEdit: TLabeledEdit;

		published m__GreenPiece: TImage;
		published m__BluePiece: TImage;
		published m__YellowPiece: TImage;
		published m__RedPiece: TImage;
		
		
		published procedure OnCreate_MainWindow(Sender: TObject);
		published procedure OnClose_MainWindow(Sender: TObject; var Action: TCloseAction);
		published procedure OnDrawCell_DrawGrid(Sender: TObject; Column, Row: Integer; Rect: TRect; State: TGridDrawState);
		published procedure OnClick_DrawGrid(Sender: TObject);
		published procedure OnClick_BackForwardButtons(Sender: TObject; Button: TUDBtnType);

		published procedure OnClick_MenuItem__new_game(Sender: TObject);
		published procedure OnClick_MenuItem_position_modify(Sender: TObject);
		published procedure OnClick_MenuItem_position_continue(Sender: TObject);
		published procedure OnClick_MenuItem__players__any_submenu(Sender: TObject);
		published procedure OnClick_MenuItem__colour__1st_player__any_submenu(Sender: TObject);
		published procedure OnClick_MenuItem__colour__2nd_player__any_submenu(Sender: TObject);


		public m__BoardState: TBoardState;
		public m__Positions: array [1..2] of TPosition;
		public m__Players: TPlayerArray;
		
		public m__Evaluations: record
			Last, Best: TCellAddress;
			CellEvaluations: array [1..8, 1..8] of string;
		end;
		
		public m__GameHistory: TGameHistory;
		public m__IsInModifyMode, m__DoBreakGame: Boolean;
	end;

var
	MainWindow: TMainWindow;


implementation
{$R *.dfm}


procedure ClearBoard(var BoardState: TBoardState);
var
	Column, Row: Byte;
begin
	for Column := 1 to 8 do
		for Row := 1 to 8 do
			BoardState[Column, Row] := 0;
end;

procedure InitBoard(var BoardState: TBoardState);
begin
	BoardState[4, 4] := 2;
	BoardState[5, 4] := 1;
	BoardState[4, 5] := 1;
	BoardState[5, 5] := 2;
end;

procedure AnalyzePosition(BoardState: TBoardState; i__WhoseTurn: TWhoseTurn; var Position: TPosition);
var
	Column, Row: TCellNumber;
	ReversedCount: TNumberOfSquares;
	K, L: Byte;
	Vx, Vy: -1..1;
begin
	Position.PossibleMoveCount := 0;
	Position.BoardState := BoardState;
	Position.i__WhoseTurn := 0;
	
	for Column := 1 to 8 do
		for Row := 1 to 8 do
		begin
			ReversedCount := 0;
			Position.ChildPositions[Position.PossibleMoveCount + 1].BoardState := BoardState;
			
			for Vx := -1 to 1 do
				for Vy := -1 to 1 do
					if (BoardState[Column, Row] = 0) and (not ((Vx = 0) and (Vy = 0))) then
						if (Column + Vx in [1..8]) and (Row + Vy in [1..8]) and (BoardState[Column + Vx, Row + Vy] = 3 - i__WhoseTurn) then
						begin
							K := 2;
							
							while (Column + K * Vx in [1..8]) and (Row + K * Vy in [1..8]) and (BoardState[Column + K * Vx, Row + K * Vy] = 3 - i__WhoseTurn) do
								Inc(K);
								
							if (Column + K * Vx in [1..8]) and (Row + K * Vy in [1..8]) and (BoardState[Column + K * Vx, Row + K * Vy] = i__WhoseTurn) then
							begin
								for L := 0 to K - 1 do
									Position.ChildPositions[Position.PossibleMoveCount + 1].BoardState[Column + L * Vx, Row + L * Vy] := i__WhoseTurn;
									
								Inc(ReversedCount, K - 1);
							end;
						end;
			if ReversedCount > 0 then
			begin
				Inc(Position.PossibleMoveCount);
				Position.ChildPositions[Position.PossibleMoveCount].Move.Column := Column;
				Position.ChildPositions[Position.PossibleMoveCount].Move.Row := Row;
				Position.i__WhoseTurn := i__WhoseTurn;
			end;
		end;
end;

function CountCellsWithState(BoardState: TBoardState; CellState: Byte): Byte;
var
	Column, Row: TCellNumber;
begin
	Result := 0;
	for Column := 1 to BOARD_DIMENSION do
		for Row := 1 to BOARD_DIMENSION do
			if BoardState[Column, Row] = CellState then
				Inc(Result);
end;

function IsMidgame(Field: TBoardState): Boolean;
begin
	Result := CountCellsWithState(Field, 1) + CountCellsWithState(Field, 2) < 56;
end;

function GetHeuristicalEvaluation(BoardState: TBoardState; PlayerNumber: TPlayerNumber): TEvaluation;
const
	G: array [1..4] of array [1..2] of 1..8 = ((1, 1), (1, 8), (8, 1), (8, 8));
const
	K: array [1..4] of array [1..2] of 1..8 = ((2, 2), (2, 7), (7, 2), (7, 7));
var
	I: Byte;
	Positions: array [1..2] of TPosition;
begin
	Result.PieceCount := 0;
	Result.IsHeuristical := True;
	Result.IsLessOrEqual := False;
	Result.IsGreaterOrEqual := False;
	
	AnalyzePosition(BoardState, PlayerNumber, Positions[PlayerNumber]);
	AnalyzePosition(BoardState, 3 - PlayerNumber, Positions[3 - PlayerNumber]);
	
	if (Positions[PlayerNumber].PossibleMoveCount = 0) and (Positions[3 - PlayerNumber].PossibleMoveCount = 0) then
	begin
		Result.IsHeuristical := False;
		Result.PieceCount := CountCellsWithState(BoardState, PlayerNumber) - CountCellsWithState(BoardState, 3 - PlayerNumber);
		Exit;
	end;
	
	if IsMidgame(BoardState) then
		Result.PieceCount := CountCellsWithState(BoardState, 3 - PlayerNumber) - CountCellsWithState(BoardState, PlayerNumber)
	else
		Result.PieceCount := CountCellsWithState(BoardState, PlayerNumber) - CountCellsWithState(BoardState, 3 - PlayerNumber);
	
	Result.PieceCount := Result.PieceCount + Positions[PlayerNumber].PossibleMoveCount;
	Result.PieceCount := Result.PieceCount - Positions[3 - PlayerNumber].PossibleMoveCount;
	
	for I := 1 to 4 do
		if (BoardState[G[I][1], G[I][2]] = PlayerNumber) or ((BoardState[K[I][1], K[I][2]] = 3 - PlayerNumber) and (BoardState[G[I][1], G[I][2]] = 0)) then
			Result.PieceCount := Result.PieceCount + 100
		else
			if (BoardState[G[I][1], G[I][2]] = 3 - PlayerNumber) or ((BoardState[K[I][1], K[I][2]] = PlayerNumber) and (BoardState[G[I][1], G[I][2]] = 0)) then
				Result.PieceCount := Result.PieceCount - 100;
end;

function IsGreaterOrEqual(Evaluation1, Evaluation2: TEvaluation; Equal: Boolean): Boolean;
begin
	if Evaluation1.IsHeuristical = Evaluation2.IsHeuristical then
		Result := (Evaluation1.PieceCount > Evaluation2.PieceCount) or ((Evaluation1.PieceCount = Evaluation2.PieceCount) and (Equal or ((Evaluation1.IsGreaterOrEqual) and (Evaluation2.IsGreaterOrEqual = False))))
	else
		if not Evaluation1.IsHeuristical then
			Result := Evaluation1.PieceCount >= 0
		else
			Result := Evaluation2.PieceCount < 0;
end;

function EvaluationToStr_Long(Evaluation: TEvaluation): string;
begin
	Result := IntToStr(Abs(Evaluation.PieceCount));
	
	if not Evaluation.IsHeuristical then
		if Evaluation.PieceCount > 0 then
			Result := 'Win by '+ Result
		else if Evaluation.PieceCount < 0 then
			Result := 'Loss by '+ Result
		else
			Result := 'Draw'
	else
		Result := IntToStr(Evaluation.PieceCount);
end;

function EvaluationToStr_Short(Evaluation: TEvaluation): string;
begin
	Result := IntToStr(Evaluation.PieceCount);
	
	if not Evaluation.IsHeuristical then
		Result := Result +'p';
		
	if Evaluation.IsLessOrEqual then
		Result := '<='+ Result
	else if Evaluation.IsGreaterOrEqual then
		Result := '>='+ Result;
end;

function NegateEvaluation(Evaluation: TEvaluation): TEvaluation;
begin
	Result := Evaluation;
	Result.PieceCount := -Evaluation.PieceCount;
	Result.IsGreaterOrEqual := Evaluation.IsLessOrEqual;
	Result.IsLessOrEqual := Evaluation.IsGreaterOrEqual;
end;

procedure GetEngineMove(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TPlayerNumber);
var
	I, Depth, MaxDepth: TNumberOfSquares;
	Max, Res, MinValue, MaxValue: TEvaluation;
	EnginePosition, ChildPosition: TPosition;
	DoSort: Boolean;
label
	done;

	function Evaluate(Position: TPosition; Alpha, Beta: TEvaluation): TEvaluation;
	var
		I: TNumberOfSquares;
		ChildPosition: TPosition;
		Max: TEvaluation;
	label
		done;
	begin
		Application.ProcessMessages;		//without this window becomes unresponsive
		Inc(Depth);
		
		if Depth = MaxDepth then
		begin
			Max := GetHeuristicalEvaluation(Position.BoardState, Position.i__WhoseTurn);
			goto done;
		end;
		
		Max := MinValue;
		
		for I := 1 to Position.PossibleMoveCount do
		begin
			AnalyzePosition(Position.ChildPositions[I].BoardState, 3 - Position.i__WhoseTurn, ChildPosition);
		
			if ChildPosition.PossibleMoveCount > 0 then
				Result := NegateEvaluation(Evaluate(ChildPosition, NegateEvaluation(Beta), NegateEvaluation(Alpha)))		// (Alpha >= Max) => (-Alpha <= -Max) => (Beta <= -Max)
			else
			begin
				AnalyzePosition(Position.ChildPositions[I].BoardState, Position.i__WhoseTurn, ChildPosition);
				
				if ChildPosition.PossibleMoveCount > 0 then
					Result := Evaluate(ChildPosition, Alpha, Beta)
				else
					Result := GetHeuristicalEvaluation(Position.ChildPositions[I].BoardState, Position.i__WhoseTurn);
			end;
			
			if IsGreaterOrEqual(Max, Alpha, True) then
				Alpha := Max;
			
			if IsGreaterOrEqual(Result, Max, False) then
			begin
				Max := Result;
				
				if (not Max.IsLessOrEqual) and (Max.IsGreaterOrEqual or IsGreaterOrEqual(Max, Beta, True)) then
				begin
					if Position.PossibleMoveCount > 1 then
					begin
						Max.IsLessOrEqual := False;
						Max.IsGreaterOrEqual := True;
					end;
					
					goto done;
				end;
			end;
		end;
		
		done:
		
		Result := Max;
		
		if IsGreaterOrEqual(Result, MaxValue, True) then
			Result.IsGreaterOrEqual := False;
			
		Dec(Depth);
	end;

begin
	MainWindow.m__Statusbar.Panels[0].Text := 'Thinking...';
	
	MainWindow.m__Progressbar.Max := MainWindow.m__Positions[PlayerNumber].PossibleMoveCount;
	MainWindow.m__Progressbar.Position := 0;
	
	Move.Column := 1;
	Move.Row := 1;
	
	//for Depth := 1 to 15 do
	//	TranspositionTable[Depth].PositionCount := 0;
	
	for Depth := 1 to 8 do
		for MaxDepth := 1 to 8 do
			MainWindow.m__Evaluations.CellEvaluations[Depth, MaxDepth] := '';
	
	if 64 - CountCellsWithState(BoardState, 1) - CountCellsWithState(BoardState, 2) <= StrToInt(MainWindow.m__EndgameDepthLabeledEdit.Text) then
		MaxDepth := StrToInt(MainWindow.m__EndgameDepthLabeledEdit.Text)
	else
		MaxDepth := StrToInt(MainWindow.m__MidgameDepthLabeledEdit.Text);
	
	MainWindow.m__Statusbar.Panels[3].Text := 'Depth='+ IntToStr(MaxDepth);
	
	Res.IsHeuristical := True;
	MainWindow.m__MenuItem_position_modify.Enabled := False;
	
	MainWindow.m__MidgameDepthUpDown.Enabled := False;
	MainWindow.m__EndgameDepthUpDown.Enabled := False;
	
	MainWindow.m__DrawGrid.Repaint;
	Depth := 0;
	
	AnalyzePosition(BoardState, PlayerNumber, EnginePosition);
	
	if EnginePosition.PossibleMoveCount = 1 then
	begin
		Move := EnginePosition.ChildPositions[1].Move;
		goto done;
	end;
	
	MinValue.IsHeuristical := False;
	MinValue.IsLessOrEqual := False;
	MinValue.IsGreaterOrEqual := False;
	MinValue.PieceCount := -64;
	
	MaxValue.IsHeuristical := False;
	MaxValue.IsLessOrEqual := False;
	MaxValue.IsGreaterOrEqual := False;
	MaxValue.PieceCount := 64;
	
	Max := MinValue;
	
	for I := 1 to EnginePosition.PossibleMoveCount do
	begin
		AnalyzePosition(EnginePosition.ChildPositions[I].BoardState, 3 - PlayerNumber, ChildPosition);
		
		if ChildPosition.PossibleMoveCount > 0 then
			if I < EnginePosition.PossibleMoveCount then
				Res := NegateEvaluation(Evaluate(ChildPosition, NegateEvaluation(MaxValue), NegateEvaluation(Max)))
			else
				Res := NegateEvaluation(Evaluate(ChildPosition, NegateEvaluation(Max), NegateEvaluation(Max)))
		else
		begin
			AnalyzePosition(EnginePosition.ChildPositions[I].BoardState, PlayerNumber, ChildPosition);
			
			if ChildPosition.PossibleMoveCount > 0 then
				if I < EnginePosition.PossibleMoveCount then
					Res := Evaluate(ChildPosition, Max, MaxValue)
				else
					Res := Evaluate(ChildPosition, Max, Max)
			else
				Res := GetHeuristicalEvaluation(EnginePosition.ChildPositions[I].BoardState, PlayerNumber);
		end;
		
		if (not Res.IsLessOrEqual) and (Res.IsGreaterOrEqual or IsGreaterOrEqual(Res, Max, True)) then
		begin
			Move := EnginePosition.ChildPositions[I].Move;
			Max := Res;
			MainWindow.m__Evaluations.Best := Move;
			MainWindow.m__Statusbar.Panels[1].Text := EvaluationToStr_Long(Res);
		end;
		
		MainWindow.m__Evaluations.CellEvaluations[EnginePosition.ChildPositions[I].Move.Column, EnginePosition.ChildPositions[I].Move.Row] := EvaluationToStr_Short(Res);
		MainWindow.m__DrawGrid.Repaint;
		MainWindow.m__Progressbar.Position := MainWindow.m__Progressbar.Position + 1;
		
		if IsGreaterOrEqual(Max, MaxValue, True) then
			goto done;
	end;
	
	done:
	
	MainWindow.m__Statusbar.Panels[0].Text := 'Ready';
	MainWindow.m__MenuItem_position_modify.Enabled := True;
	
	MainWindow.m__MidgameDepthUpDown.Enabled := True;
	MainWindow.m__EndgameDepthUpDown.Enabled := True;
end;

function IsLegalMove(Column, Row: TCellNumber; Position: TPosition; var MoveNumber: Byte): Boolean;
var
	I: Byte;
begin
	Result := False;
	
	for I := 1 to Position.PossibleMoveCount do
		if (Position.ChildPositions[I].Move.Column = Column) and (Position.ChildPositions[I].Move.Row = Row) then
		begin
			Result := True;
			MoveNumber := I;
			Break;
		end;
end;

procedure GetPlayerMove(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TPlayerNumber);
var
	MoveNumberDummy: Byte;
begin
	repeat
		Application.ProcessMessages;		//without this window becomes unresponsive
		
		if MainWindow.m__DoBreakGame then
			Exit;
	until IsLegalMove(MainWindow.m__DrawGrid.Col + 1, MainWindow.m__DrawGrid.Row + 1, MainWindow.m__Positions[PlayerNumber], MoveNumberDummy);
	
	Move.Column := MainWindow.m__DrawGrid.Col + 1;
	Move.Row := MainWindow.m__DrawGrid.Row + 1;
end;

procedure RunGame(Players: TPlayerArray);
var
	Move: TCellAddress;
	C1, C2: Byte;
	i__WhoseTurn: Byte;
	IsGameEnd: Boolean;
begin
	MainWindow.m__MenuItem_position_modify.Enabled := True;
	
	MainWindow.m__Evaluations.Last.Column := 1;
	MainWindow.m__Evaluations.Last.Row := 1;
	
	MainWindow.m__Player1PieceCountEdit.Text := IntToStr(CountCellsWithState(MainWindow.m__BoardState, 1));
	MainWindow.m__Player2PieceCountEdit.Text := IntToStr(CountCellsWithState(MainWindow.m__BoardState, 2));
	
	AnalyzePosition(MainWindow.m__BoardState, 1, MainWindow.m__Positions[1]);
	AnalyzePosition(MainWindow.m__BoardState, 2, MainWindow.m__Positions[2]);
	
	for C1 := 1 to 8 do
		for C2 := 1 to 8 do
			MainWindow.m__Evaluations.CellEvaluations[C1, C2] := '';
			
	MainWindow.m__DrawGrid.Repaint;
	
	MainWindow.m__Statusbar.Panels[2].Text := MainWindow.m__Player1Piece.Hint +' '+ IntToStr(MainWindow.m__Positions[1].PossibleMoveCount) +' moves  '+
			MainWindow.m__Player2Piece.Hint +' '+ IntToStr(MainWindow.m__Positions[2].PossibleMoveCount) +' moves';
		
	IsGameEnd := False;
	i__WhoseTurn := 1;
	
	if MainWindow.m__Positions[1].PossibleMoveCount = 0 then
		if MainWindow.m__Positions[2].PossibleMoveCount = 0 then
			IsGameEnd := True;
		
	while not IsGameEnd do
	begin
		if MainWindow.m__DoBreakGame then
			Exit;
		
		MainWindow.m__DrawGrid.Selection := TGridRect(MainWindow.m__DrawGrid.CellRect(-1, -1));
		
		AnalyzePosition(MainWindow.m__BoardState, 1, MainWindow.m__Positions[1]);
		AnalyzePosition(MainWindow.m__BoardState, 2, MainWindow.m__Positions[2]);
		
		if MainWindow.m__Positions[i__WhoseTurn].PossibleMoveCount = 0 then
			if MainWindow.m__Positions[3 - i__WhoseTurn].PossibleMoveCount = 0 then
				IsGameEnd := True
			else
			begin
				if Players[i__WhoseTurn].Name = 'human' then
					ShowMessage('pass move');
				i__WhoseTurn := 3 - i__WhoseTurn;
			end;
			
		if not IsGameEnd then
			Players[i__WhoseTurn].GetMove(Move, MainWindow.m__BoardState, i__WhoseTurn);
			
		if IsLegalMove(Move.Column, Move.Row, MainWindow.m__Positions[i__WhoseTurn], C1) then
		begin
			MainWindow.m__BoardState := MainWindow.m__Positions[i__WhoseTurn].ChildPositions[C1].BoardState;
			MainWindow.m__Evaluations.Last := Move;
			MainWindow.m__GameHistory.MoveCount := MainWindow.m__GameHistory.MoveCount + 1;
			MainWindow.m__GameHistory.CurrentMoveNumber := MainWindow.m__GameHistory.CurrentMoveNumber + 1;
			MainWindow.m__BackForwardButtons.Position := MainWindow.m__GameHistory.CurrentMoveNumber;
			MainWindow.m__GameHistory.Positions[MainWindow.m__GameHistory.MoveCount].BoardState := MainWindow.m__BoardState;
			MainWindow.m__GameHistory.Positions[MainWindow.m__GameHistory.MoveCount].i__WhoseTurn := i__WhoseTurn;
			MainWindow.m__DrawGrid.Repaint;
		end;
		
		i__WhoseTurn := 3 - i__WhoseTurn;
		
		AnalyzePosition(MainWindow.m__BoardState, 1, MainWindow.m__Positions[1]);
		AnalyzePosition(MainWindow.m__BoardState, 2, MainWindow.m__Positions[2]);
		
		MainWindow.m__Statusbar.Panels[2].Text := MainWindow.m__Player1Piece.Hint +' '+ IntToStr(MainWindow.m__Positions[1].PossibleMoveCount) +' moves  '+
				MainWindow.m__Player2Piece.Hint +' '+ IntToStr(MainWindow.m__Positions[2].PossibleMoveCount) +' moves';
		
		MainWindow.m__Player1PieceCountEdit.Text := IntToStr(CountCellsWithState(MainWindow.m__BoardState, 1));
		MainWindow.m__Player2PieceCountEdit.Text := IntToStr(CountCellsWithState(MainWindow.m__BoardState, 2));
	end;
	
	C1 := CountCellsWithState(MainWindow.m__BoardState, 1);
	C2 := CountCellsWithState(MainWindow.m__BoardState, 2);
	
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
	
	Self.m__DoBreakGame := False;
	Self.m__IsInModifyMode := False;
	
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
	Self.OnClick_MenuItem__new_game(Self.m__MenuItem__new_game);
end;

procedure TMainWindow.OnClose_MainWindow(Sender: TObject; var Action: TCloseAction);
begin
	Halt;
end;

procedure TMainWindow.OnDrawCell_DrawGrid(Sender: TObject; Column, Row: Integer; Rect: TRect; State: TGridDrawState);
var
	Text: string;
begin
	Self.m__DrawGrid.Canvas.Brush.Color := clWhite;
	Self.m__DrawGrid.Canvas.FillRect(Rect);
	
	case Self.m__BoardState[Column + 1, Row + 1] of
		1:
			Self.m__DrawGrid.Canvas.StretchDraw(Rect, Self.m__Player1Piece.Picture.Bitmap);
		2:
			Self.m__DrawGrid.Canvas.StretchDraw(Rect, Self.m__Player2Piece.Picture.Bitmap);
	end;
	
	if (Self.m__Evaluations.Last.Column = Column + 1) and (Self.m__Evaluations.Last.Row = Row + 1) and (Self.m__BoardState[Column + 1, Row + 1] > 0) then
	begin
		Self.m__DrawGrid.Canvas.Pen.Color := clRed;
		Self.m__DrawGrid.Canvas.MoveTo(Rect.Left, Rect.Top);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Right, Rect.Top);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Right, Rect.Bottom);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Left, Rect.Bottom);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Left, Rect.Top);
	end;
	
	if (Column + 1 = Self.m__Evaluations.Best.Column) and (Row + 1 = Self.m__Evaluations.Best.Row) then
		Self.m__DrawGrid.Canvas.Font.Color := clRed
	else
		Self.m__DrawGrid.Canvas.Font.Color := clBlack;
		
	Text := Self.m__Evaluations.CellEvaluations[Column + 1, Row + 1];
	
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
	if not Self.m__IsInModifyMode then
		Exit;
		
	repeat
		Self.m__BoardState[Self.m__DrawGrid.Col + 1, Self.m__DrawGrid.Row + 1] := (Self.m__BoardState[Self.m__DrawGrid.Col + 1, Self.m__DrawGrid.Row + 1] + 1) mod 3;
	until not ((Self.m__BoardState[Self.m__DrawGrid.Col + 1, Self.m__DrawGrid.Row + 1] = 0) and (Self.m__DrawGrid.Col + 1 in [4, 5]) and (Self.m__DrawGrid.Row + 1 in [4, 5]));
	
	Self.m__DrawGrid.Repaint;
end;

procedure TMainWindow.OnClick_BackForwardButtons(Sender: TObject; Button: TUDBtnType);
begin
	if Button = btNext then
		if Self.m__GameHistory.CurrentMoveNumber + 1 <= Self.m__GameHistory.MoveCount then
			Inc(Self.m__GameHistory.CurrentMoveNumber);
	
	if Button = btPrev then
		if Self.m__GameHistory.CurrentMoveNumber - 1 >= 1 then
			Dec(Self.m__GameHistory.CurrentMoveNumber);
	
	Self.m__BackForwardButtons.Position := Self.m__GameHistory.CurrentMoveNumber;
	Self.m__BoardState := Self.m__GameHistory.Positions[Self.m__GameHistory.CurrentMoveNumber].BoardState;
	
	Self.m__DrawGrid.Repaint;
end;

procedure TMainWindow.OnClick_MenuItem__new_game(Sender: TObject);
begin
	ClearBoard(Self.m__BoardState);
	InitBoard(Self.m__BoardState);
	ClearBoard(Self.m__GameHistory.Positions[1].BoardState);
	InitBoard(Self.m__GameHistory.Positions[1].BoardState);
	
	Self.m__GameHistory.MoveCount := 1;
	Self.m__GameHistory.CurrentMoveNumber := 1;
	
	Self.m__BackForwardButtons.Position := Self.m__GameHistory.CurrentMoveNumber;
	
	Self.m__DrawGrid.DefaultDrawing := False;
	
	Self.m__Statusbar.Panels[0].Text := '';
	Self.m__Statusbar.Panels[1].Text := '';
	
	if Self.m__MenuItem__players__cpu_vs_cpu.Checked or Self.m__MenuItem__players__cpu_vs_human.Checked then
	begin
		Self.m__Players[1].GetMove := GetEngineMove;
		Self.m__Players[1].Name := 'CPU';
	end
	else
	begin
		Self.m__Players[1].GetMove := GetPlayerMove;
		Self.m__Players[1].Name := 'human';
	end;
	
	if Self.m__MenuItem__players__2_players.Checked or Self.m__MenuItem__players__cpu_vs_human.Checked then
	begin
		Self.m__Players[2].GetMove := GetPlayerMove;
		Self.m__Players[2].Name := 'human';
	end
	else
	begin
		Self.m__Players[2].GetMove := GetEngineMove;
		Self.m__Players[2].Name := 'CPU';
	end;
	
	Self.m__Player1Label.Caption := Self.m__Players[1].Name;
	Self.m__Player2Label.Caption := Self.m__Players[2].Name;
	RunGame(Self.m__Players);
	Self.m__DrawGrid.Repaint;
end;

procedure TMainWindow.OnClick_MenuItem_position_modify(Sender: TObject);
begin
	Self.m__MenuItem_position_modify.Enabled := False;
	Self.m__MenuItem_position_continue.Enabled := True;
	Self.m__DoBreakGame := True;
	Self.m__IsInModifyMode := True;
end;

procedure TMainWindow.OnClick_MenuItem_position_continue(Sender: TObject);
begin
	Self.m__MenuItem_position_modify.Enabled := True;
	Self.m__MenuItem_position_continue.Enabled := False;
	Self.m__DoBreakGame := False;
	Self.m__IsInModifyMode := False;
	Self.m__DrawGrid.Selection := TGridRect(Self.m__DrawGrid.CellRect(-1, -1));
	Self.m__GameHistory.Positions[1].BoardState := Self.m__BoardState;
	Self.m__GameHistory.MoveCount := 1;
	Self.m__GameHistory.CurrentMoveNumber := 1;
	Self.m__BackForwardButtons.Position := Self.m__GameHistory.CurrentMoveNumber;
	RunGame(Self.m__Players);
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
		Self.m__Players[1].GetMove := GetEngineMove;
		Self.m__Players[1].Name := 'CPU';
	end
	else
	begin
		Self.m__Players[1].GetMove := GetPlayerMove;
		Self.m__Players[1].Name := 'human';
	end;
	
	if Self.m__MenuItem__players__2_players.Checked or Self.m__MenuItem__players__cpu_vs_human.Checked then
	begin
		Self.m__Players[2].GetMove := GetPlayerMove;
		Self.m__Players[2].Name := 'human';
	end
	else
	begin
		Self.m__Players[2].GetMove := GetEngineMove;
		Self.m__Players[2].Name := 'CPU';
	end;
	
	Self.m__Player1Label.Caption := Self.m__Players[1].Name;
	Self.m__Player2Label.Caption := Self.m__Players[2].Name;
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
	
	Self.m__Statusbar.Panels[2].Text := Self.m__Player1Piece.Hint +' '+ IntToStr(Self.m__Positions[1].PossibleMoveCount) +' moves  '+
			Self.m__Player2Piece.Hint +' '+ IntToStr(Self.m__Positions[2].PossibleMoveCount) +' moves';
	
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
	
	Self.m__Statusbar.Panels[2].Text := Self.m__Player1Piece.Hint +' '+ IntToStr(Self.m__Positions[1].PossibleMoveCount) +' moves  '+
			Self.m__Player2Piece.Hint +' '+ IntToStr(Self.m__Positions[2].PossibleMoveCount) +' moves';
	
	Self.m__DrawGrid.Repaint;
end;

end.

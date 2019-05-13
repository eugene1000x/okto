
unit Impl;


interface


uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, Menus, Grids, StdCtrls, ExtCtrls, ImgList, ComCtrls, ShellCtrls;

const
	BOARD_DIMENSION = 8;
	PLAYER_TYPE_CPU = Byte(14);
	PLAYER_TYPE_HUMAN = Byte(15);

type
	TIntPlayerNumber = 1..2;
	TIntOptionalPlayerNumber = 0..2;
	TIntCellCoordinate = 1..BOARD_DIMENSION;
	TBoardState = array [TIntCellCoordinate, TIntCellCoordinate] of TIntOptionalPlayerNumber;
	
	TCellAddress = record
		Column, Row: TIntCellCoordinate;
	end;
	
	TPlayer = record
		Name: string;
		GetMove: procedure(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TIntPlayerNumber) of object;
	end;
	
	TIntCellCount = 0 .. BOARD_DIMENSION * BOARD_DIMENSION;
	
	TChildPosition = record
		Move: TCellAddress;
		BoardState: TBoardState;
	end;
	
	TPosition = record
		BoardState: TBoardState;
		i__WhoseTurn: TIntOptionalPlayerNumber;
		PossibleMoveCount: TIntCellCount;
		ChildPositions: array [TIntCellCount] of TChildPosition;
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
		Positions: array [TIntCellCount] of record
			BoardState: TBoardState;
			i__WhoseTurn: TIntPlayerNumber;
		end;
		MoveCount, CurrentMoveNumber: TIntCellCount;
	end;
	
	(**
	 * Interface that GameContext uses to get parameters for the game, to
	 * notify GUI about evaluation start, game progress, etc.
	 *)
	IGameDriver = interface
		procedure OnPositionEvaluationStarted();
		
		(**
		 * Called when engine's evaluation of the current position changed while engine is
		 * searching for the best move.
		 *)
		procedure OnPositionEvaluationChanged();
		
		(**
		 * Called when evaluation of one more move is completed while engine is
		 * searching for the best move.
		 *)
		procedure OnMoveEvaluationCompleted();
		
		procedure OnPositionEvaluationCompleted();
		
		(**
		 * Called when cell has been changed in board modify mode.
		 *)
		procedure OnCellStateChanged();
		
		function GetMidgameMaxDepth(): TIntCellCount;
		function GetEndgameMaxDepth(): TIntCellCount;
	end;

	(**
	 * Class that contains logic related to the game (algorithms, game state, etc.) and that runs the game.
	 *)
	TGameContext = class
		private m__BoardState: TBoardState;
		private m__Positions: array [1..2] of TPosition;
		public m__Players: array [1..2] of TPlayer;
		private m_i__WhoseTurn: Byte;
		private m__MidgameMaxDepth, m__EndgameMaxDepth, m__MaxDepth: TIntCellCount;
		
		public m__Evaluations: record
			Last, Best: TCellAddress;
			CellEvaluations: array [1..8, 1..8] of string;
		end;
		
		private m__BestEngineMoveEvaluation: TEvaluation;
		
		public m__GameHistory: TGameHistory;
		public m__IsInModifyMode, m__DoBreakGame: Boolean;

		private m__GameDriver: IGameDriver;
		

		public constructor Create(); overload;		//disallowing to invoke TObject.Create()
        public constructor Create(GameDriver: IGameDriver); overload;
        
        (**
         * Init (re-read) algorithm parameters for each CPU's move.
         * These may be changed during the game.
         *)
        private procedure InitParameters();

		(**
		 * Current used max evaluation depth.
		 *)
		public function GetMaxDepth(): TIntCellCount;
		
		(**
		 * Number of possible moves for given player in the current position with his pieces.
		 * 
		 * @param PlayerNumber If 0, take player whose turn now is.
		 *)
		public function GetPossibleMoveCount(PlayerNumber: TIntOptionalPlayerNumber = 0): TIntCellCount;
		
		public function GetBestEngineMoveEvaluation(): TEvaluation;
		
		public function GetCellState(Column, Row: Byte): TIntOptionalPlayerNumber;
		public procedure SetCellState(Column, Row: Byte; CellState: TIntOptionalPlayerNumber);
		public procedure FinishBoardModification();
		public procedure GoBack();
		public procedure GoForward();
		
		public procedure GetEngineMove(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TIntPlayerNumber);
		public procedure GetPlayerMove(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TIntPlayerNumber);
		
		(**
		 * Starts new game from standard initial position (with 4 pieces).
		 * 
		 * @param Player1Type Player to make first move. One of PLAYER_TYPE_* constants.
		 * @param Player2Type Player to make second move. One of PLAYER_TYPE_* constants.
		 *)
		public procedure StartNewGame(Player1Type, Player2Type: Byte);
		
		(**
		 * Runs game from current position on the board.
		 *)
		public procedure RunGame();
	end;
	
	TMainWindow = class(TForm, IGameDriver)
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
		
		private m__GameContext: TGameContext;
		
		
		public constructor Create(Owner: TComponent); override;
		public destructor Destroy(); override;
		
		//IGameDriver
		public procedure OnPositionEvaluationStarted();
		public procedure OnPositionEvaluationChanged();
		public procedure OnMoveEvaluationCompleted();
		public procedure OnPositionEvaluationCompleted();
		public procedure OnCellStateChanged();
		public function GetMidgameMaxDepth(): TIntCellCount;
		public function GetEndgameMaxDepth(): TIntCellCount;

		//graphical component events		
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
	end;


implementation
{$R *.dfm}


constructor TGameContext.Create();
begin
	Assert(False, 'TGameContext.Create() inherited from TObject should not be used.');
end;

constructor TGameContext.Create(GameDriver: IGameDriver);
begin
	inherited Create();
	
	Self.m__GameDriver := GameDriver;
end;

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

procedure AnalyzePosition(BoardState: TBoardState; i__WhoseTurn: TIntOptionalPlayerNumber; var Position: TPosition);
var
	Column, Row: TIntCellCoordinate;
	ReversedCount: TIntCellCount;
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
	Column, Row: TIntCellCoordinate;
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

function GetHeuristicalEvaluation(BoardState: TBoardState; PlayerNumber: TIntPlayerNumber): TEvaluation;
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
		Exit();
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

procedure TGameContext.InitParameters();
begin
	Self.m__MidgameMaxDepth := Self.m__GameDriver.GetMidgameMaxDepth();
	Self.m__EndgameMaxDepth := Self.m__GameDriver.GetEndgameMaxDepth();

	if 64 - CountCellsWithState(Self.m__BoardState, 1) - CountCellsWithState(Self.m__BoardState, 2) <= Self.m__EndgameMaxDepth then
		Self.m__MaxDepth := Self.m__EndgameMaxDepth
	else
		Self.m__MaxDepth := Self.m__MidgameMaxDepth;
end;

function TGameContext.GetMaxDepth(): TIntCellCount;
begin
	Result := Self.m__MaxDepth;
end;

function TGameContext.GetPossibleMoveCount(PlayerNumber: TIntOptionalPlayerNumber = 0): TIntCellCount;
begin
	if PlayerNumber = 0 then
		PlayerNumber := Self.m_i__WhoseTurn;
	
	Result := Self.m__Positions[PlayerNumber].PossibleMoveCount;
end;

function TGameContext.GetBestEngineMoveEvaluation(): TEvaluation;
begin
	Result := Self.m__BestEngineMoveEvaluation;
end;

function TGameContext.GetCellState(Column, Row: Byte): TIntOptionalPlayerNumber;
begin
	Result := Self.m__BoardState[Column + 1, Row + 1];
end;

procedure TGameContext.SetCellState(Column, Row: Byte; CellState: TIntOptionalPlayerNumber);
begin
	if not Self.m__IsInModifyMode then
		Exit();
		
	Self.m__BoardState[Column + 1, Row + 1] := CellState;
	
	Self.m__GameDriver.OnCellStateChanged();
end;

procedure TGameContext.FinishBoardModification();
begin
	Self.m__DoBreakGame := False;
	Self.m__IsInModifyMode := False;
	Self.m__GameHistory.Positions[1].BoardState := Self.m__BoardState;
	Self.m__GameHistory.MoveCount := 1;
	Self.m__GameHistory.CurrentMoveNumber := 1;
end;

procedure TGameContext.GoBack();
begin
	if Self.m__GameHistory.CurrentMoveNumber - 1 >= 1 then
		Dec(Self.m__GameHistory.CurrentMoveNumber);
		
	Self.m__BoardState := Self.m__GameHistory.Positions[Self.m__GameHistory.CurrentMoveNumber].BoardState;
end;

procedure TGameContext.GoForward();
begin
	if Self.m__GameHistory.CurrentMoveNumber + 1 <= Self.m__GameHistory.MoveCount then
		Inc(Self.m__GameHistory.CurrentMoveNumber);
		
	Self.m__BoardState := Self.m__GameHistory.Positions[Self.m__GameHistory.CurrentMoveNumber].BoardState;
end;

procedure TGameContext.GetEngineMove(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TIntPlayerNumber);
var
	I, Column, Row, Depth: TIntCellCount;
	Res, MinValue, MaxValue: TEvaluation;
	EnginePosition, ChildPosition: TPosition;
label
	done;

	function Evaluate(Position: TPosition; Alpha, Beta: TEvaluation): TEvaluation;
	var
		I: TIntCellCount;
		ChildPosition: TPosition;
		Max: TEvaluation;
	label
		done;
	begin
		Application.ProcessMessages();		//without this window becomes unresponsive
		Inc(Depth);
		
		if Depth = Self.m__MaxDepth then
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
	Self.InitParameters();
	
	for Column := 1 to 8 do
		for Row := 1 to 8 do
			Self.m__Evaluations.CellEvaluations[Column, Row] := '';
	
	Self.m__GameDriver.OnPositionEvaluationStarted();
	
	Move.Column := 1;
	Move.Row := 1;
	
	Res.IsHeuristical := True;
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
	
	Self.m__BestEngineMoveEvaluation := MinValue;
	
	for I := 1 to EnginePosition.PossibleMoveCount do
	begin
		AnalyzePosition(EnginePosition.ChildPositions[I].BoardState, 3 - PlayerNumber, ChildPosition);
		
		if ChildPosition.PossibleMoveCount > 0 then
			if I < EnginePosition.PossibleMoveCount then
				Res := NegateEvaluation(Evaluate(ChildPosition, NegateEvaluation(MaxValue), NegateEvaluation(Self.m__BestEngineMoveEvaluation)))
			else
				Res := NegateEvaluation(Evaluate(ChildPosition, NegateEvaluation(Self.m__BestEngineMoveEvaluation), NegateEvaluation(Self.m__BestEngineMoveEvaluation)))
		else
		begin
			AnalyzePosition(EnginePosition.ChildPositions[I].BoardState, PlayerNumber, ChildPosition);
			
			if ChildPosition.PossibleMoveCount > 0 then
				if I < EnginePosition.PossibleMoveCount then
					Res := Evaluate(ChildPosition, Self.m__BestEngineMoveEvaluation, MaxValue)
				else
					Res := Evaluate(ChildPosition, Self.m__BestEngineMoveEvaluation, Self.m__BestEngineMoveEvaluation)
			else
				Res := GetHeuristicalEvaluation(EnginePosition.ChildPositions[I].BoardState, PlayerNumber);
		end;
		
		if (not Res.IsLessOrEqual) and (Res.IsGreaterOrEqual or IsGreaterOrEqual(Res, Self.m__BestEngineMoveEvaluation, True)) then
		begin
			Move := EnginePosition.ChildPositions[I].Move;
			Self.m__BestEngineMoveEvaluation := Res;
			Self.m__Evaluations.Best := Move;
			
			Self.m__GameDriver.OnPositionEvaluationChanged();
		end;
		
		Self.m__Evaluations.CellEvaluations[EnginePosition.ChildPositions[I].Move.Column, EnginePosition.ChildPositions[I].Move.Row] := EvaluationToStr_Short(Res);
		
		Self.m__GameDriver.OnMoveEvaluationCompleted();
		
		if IsGreaterOrEqual(Self.m__BestEngineMoveEvaluation, MaxValue, True) then
			goto done;
	end;
	
	done:
	
	Self.m__GameDriver.OnPositionEvaluationCompleted();
end;

function IsLegalMove(Column, Row: TIntCellCoordinate; Position: TPosition; var MoveNumber: Byte): Boolean;
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

procedure TGameContext.GetPlayerMove(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TIntPlayerNumber);
var
	MoveNumberDummy: Byte;
	MainWindow: TMainWindow;
begin
	MainWindow := TMainWindow(Self.m__GameDriver);

	repeat
		Application.ProcessMessages();		//without this window becomes unresponsive
		
		if Self.m__DoBreakGame then
			Exit();
	until IsLegalMove(MainWindow.m__DrawGrid.Col + 1, MainWindow.m__DrawGrid.Row + 1, Self.m__Positions[PlayerNumber], MoveNumberDummy);
	
	Move.Column := MainWindow.m__DrawGrid.Col + 1;
	Move.Row := MainWindow.m__DrawGrid.Row + 1;
end;

procedure TGameContext.StartNewGame(Player1Type, Player2Type: Byte);
begin
	ClearBoard(Self.m__BoardState);
	InitBoard(Self.m__BoardState);
	
	ClearBoard(Self.m__GameHistory.Positions[1].BoardState);
	InitBoard(Self.m__GameHistory.Positions[1].BoardState);
	
	Self.m__GameHistory.MoveCount := 1;
	Self.m__GameHistory.CurrentMoveNumber := 1;
	
	if Player1Type = PLAYER_TYPE_HUMAN then
	begin
		Self.m__Players[1].GetMove := Self.GetPlayerMove;
		Self.m__Players[1].Name := 'human';
	end
	else if Player1Type = PLAYER_TYPE_CPU then
	begin
		Self.m__Players[1].GetMove := Self.GetEngineMove;
		Self.m__Players[1].Name := 'CPU';
	end
	else
		Assert(false, 'Player1Type must be one of PLAYER_TYPE_* constants: '+ IntToStr(Player1Type));
		
	if Player2Type = PLAYER_TYPE_HUMAN then
	begin
		Self.m__Players[2].GetMove := Self.GetPlayerMove;
		Self.m__Players[2].Name := 'human';
	end
	else if Player2Type = PLAYER_TYPE_CPU then
	begin
		Self.m__Players[2].GetMove := Self.GetEngineMove;
		Self.m__Players[2].Name := 'CPU';
	end
	else
		Assert(false, 'Player2Type must be one of PLAYER_TYPE_* constants: '+ IntToStr(Player2Type));
	
	Self.RunGame();
end;

procedure TGameContext.RunGame();
var
	Move: TCellAddress;
	C1, C2: Byte;
	i__WhoseTurn: Byte;
	IsGameEnd: Boolean;
	MainWindow: TMainWindow;
begin
	MainWindow := TMainWindow(Self.m__GameDriver);

	MainWindow.m__MenuItem_position_modify.Enabled := True;
	
	Self.m__Evaluations.Last.Column := 1;
	Self.m__Evaluations.Last.Row := 1;
	
	MainWindow.m__Player1PieceCountEdit.Text := IntToStr(CountCellsWithState(Self.m__BoardState, 1));
	MainWindow.m__Player2PieceCountEdit.Text := IntToStr(CountCellsWithState(Self.m__BoardState, 2));
	
	AnalyzePosition(Self.m__BoardState, 1, Self.m__Positions[1]);
	AnalyzePosition(Self.m__BoardState, 2, Self.m__Positions[2]);
	
	for C1 := 1 to 8 do
		for C2 := 1 to 8 do
			Self.m__Evaluations.CellEvaluations[C1, C2] := '';
			
	MainWindow.m__DrawGrid.Repaint();
	
	MainWindow.m__Statusbar.Panels[2].Text := MainWindow.m__Player1Piece.Hint +' '+ IntToStr(Self.m__Positions[1].PossibleMoveCount) +' moves  '+
			MainWindow.m__Player2Piece.Hint +' '+ IntToStr(Self.m__Positions[2].PossibleMoveCount) +' moves';
		
	IsGameEnd := False;
	i__WhoseTurn := 1;
	
	if Self.m__Positions[1].PossibleMoveCount = 0 then
		if Self.m__Positions[2].PossibleMoveCount = 0 then
			IsGameEnd := True;
		
	while not IsGameEnd do
	begin
		if Self.m__DoBreakGame then
			Exit();
		
		MainWindow.m__DrawGrid.Selection := TGridRect(MainWindow.m__DrawGrid.CellRect(-1, -1));		//TODO: Why whis typecast (in two places)?
		
		AnalyzePosition(Self.m__BoardState, 1, Self.m__Positions[1]);
		AnalyzePosition(Self.m__BoardState, 2, Self.m__Positions[2]);
		
		if Self.m__Positions[i__WhoseTurn].PossibleMoveCount = 0 then
			if Self.m__Positions[3 - i__WhoseTurn].PossibleMoveCount = 0 then
				IsGameEnd := True
			else
			begin
				if Self.m__Players[i__WhoseTurn].Name = 'human' then
					ShowMessage('pass move');
				i__WhoseTurn := 3 - i__WhoseTurn;
			end;
			
		if not IsGameEnd then
		begin
			Self.m__Players[i__WhoseTurn].GetMove(Move, Self.m__BoardState, i__WhoseTurn);
			Self.m_i__WhoseTurn := i__WhoseTurn;
		end;
			
		if IsLegalMove(Move.Column, Move.Row, Self.m__Positions[i__WhoseTurn], C1) then
		begin
			Self.m__BoardState := Self.m__Positions[i__WhoseTurn].ChildPositions[C1].BoardState;
			Self.m__Evaluations.Last := Move;
			Self.m__GameHistory.MoveCount := Self.m__GameHistory.MoveCount + 1;
			Self.m__GameHistory.CurrentMoveNumber := Self.m__GameHistory.CurrentMoveNumber + 1;
			MainWindow.m__BackForwardButtons.Position := Self.m__GameHistory.CurrentMoveNumber;
			Self.m__GameHistory.Positions[Self.m__GameHistory.MoveCount].BoardState := Self.m__BoardState;
			Self.m__GameHistory.Positions[Self.m__GameHistory.MoveCount].i__WhoseTurn := i__WhoseTurn;
			MainWindow.m__DrawGrid.Repaint();
		end;
		
		i__WhoseTurn := 3 - i__WhoseTurn;
		
		AnalyzePosition(Self.m__BoardState, 1, Self.m__Positions[1]);
		AnalyzePosition(Self.m__BoardState, 2, Self.m__Positions[2]);
		
		MainWindow.m__Statusbar.Panels[2].Text := MainWindow.m__Player1Piece.Hint +' '+ IntToStr(Self.m__Positions[1].PossibleMoveCount) +' moves  '+
				MainWindow.m__Player2Piece.Hint +' '+ IntToStr(Self.m__Positions[2].PossibleMoveCount) +' moves';
		
		MainWindow.m__Player1PieceCountEdit.Text := IntToStr(CountCellsWithState(Self.m__BoardState, 1));
		MainWindow.m__Player2PieceCountEdit.Text := IntToStr(CountCellsWithState(Self.m__BoardState, 2));
	end;
	
	C1 := CountCellsWithState(Self.m__BoardState, 1);
	C2 := CountCellsWithState(Self.m__BoardState, 2);
	
	if C1 > C2 then
		ShowMessage('Winner is '+ Self.m__Players[1].name)
	else if C2 > C1 then
		ShowMessage('Winner is '+ Self.m__Players[2].name)
	else
		ShowMessage('Draw');
			
	MainWindow.m__MenuItem_position_modify.Enabled := True;
end;

procedure TMainWindow.OnPositionEvaluationStarted();
var
	PossibleMoveCount, MaxDepth: TIntCellCount;
begin
	PossibleMoveCount := Self.m__GameContext.GetPossibleMoveCount();
	MaxDepth := Self.m__GameContext.GetMaxDepth();
		
	Self.m__Statusbar.Panels[0].Text := 'Thinking...';
	Self.m__Statusbar.Panels[3].Text := 'Depth='+ IntToStr(MaxDepth);
	
	Self.m__Progressbar.Max := PossibleMoveCount;
	Self.m__Progressbar.Position := 0;
	
	Self.m__MenuItem_position_modify.Enabled := False;
	
	Self.m__MidgameDepthUpDown.Enabled := False;
	Self.m__EndgameDepthUpDown.Enabled := False;
	
	Self.m__DrawGrid.Repaint();
end;

procedure TMainWindow.OnPositionEvaluationChanged();
begin
	Self.m__Statusbar.Panels[1].Text := EvaluationToStr_Long(Self.m__GameContext.GetBestEngineMoveEvaluation());
end;

procedure TMainWindow.OnMoveEvaluationCompleted();
begin
	Self.m__DrawGrid.Repaint();
	Self.m__Progressbar.Position := Self.m__Progressbar.Position + 1;
end;

procedure TMainWindow.OnPositionEvaluationCompleted();
begin
	Self.m__Statusbar.Panels[0].Text := 'Ready';
	Self.m__MenuItem_position_modify.Enabled := True;
	
	Self.m__MidgameDepthUpDown.Enabled := True;
	Self.m__EndgameDepthUpDown.Enabled := True;
end;

procedure TMainWindow.OnCellStateChanged();
begin
	Self.m__DrawGrid.Repaint();
end;

function TMainWindow.GetMidgameMaxDepth(): TIntCellCount;
begin
	Result := StrToInt(Self.m__MidgameDepthLabeledEdit.Text);
end;

function TMainWindow.GetEndgameMaxDepth(): TIntCellCount;
begin
	Result := StrToInt(Self.m__EndgameDepthLabeledEdit.Text);
end;

constructor TMainWindow.Create(Owner: TComponent);
begin
	inherited Create(Owner);

    Self.m__GameContext := TGameContext.Create(Self);
end;

destructor TMainWindow.Destroy();
begin
	Self.m__GameContext.Free();
	
	inherited Destroy();
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
	
	Self.m__GameContext.m__DoBreakGame := False;
	Self.m__GameContext.m__IsInModifyMode := False;
	
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
	
	Self.Show();
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
	
	case Self.m__GameContext.GetCellState(Column, Row) of
		1:
			Self.m__DrawGrid.Canvas.StretchDraw(Rect, Self.m__Player1Piece.Picture.Bitmap);
		2:
			Self.m__DrawGrid.Canvas.StretchDraw(Rect, Self.m__Player2Piece.Picture.Bitmap);
	end;
	
	if (Self.m__GameContext.m__Evaluations.Last.Column = Column + 1) and (Self.m__GameContext.m__Evaluations.Last.Row = Row + 1) and (Self.m__GameContext.GetCellState(Column, Row) > 0) then
	begin
		Self.m__DrawGrid.Canvas.Pen.Color := clRed;
		Self.m__DrawGrid.Canvas.MoveTo(Rect.Left, Rect.Top);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Right, Rect.Top);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Right, Rect.Bottom);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Left, Rect.Bottom);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Left, Rect.Top);
	end;
	
	if (Column + 1 = Self.m__GameContext.m__Evaluations.Best.Column) and (Row + 1 = Self.m__GameContext.m__Evaluations.Best.Row) then
		Self.m__DrawGrid.Canvas.Font.Color := clRed
	else
		Self.m__DrawGrid.Canvas.Font.Color := clBlack;
		
	Text := Self.m__GameContext.m__Evaluations.CellEvaluations[Column + 1, Row + 1];
	
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
var
	CurrentCellState, NewCellState: TIntOptionalPlayerNumber;
begin
	CurrentCellState := Self.m__GameContext.GetCellState(Self.m__DrawGrid.Col, Self.m__DrawGrid.Row);
	NewCellState := CurrentCellState;
	
	repeat
		NewCellState := (NewCellState + 1) mod 3;
	until not ((NewCellState = 0) and (Self.m__DrawGrid.Col + 1 in [4, 5]) and (Self.m__DrawGrid.Row + 1 in [4, 5]));
	
	Self.m__GameContext.SetCellState(Self.m__DrawGrid.Col, Self.m__DrawGrid.Row, NewCellState);
end;

procedure TMainWindow.OnClick_BackForwardButtons(Sender: TObject; Button: TUDBtnType);
begin
	if Button = btNext then
		Self.m__GameContext.GoForward();
	
	if Button = btPrev then
		Self.m__GameContext.GoBack();
	
	Self.m__BackForwardButtons.Position := Self.m__GameContext.m__GameHistory.CurrentMoveNumber;
	Self.m__DrawGrid.Repaint();
end;

procedure TMainWindow.OnClick_MenuItem__new_game(Sender: TObject);
var
	Player1Type, Player2Type: Byte;
	Player1Name, Player2Name: string;
begin
	Self.m__BackForwardButtons.Position := 1;
	Self.m__DrawGrid.DefaultDrawing := False;
	
	Self.m__Statusbar.Panels[0].Text := '';
	Self.m__Statusbar.Panels[1].Text := '';
	
	
	if Self.m__MenuItem__players__2_players.Checked then
	begin
		Player1Type := PLAYER_TYPE_HUMAN;
		Player2Type := PLAYER_TYPE_HUMAN;
	end
	else if Self.m__MenuItem__players__human_vs_cpu.Checked then
	begin
		Player1Type := PLAYER_TYPE_HUMAN;
		Player2Type := PLAYER_TYPE_CPU;
	end
	else if Self.m__MenuItem__players__cpu_vs_human.Checked then
	begin
		Player1Type := PLAYER_TYPE_CPU;
		Player2Type := PLAYER_TYPE_HUMAN;
	end
	else if Self.m__MenuItem__players__cpu_vs_cpu.Checked then
	begin
		Player1Type := PLAYER_TYPE_CPU;
		Player2Type := PLAYER_TYPE_CPU;
	end
	else
		Assert(false, 'None of menu items is checked');
	
	if Player1Type = PLAYER_TYPE_HUMAN then
		Player1Name := 'human'
	else if Player1Type = PLAYER_TYPE_CPU then
		Player1Name := 'CPU'
	else
		Assert(false, 'Player1Type must be one of PLAYER_TYPE_* constants: '+ IntToStr(Player1Type));
		
	if Player2Type = PLAYER_TYPE_HUMAN then
		Player2Name := 'human'
	else if Player2Type = PLAYER_TYPE_CPU then
		Player2Name := 'CPU'
	else
		Assert(false, 'Player2Type must be one of PLAYER_TYPE_* constants: '+ IntToStr(Player2Type));
	
	Self.m__Player1Label.Caption := Player1Name;//Self.m__GameContext.m__Players[1].Name;
	Self.m__Player2Label.Caption := Player2Name;//Self.m__GameContext.m__Players[2].Name;
	
	Self.m__GameContext.StartNewGame(Player1Type, Player2Type);

	Self.m__DrawGrid.Repaint();
end;

procedure TMainWindow.OnClick_MenuItem_position_modify(Sender: TObject);
begin
	Self.m__MenuItem_position_modify.Enabled := False;
	Self.m__MenuItem_position_continue.Enabled := True;
	Self.m__GameContext.m__DoBreakGame := True;
	Self.m__GameContext.m__IsInModifyMode := True;
end;

procedure TMainWindow.OnClick_MenuItem_position_continue(Sender: TObject);
begin
	Self.m__MenuItem_position_modify.Enabled := True;
	Self.m__MenuItem_position_continue.Enabled := False;
	Self.m__DrawGrid.Selection := TGridRect(Self.m__DrawGrid.CellRect(-1, -1));		//TODO: Why whis typecast (in two places)?
	
	Self.m__GameContext.FinishBoardModification();
	
	Self.m__BackForwardButtons.Position := Self.m__GameContext.m__GameHistory.CurrentMoveNumber;
	Self.m__GameContext.RunGame();
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
		Self.m__GameContext.m__Players[1].GetMove := Self.m__GameContext.GetEngineMove;
		Self.m__GameContext.m__Players[1].Name := 'CPU';
	end
	else
	begin
		Self.m__GameContext.m__Players[1].GetMove := Self.m__GameContext.GetPlayerMove;
		Self.m__GameContext.m__Players[1].Name := 'human';
	end;
	
	if Self.m__MenuItem__players__2_players.Checked or Self.m__MenuItem__players__cpu_vs_human.Checked then
	begin
		Self.m__GameContext.m__Players[2].GetMove := Self.m__GameContext.GetPlayerMove;
		Self.m__GameContext.m__Players[2].Name := 'human';
	end
	else
	begin
		Self.m__GameContext.m__Players[2].GetMove := Self.m__GameContext.GetEngineMove;
		Self.m__GameContext.m__Players[2].Name := 'CPU';
	end;
	
	Self.m__Player1Label.Caption := Self.m__GameContext.m__Players[1].Name;
	Self.m__Player2Label.Caption := Self.m__GameContext.m__Players[2].Name;
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
	
	Self.m__Statusbar.Panels[2].Text := Self.m__Player1Piece.Hint +' '+ IntToStr(Self.m__GameContext.GetPossibleMoveCount(1)) +' moves  '+
			Self.m__Player2Piece.Hint +' '+ IntToStr(Self.m__GameContext.GetPossibleMoveCount(2)) +' moves';
	
	Self.m__DrawGrid.Repaint();
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
	
	Self.m__Statusbar.Panels[2].Text := Self.m__Player1Piece.Hint +' '+ IntToStr(Self.m__GameContext.GetPossibleMoveCount(1)) +' moves  '+
			Self.m__Player2Piece.Hint +' '+ IntToStr(Self.m__GameContext.GetPossibleMoveCount(2)) +' moves';
	
	Self.m__DrawGrid.Repaint();
end;

end.

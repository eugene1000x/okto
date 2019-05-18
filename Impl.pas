
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
	(**
	 * 1 is the player who makes first move in the game,
	 * 2 is the player who makes second move in the game.
	 *)
	TIntPlayerNumber = 1..2;
	TIntOptionalPlayerNumber = 0..2;
	
	TIntCellCoordinate = 1..BOARD_DIMENSION;
	TIntCellCount = 0 .. BOARD_DIMENSION * BOARD_DIMENSION;
	
	TBoardState = array [TIntCellCoordinate, TIntCellCoordinate] of TIntOptionalPlayerNumber;
	
	TCellAddress = record
		Column, Row: TIntCellCoordinate;
	end;
	
	TPlayer = record
		Name: string;
		fn__GetMove: procedure(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TIntPlayerNumber) of object;
		PlayerType: Byte;		//one of PLAYER_TYPE_* constants
	end;
	
	TAnalyzedPosition = record
		i__WhoseTurn: TIntOptionalPlayerNumber;
		PossibleMoveCount: TIntCellCount;
		ChildPositions: array [TIntCellCount] of record
			Move: TCellAddress;
			BoardState: TBoardState;
		end;
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
		procedure OnGameStarted();
		procedure BeforeMove();
		procedure OnMoveMade();
		procedure OnGameEnded();
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
		function GetPlayerMoveCell(): TCellAddress;
	end;

	(**
	 * Class that contains logic related to the game (algorithms, game state, etc.) and that runs the game.
	 *)
	TGameContext = class
		private m__BoardState: TBoardState;
		private m__AnalyzedPositions: array [TIntPlayerNumber] of TAnalyzedPosition;
		private m__Players: array [TIntPlayerNumber] of TPlayer;
		private m_i__WhoseTurn: Byte;
		private m__MidgameMaxDepth, m__EndgameMaxDepth, m__MaxDepth: TIntCellCount;
		
		private m__Evaluations: record
			Best: TCellAddress;
			CellEvaluations: array [1..8, 1..8] of string;
		end;
		
		private m__BestEngineMoveEvaluation: TEvaluation;
		private m__LastMadeMove: TCellAddress;
		
		private m__GameHistory: TGameHistory;
		private m__IsInModifyMode, m__DoBreakGame: Boolean;

		private m__GameDriver: IGameDriver;
		

		public constructor Create(); overload;		//disallowing to invoke TObject.Create()
		public constructor Create(GameDriver: IGameDriver); overload;
		
		private procedure ClearBoard(var BoardState: TBoardState);
		private procedure InitBoard(var BoardState: TBoardState);
		private procedure AnalyzePosition(BoardState: TBoardState; i__WhoseTurn: TIntOptionalPlayerNumber; var AnalyzedPosition: TAnalyzedPosition);
		
		(**
		 * Version without BoardState param takes the current position.
		 *)
		public function CountCellsWithState(CellState: TIntOptionalPlayerNumber): TIntCellCount; overload;
		private function CountCellsWithState(BoardState: TBoardState; CellState: TIntOptionalPlayerNumber): TIntCellCount; overload;
		
		private function IsMidgame(BoardState: TBoardState): Boolean;
		private function GetHeuristicalEvaluation(BoardState: TBoardState; PlayerNumber: TIntPlayerNumber): TEvaluation;
		private function IsEvaluationGreaterOrEqual(Evaluation1, Evaluation2: TEvaluation; Equal: Boolean): Boolean;
		private function EvaluationToStr_Short(Evaluation: TEvaluation): string;
		private function NegateEvaluation(Evaluation: TEvaluation): TEvaluation;
		
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
		
		(**
		 * Returns last made move in the game.
		 *)
		public function GetLastMove(): TCellAddress;
		
		public function GetBestMoveFoundTillNow(): TCellAddress;
		
		public function s__GetMoveEvaluation(Column, Row: Byte): string;
		public function GetCellState(Column, Row: Byte): TIntOptionalPlayerNumber;
		public procedure SetCellState(Column, Row: Byte; CellState: TIntOptionalPlayerNumber);
		public procedure StartBoardModification();
		public procedure FinishBoardModification();
		public procedure GoBack();
		public procedure GoForward();
		public function GetCurrentMoveNumberInHistory(): TIntCellCount;
		
		private procedure GetEngineMove(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TIntPlayerNumber);
		private function IsLegalMove(Column, Row: TIntCellCoordinate; AnalyzedPosition: TAnalyzedPosition; var MoveNumber: Byte): Boolean;
		private procedure GetPlayerMove(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TIntPlayerNumber);
		
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
		
		public procedure SetPlayers(Player1Type, Player2Type: Byte);
		public function GetPlayerName(PlayerNumber: TIntPlayerNumber): string;
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
		public procedure OnGameStarted();
		public procedure BeforeMove();
		public procedure OnMoveMade();
		public procedure OnGameEnded();
		public procedure OnPositionEvaluationStarted();
		public procedure OnPositionEvaluationChanged();
		public procedure OnMoveEvaluationCompleted();
		public procedure OnPositionEvaluationCompleted();
		public procedure OnCellStateChanged();
		public function GetMidgameMaxDepth(): TIntCellCount;
		public function GetEndgameMaxDepth(): TIntCellCount;
		public function GetPlayerMoveCell(): TCellAddress;
		
		private procedure UpdatePlayers(var Player1Type, Player2Type: Byte);
		private function EvaluationToStr_Long(Evaluation: TEvaluation): string;

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

procedure TGameContext.ClearBoard(var BoardState: TBoardState);
var
	Column, Row: Byte;
begin
	for Column := 1 to 8 do
		for Row := 1 to 8 do
			BoardState[Column, Row] := 0;
end;

procedure TGameContext.InitBoard(var BoardState: TBoardState);
begin
	BoardState[4, 4] := 2;
	BoardState[5, 4] := 1;
	BoardState[4, 5] := 1;
	BoardState[5, 5] := 2;
end;

procedure TGameContext.AnalyzePosition(BoardState: TBoardState; i__WhoseTurn: TIntOptionalPlayerNumber; var AnalyzedPosition: TAnalyzedPosition);
var
	Column, Row: TIntCellCoordinate;
	ReversedCount: TIntCellCount;
	K, L: Byte;
	Vx, Vy: -1..1;
begin
	AnalyzedPosition.PossibleMoveCount := 0;
	AnalyzedPosition.i__WhoseTurn := 0;
	
	for Column := 1 to 8 do
		for Row := 1 to 8 do
		begin
			ReversedCount := 0;
			AnalyzedPosition.ChildPositions[AnalyzedPosition.PossibleMoveCount + 1].BoardState := BoardState;
			
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
									AnalyzedPosition.ChildPositions[AnalyzedPosition.PossibleMoveCount + 1].BoardState[Column + L * Vx, Row + L * Vy] := i__WhoseTurn;
									
								Inc(ReversedCount, K - 1);
							end;
						end;
			if ReversedCount > 0 then
			begin
				Inc(AnalyzedPosition.PossibleMoveCount);
				AnalyzedPosition.ChildPositions[AnalyzedPosition.PossibleMoveCount].Move.Column := Column;
				AnalyzedPosition.ChildPositions[AnalyzedPosition.PossibleMoveCount].Move.Row := Row;
				AnalyzedPosition.i__WhoseTurn := i__WhoseTurn;
			end;
		end;
end;

function TGameContext.CountCellsWithState(CellState: TIntOptionalPlayerNumber): TIntCellCount;
begin
	Result := Self.CountCellsWithState(Self.m__BoardState, CellState);
end;

function TGameContext.CountCellsWithState(BoardState: TBoardState; CellState: TIntOptionalPlayerNumber): TIntCellCount;
var
	Column, Row: TIntCellCoordinate;
begin
	Result := 0;
	for Column := 1 to BOARD_DIMENSION do
		for Row := 1 to BOARD_DIMENSION do
			if BoardState[Column, Row] = CellState then
				Inc(Result);
end;

function TGameContext.IsMidgame(BoardState: TBoardState): Boolean;
begin
	Result := Self.CountCellsWithState(BoardState, 1) + Self.CountCellsWithState(BoardState, 2) < 56;
end;

function TGameContext.GetHeuristicalEvaluation(BoardState: TBoardState; PlayerNumber: TIntPlayerNumber): TEvaluation;
const
	G: array [1..4] of array [1..2] of 1..8 = ((1, 1), (1, 8), (8, 1), (8, 8));
	K: array [1..4] of array [1..2] of 1..8 = ((2, 2), (2, 7), (7, 2), (7, 7));
var
	I: Byte;
	AnalyzedPositions: array [TIntPlayerNumber] of TAnalyzedPosition;
begin
	Result.PieceCount := 0;
	Result.IsHeuristical := True;
	Result.IsLessOrEqual := False;
	Result.IsGreaterOrEqual := False;
	
	Self.AnalyzePosition(BoardState, PlayerNumber, AnalyzedPositions[PlayerNumber]);
	Self.AnalyzePosition(BoardState, 3 - PlayerNumber, AnalyzedPositions[3 - PlayerNumber]);
	
	if (AnalyzedPositions[PlayerNumber].PossibleMoveCount = 0) and (AnalyzedPositions[3 - PlayerNumber].PossibleMoveCount = 0) then
	begin
		Result.IsHeuristical := False;
		Result.PieceCount := Self.CountCellsWithState(BoardState, PlayerNumber) - Self.CountCellsWithState(BoardState, 3 - PlayerNumber);
		Exit();
	end;
	
	if Self.IsMidgame(BoardState) then
		Result.PieceCount := Self.CountCellsWithState(BoardState, 3 - PlayerNumber) - Self.CountCellsWithState(BoardState, PlayerNumber)
	else
		Result.PieceCount := Self.CountCellsWithState(BoardState, PlayerNumber) - Self.CountCellsWithState(BoardState, 3 - PlayerNumber);
	
	Result.PieceCount := Result.PieceCount + AnalyzedPositions[PlayerNumber].PossibleMoveCount;
	Result.PieceCount := Result.PieceCount - AnalyzedPositions[3 - PlayerNumber].PossibleMoveCount;
	
	for I := 1 to 4 do
		if (BoardState[G[I][1], G[I][2]] = PlayerNumber) or ((BoardState[K[I][1], K[I][2]] = 3 - PlayerNumber) and (BoardState[G[I][1], G[I][2]] = 0)) then
			Result.PieceCount := Result.PieceCount + 100
		else if (BoardState[G[I][1], G[I][2]] = 3 - PlayerNumber) or ((BoardState[K[I][1], K[I][2]] = PlayerNumber) and (BoardState[G[I][1], G[I][2]] = 0)) then
			Result.PieceCount := Result.PieceCount - 100;
end;

function TGameContext.IsEvaluationGreaterOrEqual(Evaluation1, Evaluation2: TEvaluation; Equal: Boolean): Boolean;
begin
	if Evaluation1.IsHeuristical = Evaluation2.IsHeuristical then
		Result := (Evaluation1.PieceCount > Evaluation2.PieceCount) or ((Evaluation1.PieceCount = Evaluation2.PieceCount) and (Equal or ((Evaluation1.IsGreaterOrEqual) and (Evaluation2.IsGreaterOrEqual = False))))
	else if not Evaluation1.IsHeuristical then
		Result := Evaluation1.PieceCount >= 0
	else
		Result := Evaluation2.PieceCount < 0;
end;

function TMainWindow.EvaluationToStr_Long(Evaluation: TEvaluation): string;
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

function TGameContext.EvaluationToStr_Short(Evaluation: TEvaluation): string;
begin
	Result := IntToStr(Evaluation.PieceCount);
	
	if not Evaluation.IsHeuristical then
		Result := Result +'p';
		
	if Evaluation.IsLessOrEqual then
		Result := '<='+ Result
	else if Evaluation.IsGreaterOrEqual then
		Result := '>='+ Result;
end;

function TGameContext.NegateEvaluation(Evaluation: TEvaluation): TEvaluation;
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

	if 64 - Self.CountCellsWithState(1) - Self.CountCellsWithState(2) <= Self.m__EndgameMaxDepth then
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
	
	Result := Self.m__AnalyzedPositions[PlayerNumber].PossibleMoveCount;
end;

function TGameContext.GetBestEngineMoveEvaluation(): TEvaluation;
begin
	Result := Self.m__BestEngineMoveEvaluation;
end;

function TGameContext.GetLastMove(): TCellAddress;
begin
	Result := Self.m__LastMadeMove;
end;

function TGameContext.GetBestMoveFoundTillNow(): TCellAddress;
begin
	Result := Self.m__Evaluations.Best;
end;

function TGameContext.s__GetMoveEvaluation(Column, Row: Byte): string;
begin
	Result := Self.m__Evaluations.CellEvaluations[Column + 1, Row + 1];
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

procedure TGameContext.StartBoardModification();
begin
	Self.m__DoBreakGame := True;
	Self.m__IsInModifyMode := True;
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

function TGameContext.GetCurrentMoveNumberInHistory(): TIntCellCount;
begin
	Result := Self.m__GameHistory.CurrentMoveNumber;
end;

procedure TGameContext.GetEngineMove(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TIntPlayerNumber);
var
	I, Column, Row, Depth: TIntCellCount;
	Res, MinValue, MaxValue: TEvaluation;
	AnalyzedEnginePosition, AnalyzedChildPosition: TAnalyzedPosition;
label
	done;

	function Evaluate(BoardState: TBoardState; AnalyzedPosition: TAnalyzedPosition; Alpha, Beta: TEvaluation): TEvaluation;
	var
		I: TIntCellCount;
		AnalyzedChildPosition: TAnalyzedPosition;
		Max: TEvaluation;
	label
		done;
	begin
		Application.ProcessMessages();		//without this window becomes unresponsive
		Inc(Depth);
		
		if Depth = Self.m__MaxDepth then
		begin
			Max := Self.GetHeuristicalEvaluation(BoardState, AnalyzedPosition.i__WhoseTurn);
			goto done;
		end;
		
		Max := MinValue;
		
		for I := 1 to AnalyzedPosition.PossibleMoveCount do
		begin
			Self.AnalyzePosition(AnalyzedPosition.ChildPositions[I].BoardState, 3 - AnalyzedPosition.i__WhoseTurn, AnalyzedChildPosition);
		
			if AnalyzedChildPosition.PossibleMoveCount > 0 then
				Result := Self.NegateEvaluation(Evaluate(AnalyzedPosition.ChildPositions[I].BoardState, AnalyzedChildPosition, Self.NegateEvaluation(Beta), Self.NegateEvaluation(Alpha)))		// (Alpha >= Max) => (-Alpha <= -Max) => (Beta <= -Max)
			else
			begin
				Self.AnalyzePosition(AnalyzedPosition.ChildPositions[I].BoardState, AnalyzedPosition.i__WhoseTurn, AnalyzedChildPosition);
				
				if AnalyzedChildPosition.PossibleMoveCount > 0 then
					Result := Evaluate(AnalyzedPosition.ChildPositions[I].BoardState, AnalyzedChildPosition, Alpha, Beta)
				else
					Result := Self.GetHeuristicalEvaluation(AnalyzedPosition.ChildPositions[I].BoardState, AnalyzedPosition.i__WhoseTurn);
			end;
			
			if Self.IsEvaluationGreaterOrEqual(Max, Alpha, True) then
				Alpha := Max;
			
			if Self.IsEvaluationGreaterOrEqual(Result, Max, False) then
			begin
				Max := Result;
				
				if (not Max.IsLessOrEqual) and (Max.IsGreaterOrEqual or Self.IsEvaluationGreaterOrEqual(Max, Beta, True)) then
				begin
					if AnalyzedPosition.PossibleMoveCount > 1 then
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
		
		if Self.IsEvaluationGreaterOrEqual(Result, MaxValue, True) then
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
	
	Self.AnalyzePosition(BoardState, PlayerNumber, AnalyzedEnginePosition);
	
	if AnalyzedEnginePosition.PossibleMoveCount = 1 then
	begin
		Move := AnalyzedEnginePosition.ChildPositions[1].Move;
		goto done;
	end;
	
	MinValue.IsHeuristical := False;
	MinValue.IsLessOrEqual := False;
	MinValue.IsGreaterOrEqual := False;
	MinValue.PieceCount := -(BOARD_DIMENSION * BOARD_DIMENSION);
	
	MaxValue.IsHeuristical := False;
	MaxValue.IsLessOrEqual := False;
	MaxValue.IsGreaterOrEqual := False;
	MaxValue.PieceCount := BOARD_DIMENSION * BOARD_DIMENSION;
	
	Self.m__BestEngineMoveEvaluation := MinValue;
	
	for I := 1 to AnalyzedEnginePosition.PossibleMoveCount do
	begin
		Self.AnalyzePosition(AnalyzedEnginePosition.ChildPositions[I].BoardState, 3 - PlayerNumber, AnalyzedChildPosition);
		
		if AnalyzedChildPosition.PossibleMoveCount > 0 then
			if I < AnalyzedEnginePosition.PossibleMoveCount then
				Res := Self.NegateEvaluation(Evaluate(AnalyzedEnginePosition.ChildPositions[I].BoardState, AnalyzedChildPosition, Self.NegateEvaluation(MaxValue), Self.NegateEvaluation(Self.m__BestEngineMoveEvaluation)))
			else
				Res := Self.NegateEvaluation(Evaluate(AnalyzedEnginePosition.ChildPositions[I].BoardState, AnalyzedChildPosition, Self.NegateEvaluation(Self.m__BestEngineMoveEvaluation), Self.NegateEvaluation(Self.m__BestEngineMoveEvaluation)))
		else
		begin
			Self.AnalyzePosition(AnalyzedEnginePosition.ChildPositions[I].BoardState, PlayerNumber, AnalyzedChildPosition);
			
			if AnalyzedChildPosition.PossibleMoveCount > 0 then
				if I < AnalyzedEnginePosition.PossibleMoveCount then
					Res := Evaluate(AnalyzedEnginePosition.ChildPositions[I].BoardState, AnalyzedChildPosition, Self.m__BestEngineMoveEvaluation, MaxValue)
				else
					Res := Evaluate(AnalyzedEnginePosition.ChildPositions[I].BoardState, AnalyzedChildPosition, Self.m__BestEngineMoveEvaluation, Self.m__BestEngineMoveEvaluation)
			else
				Res := Self.GetHeuristicalEvaluation(AnalyzedEnginePosition.ChildPositions[I].BoardState, PlayerNumber);
		end;
		
		if (not Res.IsLessOrEqual) and (Res.IsGreaterOrEqual or Self.IsEvaluationGreaterOrEqual(Res, Self.m__BestEngineMoveEvaluation, True)) then
		begin
			Move := AnalyzedEnginePosition.ChildPositions[I].Move;
			Self.m__BestEngineMoveEvaluation := Res;
			Self.m__Evaluations.Best := Move;
			
			Self.m__GameDriver.OnPositionEvaluationChanged();
		end;
		
		Self.m__Evaluations.CellEvaluations[AnalyzedEnginePosition.ChildPositions[I].Move.Column, AnalyzedEnginePosition.ChildPositions[I].Move.Row] := Self.EvaluationToStr_Short(Res);
		
		Self.m__GameDriver.OnMoveEvaluationCompleted();
		
		if Self.IsEvaluationGreaterOrEqual(Self.m__BestEngineMoveEvaluation, MaxValue, True) then
			goto done;
	end;
	
	done:
	
	Self.m__GameDriver.OnPositionEvaluationCompleted();
end;

function TGameContext.IsLegalMove(Column, Row: TIntCellCoordinate; AnalyzedPosition: TAnalyzedPosition; var MoveNumber: Byte): Boolean;
var
	I: Byte;
begin
	Result := False;
	
	for I := 1 to AnalyzedPosition.PossibleMoveCount do
		if (AnalyzedPosition.ChildPositions[I].Move.Column = Column) and (AnalyzedPosition.ChildPositions[I].Move.Row = Row) then
		begin
			Result := True;
			MoveNumber := I;
			Break;
		end;
end;

procedure TGameContext.GetPlayerMove(var Move: TCellAddress; BoardState: TBoardState; PlayerNumber: TIntPlayerNumber);
var
	MoveNumberDummy: Byte;
begin
	repeat
		Application.ProcessMessages();		//without this window becomes unresponsive
		
		if Self.m__DoBreakGame then
			Exit();
	
		Move := Self.m__GameDriver.GetPlayerMoveCell();
	until Self.IsLegalMove(Move.Column, Move.Row, Self.m__AnalyzedPositions[PlayerNumber], MoveNumberDummy);
end;

procedure TGameContext.StartNewGame(Player1Type, Player2Type: Byte);
begin
	Self.ClearBoard(Self.m__BoardState);
	Self.InitBoard(Self.m__BoardState);
	
	Self.ClearBoard(Self.m__GameHistory.Positions[1].BoardState);
	Self.InitBoard(Self.m__GameHistory.Positions[1].BoardState);
	
	Self.m__GameHistory.MoveCount := 1;
	Self.m__GameHistory.CurrentMoveNumber := 1;
	
	Self.SetPlayers(Player1Type, Player2Type);
	Self.RunGame();
end;

procedure TGameContext.RunGame();
var
	Move: TCellAddress;
	C1, C2: Byte;
	i__WhoseTurn: Byte;
	IsGameEnd: Boolean;
begin
	Self.m__LastMadeMove.Column := 1;
	Self.m__LastMadeMove.Row := 1;
	
	Self.AnalyzePosition(Self.m__BoardState, 1, Self.m__AnalyzedPositions[1]);
	Self.AnalyzePosition(Self.m__BoardState, 2, Self.m__AnalyzedPositions[2]);
	
	for C1 := 1 to 8 do
		for C2 := 1 to 8 do
			Self.m__Evaluations.CellEvaluations[C1, C2] := '';
			
	Self.m__GameDriver.OnGameStarted();
	
	IsGameEnd := False;
	i__WhoseTurn := 1;
	
	if Self.m__AnalyzedPositions[1].PossibleMoveCount = 0 then
		if Self.m__AnalyzedPositions[2].PossibleMoveCount = 0 then
			IsGameEnd := True;
		
	while not IsGameEnd do
	begin
		if Self.m__DoBreakGame then
			Exit();
		
		Self.m__GameDriver.BeforeMove();
		
		Self.AnalyzePosition(Self.m__BoardState, 1, Self.m__AnalyzedPositions[1]);
		Self.AnalyzePosition(Self.m__BoardState, 2, Self.m__AnalyzedPositions[2]);
		
		if Self.m__AnalyzedPositions[i__WhoseTurn].PossibleMoveCount = 0 then
			if Self.m__AnalyzedPositions[3 - i__WhoseTurn].PossibleMoveCount = 0 then
				IsGameEnd := True
			else
			begin
				if Self.m__Players[i__WhoseTurn].PlayerType = PLAYER_TYPE_HUMAN then
					ShowMessage('pass move');
				i__WhoseTurn := 3 - i__WhoseTurn;
			end;
			
		if not IsGameEnd then
		begin
			Self.m__Players[i__WhoseTurn].fn__GetMove(Move, Self.m__BoardState, i__WhoseTurn);
			Self.m_i__WhoseTurn := i__WhoseTurn;
		end;
			
		if Self.IsLegalMove(Move.Column, Move.Row, Self.m__AnalyzedPositions[i__WhoseTurn], C1) then
		begin
			Self.m__BoardState := Self.m__AnalyzedPositions[i__WhoseTurn].ChildPositions[C1].BoardState;
			Self.m__LastMadeMove := Move;
			Self.m__GameHistory.MoveCount := Self.m__GameHistory.MoveCount + 1;
			Self.m__GameHistory.CurrentMoveNumber := Self.m__GameHistory.CurrentMoveNumber + 1;
			Self.m__GameHistory.Positions[Self.m__GameHistory.MoveCount].BoardState := Self.m__BoardState;
			Self.m__GameHistory.Positions[Self.m__GameHistory.MoveCount].i__WhoseTurn := i__WhoseTurn;
		end;
		
		i__WhoseTurn := 3 - i__WhoseTurn;
		
		Self.AnalyzePosition(Self.m__BoardState, 1, Self.m__AnalyzedPositions[1]);
		Self.AnalyzePosition(Self.m__BoardState, 2, Self.m__AnalyzedPositions[2]);
		
		Self.m__GameDriver.OnMoveMade();
	end;
	
	C1 := Self.CountCellsWithState(1);
	C2 := Self.CountCellsWithState(2);
	
	if C1 > C2 then
		ShowMessage('Winner is '+ Self.m__Players[1].name)
	else if C2 > C1 then
		ShowMessage('Winner is '+ Self.m__Players[2].name)
	else
		ShowMessage('Draw');
			
	Self.m__GameDriver.OnGameEnded();
end;

procedure TGameContext.SetPlayers(Player1Type, Player2Type: Byte);
begin
	Self.m__Players[1].PlayerType := Player1Type;
	Self.m__Players[2].PlayerType := Player2Type;

	if Player1Type = PLAYER_TYPE_CPU then
	begin
		Self.m__Players[1].fn__GetMove := Self.GetEngineMove;
		Self.m__Players[1].Name := 'CPU';
	end
	else if Player1Type = PLAYER_TYPE_HUMAN then
	begin
		Self.m__Players[1].fn__GetMove := Self.GetPlayerMove;
		Self.m__Players[1].Name := 'human';
	end
	else
		Assert(False, 'Player1Type must be one of PLAYER_TYPE_* constants: '+ IntToStr(Player1Type));
	
	if Player2Type = PLAYER_TYPE_CPU then
	begin
		Self.m__Players[2].fn__GetMove := Self.GetEngineMove;
		Self.m__Players[2].Name := 'CPU';
	end
	else if Player2Type = PLAYER_TYPE_HUMAN then
	begin
		Self.m__Players[2].fn__GetMove := Self.GetPlayerMove;
		Self.m__Players[2].Name := 'human';
	end
	else
		Assert(False, 'Player2Type must be one of PLAYER_TYPE_* constants: '+ IntToStr(Player2Type));
end;

function TGameContext.GetPlayerName(PlayerNumber: TIntPlayerNumber): string;
begin
	Result := Self.m__Players[PlayerNumber].Name;
end;

procedure TMainWindow.OnGameStarted();
begin
	Self.m__MenuItem_position_modify.Enabled := True;
	
	Self.m__Player1PieceCountEdit.Text := IntToStr(Self.m__GameContext.CountCellsWithState(1));
	Self.m__Player2PieceCountEdit.Text := IntToStr(Self.m__GameContext.CountCellsWithState(2));
	
	Self.m__DrawGrid.Repaint();
	
	Self.m__Statusbar.Panels[2].Text := Self.m__Player1Piece.Hint +' '+ IntToStr(Self.m__GameContext.GetPossibleMoveCount(1)) +' moves  '+
			Self.m__Player2Piece.Hint +' '+ IntToStr(Self.m__GameContext.GetPossibleMoveCount(2)) +' moves';
end;

procedure TMainWindow.BeforeMove();
begin
	Self.m__DrawGrid.Selection := TGridRect(Self.m__DrawGrid.CellRect(-1, -1));		//TODO: Why whis typecast (in two places)?
end;

procedure TMainWindow.OnMoveMade();
begin
	Self.m__BackForwardButtons.Position := Self.m__GameContext.GetCurrentMoveNumberInHistory();
	Self.m__DrawGrid.Repaint();
		
	Self.m__Statusbar.Panels[2].Text := Self.m__Player1Piece.Hint +' '+ IntToStr(Self.m__GameContext.GetPossibleMoveCount(1)) +' moves  '+
			Self.m__Player2Piece.Hint +' '+ IntToStr(Self.m__GameContext.GetPossibleMoveCount(2)) +' moves';
	
	Self.m__Player1PieceCountEdit.Text := IntToStr(Self.m__GameContext.CountCellsWithState(1));
	Self.m__Player2PieceCountEdit.Text := IntToStr(Self.m__GameContext.CountCellsWithState(2));
end;

procedure TMainWindow.OnGameEnded();
begin
	Self.m__MenuItem_position_modify.Enabled := True;
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
	Self.m__Statusbar.Panels[1].Text := Self.EvaluationToStr_Long(Self.m__GameContext.GetBestEngineMoveEvaluation());
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

function TMainWindow.GetPlayerMoveCell(): TCellAddress;
begin
	Result.Column := Self.m__DrawGrid.Col + 1;
	Result.Row := Self.m__DrawGrid.Row + 1;
end;

procedure TMainWindow.UpdatePlayers(var Player1Type, Player2Type: Byte);
begin
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
	
	Self.m__GameContext.SetPlayers(Player1Type, Player2Type);
	
	Self.m__Player1Label.Caption := Self.m__GameContext.GetPlayerName(1);
	Self.m__Player2Label.Caption := Self.m__GameContext.GetPlayerName(2);
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
	s__Evaluation: string;
	LastMove, BestMove: TCellAddress;
begin
	Self.m__DrawGrid.Canvas.Brush.Color := clWhite;
	Self.m__DrawGrid.Canvas.FillRect(Rect);
	
	case Self.m__GameContext.GetCellState(Column, Row) of
		1:
			Self.m__DrawGrid.Canvas.StretchDraw(Rect, Self.m__Player1Piece.Picture.Bitmap);
		2:
			Self.m__DrawGrid.Canvas.StretchDraw(Rect, Self.m__Player2Piece.Picture.Bitmap);
	end;
	
	
	LastMove := Self.m__GameContext.GetLastMove();
	
	if (LastMove.Column = Column + 1) and (LastMove.Row = Row + 1) and (Self.m__GameContext.GetCellState(Column, Row) > 0) then
	begin
		Self.m__DrawGrid.Canvas.Pen.Color := clRed;
		Self.m__DrawGrid.Canvas.MoveTo(Rect.Left, Rect.Top);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Right, Rect.Top);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Right, Rect.Bottom);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Left, Rect.Bottom);
		Self.m__DrawGrid.Canvas.LineTo(Rect.Left, Rect.Top);
	end;
	
	
	BestMove := Self.m__GameContext.GetBestMoveFoundTillNow();
	
	if (Column + 1 = BestMove.Column) and (Row + 1 = BestMove.Row) then
		Self.m__DrawGrid.Canvas.Font.Color := clRed
	else
		Self.m__DrawGrid.Canvas.Font.Color := clBlack;
		
		
	s__Evaluation := Self.m__GameContext.s__GetMoveEvaluation(Column, Row);
	
	if Copy(s__Evaluation, 1, 2) = '<=' then
		with Self.m__DrawGrid.Canvas do
		begin
			Delete(s__Evaluation, 1, 2);
			Pen.Color := clBlack;
			Draw(Rect.Left, Rect.Top, Self.m__LessOrEqual.Picture.Graphic);
			TextOut(Rect.Left + 8, Rect.Top, s__Evaluation);
		end
	else if Copy(s__Evaluation, 1, 2) = '>=' then
		with Self.m__DrawGrid.Canvas do
		begin
			Delete(s__Evaluation, 1, 2);
			Pen.Color := clBlack;
			Draw(Rect.Left, Rect.Top, Self.m__GreaterOrEqual.Picture.Graphic);
			TextOut(Rect.Left + 8, Rect.Top, s__Evaluation);
		end
	else
		Self.m__DrawGrid.Canvas.TextOut(Rect.Left, Rect.Top, s__Evaluation);
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
	
	Self.m__BackForwardButtons.Position := Self.m__GameContext.GetCurrentMoveNumberInHistory();
	Self.m__DrawGrid.Repaint();
end;

procedure TMainWindow.OnClick_MenuItem__new_game(Sender: TObject);
var
	Player1Type, Player2Type: Byte;
begin
	Self.m__BackForwardButtons.Position := 1;
	Self.m__DrawGrid.DefaultDrawing := False;
	
	Self.m__Statusbar.Panels[0].Text := '';
	Self.m__Statusbar.Panels[1].Text := '';
	
	Self.UpdatePlayers(Player1Type, Player2Type);
	Self.m__GameContext.StartNewGame(Player1Type, Player2Type);

	Self.m__DrawGrid.Repaint();
end;

procedure TMainWindow.OnClick_MenuItem_position_modify(Sender: TObject);
begin
	Self.m__MenuItem_position_modify.Enabled := False;
	Self.m__MenuItem_position_continue.Enabled := True;
	
	Self.m__GameContext.StartBoardModification();
end;

procedure TMainWindow.OnClick_MenuItem_position_continue(Sender: TObject);
begin
	Self.m__MenuItem_position_modify.Enabled := True;
	Self.m__MenuItem_position_continue.Enabled := False;
	Self.m__DrawGrid.Selection := TGridRect(Self.m__DrawGrid.CellRect(-1, -1));		//TODO: Why whis typecast (in two places)?
	
	Self.m__GameContext.FinishBoardModification();
	
	Self.m__BackForwardButtons.Position := Self.m__GameContext.GetCurrentMoveNumberInHistory();
	Self.m__GameContext.RunGame();
end;

procedure TMainWindow.OnClick_MenuItem__players__any_submenu(Sender: TObject);
var
	Player1TypeDummy, Player2TypeDummy: Byte;
begin
	Self.m__MenuItem__players__human_vs_cpu.Checked := False;
	Self.m__MenuItem__players__cpu_vs_cpu.Checked := False;
	Self.m__MenuItem__players__cpu_vs_human.Checked := False;
	Self.m__MenuItem__players__2_players.Checked := False;
	
	(Sender as TMenuItem).Checked := True;
	
	Self.UpdatePlayers(Player1TypeDummy, Player2TypeDummy);
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

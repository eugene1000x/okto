
program Othello;


uses
	Forms,
	Reversi in 'Reversi.pas';

{$R *.res}

begin
	Application.Initialize;
	Application.CreateForm(TMainWindow, MainWindow);
	MainWindow.OnClick_MenuItem__new_game(MainWindow.m__MenuItem__new_game);
	Application.Run;
end.

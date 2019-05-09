
program Othello;


uses
	Forms,
	Reversi in 'Reversi.pas';

{$R *.res}

begin
	Application.Initialize;
	Application.CreateForm(TForm1, Form1);
	Form1.NewClick(Form1.New);
	Application.Run;
end.

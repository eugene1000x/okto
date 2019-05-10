
program Okto;


uses
	Forms,
	Impl in 'Impl.pas';

{$R *.res}

begin
	Application.Initialize;
	Application.CreateForm(TMainWindow, MainWindow);
	Application.Run;
end.

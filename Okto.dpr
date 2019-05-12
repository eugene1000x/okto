
program Okto;


uses
	Forms,
	Impl in 'Impl.pas';

var
	MainWindowDummy: TMainWindow;

{$R *.res}

begin
	Application.Initialize;
	Application.CreateForm(TMainWindow, MainWindowDummy);
	Application.Run;
end.

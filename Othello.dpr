program Othello;

uses
  Forms,
  Reversi in 'Reversi.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Form1.Newclick(Form1.New);
  Application.Run;
end.

program FluentUIDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  DemoDialog in 'DemoDialog.pas' {Form7};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TForm7, Form7);
  Application.Run;

end.

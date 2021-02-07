unit DemoDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FluentUI.Dialog,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TForm7 = class(TForm)
    FluendDialog1: TFluendDialog;
    Button1: TButton;
    Layout1: TLayout;
    StyleBook1: TStyleBook;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}

procedure TForm7.Button1Click(Sender: TObject);
begin
  FluendDialog1.Title := 'Hello world';
  FluendDialog1.Show;
end;

end.

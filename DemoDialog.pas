unit DemoDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FluentUI.Dialog,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TForm7 = class(TForm)
    Button1: TButton;
    Layout1: TLayout;
    StyleBook1: TStyleBook;
    Button2: TButton;
    Button3: TButton;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FluendDialog1: TFluendDialog;
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}

procedure TForm7.FormCreate(Sender: TObject);
begin
  FluendDialog1 := TFluendDialog.Create(Self);
end;

procedure TForm7.Button1Click(Sender: TObject);
begin
  FluendDialog1.Title := 'Hello world';
  FluendDialog1.SubText := 'FMX = Fluent UI = <3';
  FluendDialog1.Show;
end;

end.

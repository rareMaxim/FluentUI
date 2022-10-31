﻿unit DemoDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FluentUI.Dialog,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TForm7 = class(TForm)
    Button1: TButton;
    Layout1: TLayout;
    StyleBook1: TStyleBook;
    Button2: TButton;
    Button3: TButton;
    SpeedButton1: TSpeedButton;
    Memo1: TMemo;
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
var
  lDialog: IFluentDialogUserContent;
begin
  lDialog := TFluentDialogUserContent.Create(Self);
  lDialog.Title := 'Hello world';
  lDialog.SubTitle := 'FMX + Fluent UI = <3';
  lDialog.PrimatyButtonText := ':)';
  lDialog.Content := Memo1;
  lDialog.Show;
end;

end.

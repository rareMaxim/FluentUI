unit FluentUI.Dialog;

interface

uses
  FMX.Forms,
  FMX.Objects,
  System.Classes,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Layouts;

type
{$SCOPEDENUMS ON}
  TDialogType = (Normal, LargeHeader, Close);
  TContentDialogButton = (None, Primary);
{$SCOPEDENUMS OFF}

  IFluentDialog = interface
    ['{D71BE646-52F3-4B0E-8878-9C0DB0009B49}']

  end;

  /// <summary>
  /// A dialog box (Dialog) is a temporary pop-up that takes focus from the page or
  /// app and requires people to interact with it. It’s primarily used for confirming
  /// actions, such as deleting a file, or asking people to make a choice.
  /// </summary>
  TFluendDialog = class(TComponent)
  private
    FOverlayRect: TRectangle;
    FDialogRect: TRectangle;
    FIsDark: Boolean;
    FTitleUI: TLabel;
    FSubTextUI: TLabel;
    FDialogType: TDialogType;
    FContent: TFmxObject;
    FPrimaryButton: TButton;
    FSecondaryButtonText: string;
    FCloseButtonText: string;
    FButtonsLayout: TLayout;
    function GetPrimaryButtonText: string;
    function GetSubText: String;
    function GetTitle: string;
    procedure SetContent(const Value: TFmxObject);
    procedure SetPrimaryButtonText(const Value: string);
    procedure SetSubText(const Value: String);
    procedure SetTitle(const Value: string);
  protected
    procedure OnOverlayRectClick(Sender: TObject);
    procedure UpdateParentForm;
    procedure BuildButtons;
    procedure BuildPrimaryButton(const AText: string);
    procedure BuildTitle(const AText: string);
    procedure BuildSubTitle(const AText: string);
    procedure BuildDialogRect;
  public

    procedure Show;
    procedure Close;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Title: string read GetTitle write SetTitle;
    property PrimaryButtonText: string read GetPrimaryButtonText write SetPrimaryButtonText;
    property SecondaryButtonText: string read FSecondaryButtonText write FSecondaryButtonText;
    property CloseButtonText: string read FCloseButtonText write FCloseButtonText;
    property SubText: String read GetSubText write SetSubText;
    property IsDark: Boolean read FIsDark write FIsDark;
    property DialogType: TDialogType read FDialogType write FDialogType;
    property Content: TFmxObject read FContent write SetContent;
  end;

procedure Register;

implementation

uses
  FMX.Graphics;

procedure Register;
begin
  RegisterComponents('FluentUI', [TFluendDialog]);
end;
{ TFluendDialog }

procedure TFluendDialog.BuildButtons;
begin
  FButtonsLayout := TLayout.Create(FDialogRect);
  FButtonsLayout.Parent := FDialogRect;
  FButtonsLayout.Align := TAlignLayout.Top;
  FButtonsLayout.Margins.Right := -4;
  FButtonsLayout.Height := 32;
  //
  BuildPrimaryButton('Primary');

end;

procedure TFluendDialog.BuildDialogRect;
begin
  FDialogRect := TRectangle.Create(FOverlayRect);
  FDialogRect.Fill.Color := $FFFFFFFF;
  FDialogRect.Align := TAlignLayout.Center;
  FDialogRect.Parent := FOverlayRect;
  FDialogRect.Height := 172;
  FDialogRect.Width := 288;
  FDialogRect.Sides := [];
  FDialogRect.Padding.Top := 16;
  FDialogRect.Padding.Right := 46;
  FDialogRect.Padding.Bottom := 20;
  FDialogRect.Padding.Left := 24;
  FDialogRect.XRadius := 2;
  FDialogRect.YRadius := 2;
  FDialogRect.Stroke.Kind := TBrushKind.None;
end;

procedure TFluendDialog.BuildPrimaryButton(const AText: string);
begin
  FPrimaryButton := TButton.Create(FButtonsLayout);
  FPrimaryButton.Parent := FButtonsLayout;
  FPrimaryButton.Align := TAlignLayout.Right;
  FPrimaryButton.Text := AText;
end;

procedure TFluendDialog.BuildSubTitle(const AText: string);
begin
  if not Assigned(FSubTextUI) then
    FSubTextUI := TLabel.Create(FDialogRect);
  FSubTextUI.Align := TAlignLayout.Top;
  FSubTextUI.Parent := FDialogRect;
  FSubTextUI.AutoSize := True;
  FSubTextUI.Margins.Top := 24;
  FSubTextUI.Margins.Bottom := 24;
  FSubTextUI.Text := AText;
  FSubTextUI.StyledSettings := [TStyledSetting.Style];
  FSubTextUI.TextSettings.FontColor := $FF605E5C;
  FSubTextUI.TextSettings.Font.Size := 14;
  FSubTextUI.TextSettings.Font.Family := 'Segoe UI';
end;

procedure TFluendDialog.BuildTitle(const AText: string);
begin
  if not Assigned(FTitleUI) then
    FTitleUI := TLabel.Create(FDialogRect);
  FTitleUI.Align := TAlignLayout.Top;
  FTitleUI.Parent := FDialogRect;
  FTitleUI.AutoSize := True;
  FTitleUI.Text := AText;
  FTitleUI.StyledSettings := [TStyledSetting.Style];
  FTitleUI.TextSettings.FontColor := $FF323130;
  FTitleUI.TextSettings.Font.Size := 20;
  FTitleUI.TextSettings.Font.Family := 'Segoe UI';
end;

procedure TFluendDialog.Close;
begin
  FOverlayRect.Parent := nil;
end;

constructor TFluendDialog.Create(AOwner: TComponent);
begin
  inherited;
  FDialogType := TDialogType.Normal;
  FOverlayRect := TRectangle.Create(Self);
  FOverlayRect.Align := TAlignLayout.Client;
  FOverlayRect.Sides := [];
  FOverlayRect.Fill.Color := $66000000;
  FOverlayRect.OnClick := OnOverlayRectClick;
  //

  //
  BuildDialogRect;
  //
  BuildButtons;
  //
  BuildSubTitle('Do you want to send this message without a subject?');
  //
  BuildTitle('Hello world');
  //

end;

destructor TFluendDialog.Destroy;
begin
  inherited;
end;

function TFluendDialog.GetPrimaryButtonText: string;
begin
  Result := FPrimaryButton.Text;
end;

function TFluendDialog.GetSubText: String;
begin
  Result := FSubTextUI.Text;
end;

function TFluendDialog.GetTitle: string;
begin
  Result := FTitleUI.Text;
end;

procedure TFluendDialog.OnOverlayRectClick(Sender: TObject);
begin
  Close;
end;

procedure TFluendDialog.SetContent(const Value: TFmxObject);
begin
  FContent := Value;
end;

procedure TFluendDialog.SetPrimaryButtonText(const Value: string);
begin
  FPrimaryButton.Text := Value;
end;

procedure TFluendDialog.SetSubText(const Value: String);
begin
  FSubTextUI.Text := Value;
end;

procedure TFluendDialog.SetTitle(const Value: string);
begin
  FTitleUI.Text := Value;
end;

procedure TFluendDialog.Show;
begin
  UpdateParentForm;
end;

procedure TFluendDialog.UpdateParentForm;
begin
  FOverlayRect.Parent := Screen.ActiveForm;
end;

end.

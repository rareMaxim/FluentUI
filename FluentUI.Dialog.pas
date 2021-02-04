unit FluentUI.Dialog;

interface

uses
  FMX.Forms,
  FMX.Objects,
  System.Classes,
  FMX.Types,
  FMX.StdCtrls;

type
{$SCOPEDENUMS ON}
  TDialogType = (Normal, LargeHeader, Close);
{$SCOPEDENUMS OFF}

  TFluendDialog = class(TComponent)
  private
    FOverlayRect: TRectangle;
    FDialogRect: TRectangle;
    FIsDark: Boolean;
    FTitleUI: TLabel;
    FSubTextUI: TLabel;
    FDialogType: TDialogType;
    function GetSubText: String;
    function GetTitle: string;
    procedure SetSubText(const Value: String);
    procedure SetTitle(const Value: string);
  protected
    procedure OnOverlayRectClick(Sender: TObject);
    procedure UpdateParentForm;
  public

    procedure Show;
    procedure Close;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Title: string read GetTitle write SetTitle;
    property SubText: String read GetSubText write SetSubText;
    property IsDark: Boolean read FIsDark write FIsDark;
    property DialogType: TDialogType read FDialogType write FDialogType;
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
  //
  FSubTextUI := TLabel.Create(FDialogRect);
  FSubTextUI.Align := TAlignLayout.Top;
  FSubTextUI.Parent := FDialogRect;
  FSubTextUI.AutoSize := True;
  FSubTextUI.Margins.Top := 24;
  FSubTextUI.Text := 'Do you want to send this message without a subject?';
  FSubTextUI.StyledSettings := [TStyledSetting.Style];
  FSubTextUI.TextSettings.FontColor := $FF605E5C;
  FSubTextUI.TextSettings.Font.Size := 14;
  FSubTextUI.TextSettings.Font.Family := 'Segoe UI';
  //
  FTitleUI := TLabel.Create(FDialogRect);
  FTitleUI.Align := TAlignLayout.Top;
  FTitleUI.Parent := FDialogRect;
  FTitleUI.AutoSize := True;
  FTitleUI.Text := 'Missing Subject';
  FTitleUI.StyledSettings := [TStyledSetting.Style];
  FTitleUI.TextSettings.FontColor := $FF323130;
  FTitleUI.TextSettings.Font.Size := 20;
  FTitleUI.TextSettings.Font.Family := 'Segoe UI';

end;

destructor TFluendDialog.Destroy;
begin
  inherited;
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

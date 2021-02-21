unit FluentUI.Dialog;

interface

uses
  FMX.Forms,
  FMX.Objects,
  System.Classes,
  System.SysUtils,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Controls, System.UITypes, System.Types;

type
{$SCOPEDENUMS ON}
  TDialogType = (Normal, LargeHeader, Close);
  TContentDialogButton = (None, Primary);
{$SCOPEDENUMS OFF}

  IFluentDialog = interface
    ['{D71BE646-52F3-4B0E-8878-9C0DB0009B49}']
    procedure Show;
    procedure Close;
  end;
  /// <summary>
  /// A dialog box (Dialog) is a temporary pop-up that takes focus from the page or
  /// app and requires people to interact with it. It’s primarily used for confirming
  /// actions, such as deleting a file, or asking people to make a choice.
  /// </summary>

  TFluendDialogOverlay = class(TComponent, IFluentDialog)
  private
    FOverlay: TRectangle;
    function GetOverlayColor: TAlphaColor;
    procedure SetOverlayColor(const Value: TAlphaColor);
  protected
    procedure BuildOverlay(AOverlay: TRectangle);
    procedure DoOverlayRectClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show; virtual;
    procedure Close; virtual;
    function IsVisible: Boolean; virtual;
  published
    property OverlayColor: TAlphaColor read GetOverlayColor write SetOverlayColor;
  end;

  IFluendDialogBaseContent = interface(IFluentDialog)
    ['{3622C0A6-DE74-42FA-BC32-29ECED5860C9}']
    function GetHeight: Single;
    function GetWidth: Single;
    procedure SetHeight(const Value: Single);
    procedure SetWidth(const Value: Single);
    function GetBaseContentColor: TAlphaColor;
    procedure SetBaseContentColor(const Value: TAlphaColor);
    //
    property Height: Single read GetHeight write SetHeight;
    property Width: Single read GetWidth write SetWidth;
    property BaseContentColor: TAlphaColor read GetBaseContentColor write SetBaseContentColor;

  end;

  TFluendDialogBaseContent = class(TFluendDialogOverlay, IFluentDialog)
  private
    FBaseContent: TRectangle;
    function GetBaseContentColor: TAlphaColor;
    procedure SetBaseContentColor(const Value: TAlphaColor);
    function GetHeight: Single;
    procedure SetHeight(const Value: Single);
    function GetWidth: Single;
    procedure SetWidth(const Value: Single);
  protected
    procedure BuildBaseContent(ABaseContent: TRectangle);
  public
    procedure Show; override;
    procedure Close; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsVisible: Boolean; override;
  published
    property BaseContentColor: TAlphaColor read GetBaseContentColor write SetBaseContentColor;
    property Height: Single read GetHeight write SetHeight;
    property Width: Single read GetWidth write SetWidth;
  end;

  IFluentDialogDefault = interface(IFluendDialogBaseContent)
    ['{8CEB1177-C35E-415F-8E5E-5A7D2F2EE9E7}']
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetSubTitle: string;
    procedure SetSubTitle(const Value: string);
    function GetPrimatyButtonText: string;
    procedure SetPrimatyButtonText(const Value: string);
    function GetDefaultButtonText: string;
    procedure SetDefaultButtonText(const Value: string);
    //
    property Title: string read GetTitle write SetTitle;
    property SubTitle: string read GetSubTitle write SetSubTitle;
    property PrimatyButtonText: string read GetPrimatyButtonText write SetPrimatyButtonText;
    property DefaultButtonText: string read GetDefaultButtonText write SetDefaultButtonText;

  end;

  TFluentDialogDefault = class(TFluendDialogBaseContent, IFluentDialogDefault)
  private
    FTitle: TText;
    FSubTitle: TText;
    FPrimaryButton, FDefaultButton: TButton;
    FHeaderContent, FBodyContent, FFooterContent: TLayout;
    procedure ShowTitleControl(ATitleControl: TText);
    procedure ShowSubTitleControl(ASubTitleControl: TText);
    procedure ShowButtons(APrimary, ADefault: TButton);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    procedure BuildHeader;
    procedure BuildBody;
    procedure BuildFooter;
    function GetDefaultButtonText: string;
    function GetPrimatyButtonText: string;
    function GetSubTitle: string;
    procedure SetDefaultButtonText(const Value: string);
    procedure SetPrimatyButtonText(const Value: string);
    procedure SetSubTitle(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Show; override;
    procedure Close; override;
    function IsVisible: Boolean; override;
  published
    property Title: string read GetTitle write SetTitle;
    property SubTitle: string read GetSubTitle write SetSubTitle;
    property PrimatyButtonText: string read GetPrimatyButtonText write SetPrimatyButtonText;
    property DefaultButtonText: string read GetDefaultButtonText write SetDefaultButtonText;
  end;

implementation

uses
  FMX.Graphics,
  FMX.TextLayout,
  FluentUI.ColorPalettes;

{ TFluendDialogOverlay }

procedure TFluendDialogOverlay.BuildOverlay(AOverlay: TRectangle);
begin
 // AOverlay.BeginUpdate;
  try
    AOverlay.Parent := Screen.ActiveForm;
    FOverlay.Align := TAlignLayout.Client;
    FOverlay.Sides := [];
    FOverlay.OnClick := DoOverlayRectClick;
    FOverlay.Visible := False;
  finally
 // AOverlay.EndUpdate;
  end;
end;

procedure TFluendDialogOverlay.Close;
begin
  FOverlay.Visible := False;
end;

constructor TFluendDialogOverlay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOverlay := TRectangle.Create(Self);
  OverlayColor := $66000000;
end;

destructor TFluendDialogOverlay.Destroy;
begin
  Close;
 // FOverlay.Free; -->    FOverlay := TRectangle.Create(self);
  inherited Destroy;
end;

procedure TFluendDialogOverlay.DoOverlayRectClick(Sender: TObject);
begin
  Close;
end;

function TFluendDialogOverlay.GetOverlayColor: TAlphaColor;
begin
  Result := FOverlay.Fill.Color;
end;

function TFluendDialogOverlay.IsVisible: Boolean;
begin
  Result := FOverlay.Visible;
end;

procedure TFluendDialogOverlay.SetOverlayColor(const Value: TAlphaColor);
begin
  FOverlay.Fill.Color := Value;
end;

procedure TFluendDialogOverlay.Show;
begin
  BuildOverlay(FOverlay);
  FOverlay.Visible := True;
end;

{ TFluendDialogBaseContent }

procedure TFluendDialogBaseContent.BuildBaseContent(ABaseContent: TRectangle);
begin
// ABaseContent.BeginUpdate;
  try
    ABaseContent.Parent := FOverlay;
    ABaseContent.Align := TAlignLayout.Center;
    ABaseContent.Sides := [];
    ABaseContent.XRadius := 2;
    ABaseContent.YRadius := 2;
    ABaseContent.Stroke.Kind := TBrushKind.None;
  finally
  // ABaseContent.EndUpdate;
  end;
end;

procedure TFluendDialogBaseContent.Close;
begin
  FBaseContent.Visible := False;
  inherited Close;
end;

constructor TFluendDialogBaseContent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBaseContent := TRectangle.Create(Self);
  BaseContentColor := $FFFFFFFF;
  Height := 186;
  Width := 340;
end;

destructor TFluendDialogBaseContent.Destroy;
begin
  Close;
 // FBaseContent.Free; --> FBaseContent := TRectangle.Create(Self);
  inherited;
end;

function TFluendDialogBaseContent.GetBaseContentColor: TAlphaColor;
begin
  Result := FBaseContent.Fill.Color;
end;

function TFluendDialogBaseContent.GetHeight: Single;
begin
  Result := FBaseContent.Height;
end;

function TFluendDialogBaseContent.GetWidth: Single;
begin
  Result := FBaseContent.Width;
end;

function TFluendDialogBaseContent.IsVisible: Boolean;
begin
  Result := (inherited IsVisible) and (FBaseContent.IsVisible);
end;

procedure TFluendDialogBaseContent.SetBaseContentColor(const Value: TAlphaColor);
begin
  FBaseContent.Fill.Color := Value;
end;

procedure TFluendDialogBaseContent.SetHeight(const Value: Single);
begin
{ https://developer.microsoft.com/en-us/fluentui#/controls/web/dialog }
  if (Value >= 172) and (Value <= 340) then
    FBaseContent.Height := Value;
end;

procedure TFluendDialogBaseContent.SetWidth(const Value: Single);
begin
{ https://developer.microsoft.com/en-us/fluentui#/controls/web/dialog }
  if (Value >= 288) and (Value <= 340) then
    FBaseContent.Width := Value;
end;

procedure TFluendDialogBaseContent.Show;
begin
  inherited Show;
  BuildBaseContent(FBaseContent);
  FBaseContent.Visible := True;
end;

{ TFluentDialogDefault }

procedure TFluentDialogDefault.BuildBody;
begin
  FBodyContent := TLayout.Create(FBaseContent);
  FBodyContent.Parent := FBaseContent;
  FBodyContent.Align := TAlignLayout.Client;
  FBodyContent.Margins.Left := 24;
  FBodyContent.Margins.Right := 24;

  FSubTitle := TText.Create(Self);
end;

procedure TFluentDialogDefault.ShowButtons(APrimary, ADefault: TButton);
begin
// APrimary.BeginUpdate;
// ADefault.BeginUpdate;
  try
    APrimary.StyleLookup := 'FluentButtonPrimary';
    APrimary.Visible := not APrimary.Text.IsEmpty;
    APrimary.Parent := FFooterContent;
    APrimary.Align := TAlignLayout.Right;
    APrimary.Margins.Left := 4;
    //
    ADefault.StyleLookup := '';
    ADefault.Visible := not ADefault.Text.IsEmpty;
    ADefault.Parent := FFooterContent;
    ADefault.Align := TAlignLayout.Right;
    ADefault.Margins.Left := 4;
  finally
 // ADefault.EndUpdate;
 // APrimary.EndUpdate;
  end;
end;

procedure TFluentDialogDefault.BuildFooter;
begin
  FFooterContent := TLayout.Create(FBaseContent);
  FFooterContent.Height := 33;
  FFooterContent.Parent := FBaseContent;
  FFooterContent.Align := TAlignLayout.Bottom;
  FFooterContent.Margins.Left := 24;
  FFooterContent.Margins.Bottom := 24;
  FFooterContent.Margins.Right := 16;

  FPrimaryButton := TButton.Create(FFooterContent);
  FDefaultButton := TButton.Create(FFooterContent);
end;

procedure TFluentDialogDefault.BuildHeader;
begin
  FHeaderContent := TLayout.Create(FBaseContent);
  FHeaderContent.Parent := FBaseContent;
  FHeaderContent.Align := TAlignLayout.Top;

  FHeaderContent.Margins.Top := 16;
  FHeaderContent.Margins.Right := 46;
  FHeaderContent.Margins.Bottom := 20;
  FHeaderContent.Margins.Left := 24;

  FTitle := TText.Create(FHeaderContent);
  Title := 'TFluentUI Title';
end;

procedure TFluentDialogDefault.ShowSubTitleControl(ASubTitleControl: TText);
begin
// ASubTitleControl.BeginUpdate;
  try
    ASubTitleControl.Parent := FBodyContent;
    ASubTitleControl.Align := TAlignLayout.Client;
    ASubTitleControl.Width := FBodyContent.Width;
    ASubTitleControl.Color := TAlphaColorRec.Black;
    ASubTitleControl.TextSettings.FontColor := TfluentNeutralColors.neutralSecondary;
    ASubTitleControl.TextSettings.Font.Size := 14;
    ASubTitleControl.TextSettings.Font.Family := 'Segoe UI';
    ASubTitleControl.HorzTextAlign := TTextAlign.Leading;
    ASubTitleControl.AutoSize := True;
    FBodyContent.Height := ASubTitleControl.Height;
    FSubTitle.Visible := True;
  finally
// ASubTitleControl.EndUpdate;
  end;
end;

procedure TFluentDialogDefault.ShowTitleControl(ATitleControl: TText);
begin
// ATitleControl.BeginUpdate;
  try
    ATitleControl.Parent := FHeaderContent;
    ATitleControl.Align := TAlignLayout.Client;
    ATitleControl.Width := FHeaderContent.Width;
    ATitleControl.Color := TAlphaColorRec.Black;
    ATitleControl.TextSettings.FontColor := TfluentNeutralColors.neutralPrimary;
    ATitleControl.TextSettings.Font.Size := 20;
    ATitleControl.TextSettings.Font.Family := 'Segoe UI';
    ATitleControl.HorzTextAlign := TTextAlign.Leading;
    ATitleControl.AutoSize := True;
    FHeaderContent.Height := ATitleControl.Height;
    FTitle.Visible := True;
  finally
 // ATitleControl.EndUpdate;
  end;
end;

procedure TFluentDialogDefault.Close;
begin
  FSubTitle.Visible := False;
  FTitle.Visible := False;
  FPrimaryButton.Visible := False;
  FDefaultButton.Visible := False;
  inherited Close;
end;

constructor TFluentDialogDefault.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BuildHeader;
  BuildBody;
  BuildFooter;

end;

function TFluentDialogDefault.GetDefaultButtonText: string;
begin
  Result := FDefaultButton.Text;
end;

function TFluentDialogDefault.GetPrimatyButtonText: string;
begin
  Result := FPrimaryButton.Text;
end;

function TFluentDialogDefault.GetSubTitle: string;
begin
  Result := FSubTitle.Text;
end;

function TFluentDialogDefault.GetTitle: string;
begin
  Result := FTitle.Text;
end;

function TFluentDialogDefault.IsVisible: Boolean;
begin
  Result := inherited and FTitle.Visible;
end;

procedure TFluentDialogDefault.SetDefaultButtonText(const Value: string);
begin
  FDefaultButton.Text := Value;
end;

procedure TFluentDialogDefault.SetPrimatyButtonText(const Value: string);
begin
  FPrimaryButton.Text := Value;
end;

procedure TFluentDialogDefault.SetSubTitle(const Value: string);
begin
  FSubTitle.Text := Value;
end;

procedure TFluentDialogDefault.SetTitle(const Value: string);
begin
  FTitle.Text := Value;
end;

procedure TFluentDialogDefault.Show;
begin
  inherited Show;
  ShowTitleControl(FTitle);
  ShowSubTitleControl(FSubTitle);
  ShowButtons(FPrimaryButton, FDefaultButton);

end;

end.

unit FluentUI.Dialog;

interface

{$DEFINE DIALOG_DEBUG_LAYERS}

uses
  FMX.Forms,
  FMX.Objects,
  System.Classes,
  System.SysUtils,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Controls,
  System.UITypes,
  System.Types;

type
{$SCOPEDENUMS ON}
  TDialogType = (Normal, LargeHeader, Close);
  TContentDialogButton = (None, Primary);
{$SCOPEDENUMS OFF}

  IFluentDialog = interface
    ['{D71BE646-52F3-4B0E-8878-9C0DB0009B49}']
    function GetOverlayColor: TAlphaColor;
    procedure SetOverlayColor(const Value: TAlphaColor);
    //
    procedure Show;
    procedure Close;
    property OverlayColor: TAlphaColor read GetOverlayColor write SetOverlayColor;
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
    function BuildOverlay: TRectangle;
    procedure DoOverlayRectClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
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
    function BuildBaseContent(AOwner: TComponent): TRectangle;
  public
    procedure Show; override;
    procedure Close; override;
    constructor Create(AOwner: TComponent); override;
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
    procedure Show(AOnButtonClickCallBack: TProc<string>); overload;
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
    FHeaderContent, FBodyContent, FFooterContent: {$IF Define(DIALOG_DEBUG_LAYERS)}TRectangle{$ELSE}TLayout{$ENDIF};
    FOnButtonClickCallBack: TProc<string>;
    function BuildTitleControl(AOwner: TComponent): TText;
    function BuildSubTitleControl(AOwner: TComponent): TText;
    procedure BuildButtons(out APrimary, ADefault: TButton);
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
    procedure DoButtonClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Show; overload; override;
    procedure Show(AOnButtonClickCallBack: TProc<string>); reintroduce; overload;
    procedure Close; override;
    function IsVisible: Boolean; override;
  published
    property Title: string read GetTitle write SetTitle;
    property SubTitle: string read GetSubTitle write SetSubTitle;
    property PrimatyButtonText: string read GetPrimatyButtonText write SetPrimatyButtonText;
    property DefaultButtonText: string read GetDefaultButtonText write SetDefaultButtonText;
  end;

  IFluentDialogUserContent = interface(IFluentDialogDefault)
    ['{99A08205-F197-4E84-978F-347365C7FD81}']
    procedure SetContent(const Value: TControl);
    function GetContent: TControl;
    //
    property Content: TControl read GetContent write SetContent;
  end;

  TFluentDialogUserContent = class(TFluentDialogDefault, IFluentDialogUserContent)
  private
    FContent: TControl;
    procedure SetContent(const Value: TControl);
    function GetContent: TControl;
  public
    procedure Show; override;
  published
    property Content: TControl read GetContent write SetContent;
  end;

implementation

uses
  FMX.Graphics,
  FMX.TextLayout,
  FluentUI.ColorPalettes,
  FluentUI.Core.TextTools;

{ TFluendDialogOverlay }

function TFluendDialogOverlay.BuildOverlay: TRectangle;
begin
  Result := TRectangle.Create(Self);
  Result.BeginUpdate;
  try
    Result.Parent := Screen.ActiveForm;
    Result.Align := TAlignLayout.Client;
    Result.Sides := [];
    Result.OnClick := DoOverlayRectClick;
    Result.Visible := False;
  finally
    Result.EndUpdate;
  end;
end;

procedure TFluendDialogOverlay.Close;
begin
  FOverlay.Visible := False;
end;

constructor TFluendDialogOverlay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOverlay := BuildOverlay;
  OverlayColor := $66000000;
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
  FOverlay.Visible := True;
end;

{ TFluendDialogBaseContent }

function TFluendDialogBaseContent.BuildBaseContent(AOwner: TComponent): TRectangle;
begin
  Result := TRectangle.Create(AOwner);
  Result.BeginUpdate;
  try
    Result.Parent := FOverlay;
    Result.Align := TAlignLayout.Center;
    Result.Sides := [];
    Result.XRadius := 2;
    Result.YRadius := 2;
    Result.Stroke.Kind := TBrushKind.None;
    Result.Visible := False;
  finally
    Result.EndUpdate;
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
  FBaseContent := BuildBaseContent(Self);
  BaseContentColor := $FFFFFFFF;
  Height := 186;
  Width := 340;
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
  FBaseContent.Visible := True;
end;

{ TFluentDialogDefault }

procedure TFluentDialogDefault.BuildBody;
begin
  FBodyContent := {$IF Define(DIALOG_DEBUG_LAYERS)}TRectangle{$ELSE}TLayout{$ENDIF}.Create(FBaseContent);
  FBodyContent.Parent := FBaseContent;
  FBodyContent.Align := TAlignLayout.Client;
  FBodyContent.Margins.Left := 24;
  FBodyContent.Margins.Right := 24;

  FSubTitle := BuildSubTitleControl(FSubTitle);
end;

procedure TFluentDialogDefault.Show(AOnButtonClickCallBack: TProc<string>);
begin
  FOnButtonClickCallBack := AOnButtonClickCallBack;
  Show;
end;

procedure TFluentDialogDefault.BuildButtons(out APrimary, ADefault: TButton);
begin
  APrimary := TButton.Create(FFooterContent);
  ADefault := TButton.Create(FFooterContent);
  APrimary.BeginUpdate;
  ADefault.BeginUpdate;
  try
    APrimary.StyleLookup := 'FluentButtonPrimary';
    APrimary.Visible := not APrimary.Text.IsEmpty;
    APrimary.Parent := FFooterContent;
    APrimary.Align := TAlignLayout.Right;
    APrimary.Margins.Left := 4;
    APrimary.Name := 'btnPrimary';
    APrimary.OnClick := DoButtonClicked;
    //
    ADefault.StyleLookup := '';
    ADefault.Visible := not ADefault.Text.IsEmpty;
    ADefault.Parent := FFooterContent;
    ADefault.Align := TAlignLayout.Right;
    ADefault.Margins.Left := 4;
    ADefault.Name := 'btnDefault';
    ADefault.OnClick := DoButtonClicked;
  finally
    ADefault.EndUpdate;
    APrimary.EndUpdate;
  end;
end;

procedure TFluentDialogDefault.BuildFooter;
begin
  FFooterContent := {$IF Define(DIALOG_DEBUG_LAYERS)}TRectangle{$ELSE}TLayout{$ENDIF}.Create(FBaseContent);
  FFooterContent.Height := 33;
  FFooterContent.Parent := FBaseContent;
  FFooterContent.Align := TAlignLayout.Bottom;
  FFooterContent.Margins.Left := 24;
  FFooterContent.Margins.Bottom := 24;
  FFooterContent.Margins.Right := 16;
  FFooterContent.Margins.Top := 16;
  BuildButtons(FPrimaryButton, FDefaultButton);
end;

procedure TFluentDialogDefault.BuildHeader;
begin
  FHeaderContent := {$IF Define(DIALOG_DEBUG_LAYERS)}TRectangle{$ELSE}TLayout{$ENDIF}.Create(FBaseContent);
  FHeaderContent.Parent := FBaseContent;
  FHeaderContent.Align := TAlignLayout.Top;

  FHeaderContent.Margins.Top := 16;
  FHeaderContent.Margins.Right := 46;
  FHeaderContent.Margins.Bottom := 20;
  FHeaderContent.Margins.Left := 24;

  FTitle := BuildTitleControl(FHeaderContent);
end;

function TFluentDialogDefault.BuildSubTitleControl(AOwner: TComponent): TText;
begin
  Result := TText.Create(AOwner);

  Result.BeginUpdate;
  try
    Result.Parent := FBodyContent;
    Result.Margins.Bottom := 24;
    Result.Align := TAlignLayout.Top;
    Result.Width := FBodyContent.Width;
    Result.Color := TAlphaColorRec.Black;
    Result.TextSettings.FontColor := TfluentNeutralColors.neutralSecondary;
    Result.TextSettings.Font.Size := 14;
    Result.TextSettings.Font.Family := 'Segoe UI';
    Result.HorzTextAlign := TTextAlign.Leading;
    Result.AutoSize := True;
    FBodyContent.Height := Result.Height;
    Result.Visible := False;
  finally
    Result.EndUpdate;
  end;
end;

function TFluentDialogDefault.BuildTitleControl(AOwner: TComponent): TText;
begin
  Result := TText.Create(AOwner);
  Result.BeginUpdate;
  try
    Result.TextSettings.FontColor := TfluentNeutralColors.neutralPrimary;
    Result.TextSettings.Font.Size := 20;
    Result.TextSettings.Font.Family := 'Segoe UI';
    Result.Parent := FHeaderContent;
    Result.Align := TAlignLayout.Client;
    Result.Color := TAlphaColorRec.Black;
    Result.HorzTextAlign := TTextAlign.Leading;
    Result.Visible := False;
  finally
    Result.EndUpdate;
  end;
  FHeaderContent.Height := TTextTools.CalcTextSize(Result).Height;
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
  Title := 'TFluentUI Title';
end;

procedure TFluentDialogDefault.DoButtonClicked(Sender: TObject);
begin
  if Assigned(FOnButtonClickCallBack) then
    FOnButtonClickCallBack((Sender as TButton).Text);
  Close;
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
  FTitle.Visible := True;
  FSubTitle.Visible := True;
  FPrimaryButton.Visible := True;
  FDefaultButton.Visible := True;
end;

function TFluentDialogUserContent.GetContent: TControl;
begin
  Result := FContent;
end;

procedure TFluentDialogUserContent.SetContent(const Value: TControl);
begin
  FContent := Value;
  FContent.Parent := FBodyContent;
  FContent.Align := TAlignLayout.Top;
end;

{ TFluentDialogUserContent }

procedure TFluentDialogUserContent.Show;
begin
  inherited Show;
  FContent.Visible := True;
end;

end.

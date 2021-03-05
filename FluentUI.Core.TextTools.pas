unit FluentUI.Core.TextTools;

interface

uses
  FMX.Graphics,
  System.Types, FMX.Objects;

type
  TTextTools = class
  public
    class function CalcTextSize(const Text: string; Font: TFont; const Size: Single = 0): TSizeF; overload;
    class function CalcTextSize(Text: TText): TSizeF; overload;
  end;

implementation

uses
  FMX.TextLayout,
  System.Math,
  FMX.Types;

{ TTextTools }

class function TTextTools.CalcTextSize(Text: TText): TSizeF;
begin
  Result := CalcTextSize(Text.Text, Text.Font, 0);
end;

class function TTextTools.CalcTextSize(const Text: string; Font: TFont; const Size: Single = 0): TSizeF;
var
  TextLayout: TTextLayout;
begin
  TextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    TextLayout.BeginUpdate;
    try
      TextLayout.Text := Text;
      TextLayout.MaxSize := TPointF.Create(9999, 9999);
      TextLayout.Font.Assign(Font);
      if not SameValue(0, Size) then
      begin
        TextLayout.Font.Size := Size;
      end;
      TextLayout.WordWrap := False;
      TextLayout.Trimming := TTextTrimming.None;
      TextLayout.HorizontalAlign := TTextAlign.Leading;
      TextLayout.VerticalAlign := TTextAlign.Leading;
    finally
      TextLayout.EndUpdate;
    end;

    Result.Width := TextLayout.Width;
    Result.Height := TextLayout.Height;
  finally
    TextLayout.Free;
  end;
end;

end.

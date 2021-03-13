unit FluentUI.Helpers.TControl;

interface

uses
  System.Types,
  FMX.Controls,
  FMX.Edit;

type
  TfluentContrlolHelper = class helper for TControl
  public
    function ChildrenRect: TRectF;
    function RecomendHeightForParent: Single;
    function RecomendHeight: Single;
  end;

  TfluentTEditHelper = class helper for TEdit
  public
    function TextOrPromt: string;
  end;

implementation

uses
  System.SysUtils;
{ TfluentContrlolHelper }

function TfluentContrlolHelper.ChildrenRect: TRectF;
var
  I: Integer;
  Control: TControl;
begin
  Result := TRectF.Empty;
  { children }
  if not(ClipChildren or SmallSizeControl) and (Self <> nil) then
    for I := GetFirstVisibleObjectIndex to GetLastVisibleObjectIndex - 1 do
    begin
      Control := Self.Controls[I];
      if Control.Visible then
        Result := UnionRect(Result, Control.BoundsRect);
    end
end;

function TfluentContrlolHelper.RecomendHeight: Single;
begin
  Result := ChildrenRect.Height + Padding.Top + Padding.Bottom;
end;

function TfluentContrlolHelper.RecomendHeightForParent: Single;
begin
  Result := ChildrenRect.Height + ParentControl.Padding.Top + ParentControl.Padding.Bottom;
end;

{ TfluentTEditHelper }

function TfluentTEditHelper.TextOrPromt: string;
begin
  if not Text.IsEmpty then
    Result := Text
  else
    Result := TextPrompt;
end;

end.

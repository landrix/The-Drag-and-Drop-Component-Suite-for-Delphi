unit demo;

interface

{$include dragdrop.inc} // Disables .NET warnings

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TFormDemo = class(TForm)
    Panel1: TPanel;
    ButtonText: TBitBtn;
    ButtonExit: TBitBtn;
    ButtonFile: TBitBtn;
    Panel2: TPanel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Panel3: TPanel;
    ButtonURL: TBitBtn;
    procedure ButtonTextClick(Sender: TObject);
    procedure ButtonFileClick(Sender: TObject);
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonURLClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDemo: TFormDemo;

implementation

{$R *.DFM}

uses
  DropText,
  DropFile,
  DropURL,
  ShellApi;

procedure TFormDemo.ButtonTextClick(Sender: TObject);
begin
  with TFormText.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TFormDemo.ButtonFileClick(Sender: TObject);
begin
  with TFormFile.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TFormDemo.ButtonURLClick(Sender: TObject);
begin
  with TFormURL.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TFormDemo.ButtonExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormDemo.Label1Click(Sender: TObject);
begin
  Screen.Cursor := crAppStart;
  try
    Application.ProcessMessages; {otherwise cursor change will be missed}
    ShellExecute(0, nil, PChar(TLabel(Sender).Caption), nil, nil, SW_NORMAL);
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.

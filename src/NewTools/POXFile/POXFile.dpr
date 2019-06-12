program POXFile;

uses
  System.StartUpCopy,
  FMX.Forms,
  mainPOXFile in 'mainPOXFile.pas' {frmMain};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
